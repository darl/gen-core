package com.github.darl

import scala.tools.reflect.ToolBox
import scala.collection.mutable.ListBuffer

package object translate {

  import reflect.runtime.{universe => u}
  import u._

  class TranslateException(msg: String, position: Position) extends RuntimeException(msg + " at " + position)

  def translate[U <: u.type](tree: Tree)(implicit tb: ToolBox[U]): Tree = {
    import utils._

    /** extracting all `Ident`s from syntax tree if identifier defined in scope and it is Cell */
    def extractCells(scope: Scope, tree: Tree): (Tree, List[TermName]) = {
      val identifiers = ListBuffer.empty[TermName]
      val newTree = new Transformer {
        override def transform(tree: Tree) = tree match {
          case `u`.Bind(_, _) =>
            //ignoring identifiers inside Bind nodes
            tree
          case Ident(name) if name.isTermName && scope.local.contains(name.toTermName) && scope.local(name.toTermName).isCell =>
            identifiers += name.toTermName
            Apply(tree, List.empty)
          case _ =>
            super.transform(tree)
        }
      }.transform(tree)
      newTree -> identifiers.distinct.toList
    }

    /** typeChecking syntax tree and extracting all `Bind`s from it */
    def extractBinds(scope: Scope, tree: Tree): List[Bind] = {
      val res = ListBuffer.empty[Bind]
      new Traverser {
        override def traverse(tree: Tree) = tree match {
          case `u`.Bind(name, body) =>
            val b = Bind(name.toTermName.asInstanceOf[TermName], TypeTree(tree.tpe), isCell = false)
            res += b
            super.traverse(body.asInstanceOf[Tree])
          case _ =>
            super.traverse(tree)
        }
      }.traverse(typeCheck(scope, tree))
      res.toList
    }

    def translateIf(scope: Scope, tree: If): Tree = tree match {
      case If(_, _, Literal(Constant(()))) =>
        throw new TranslateException("`if` statement without `else` branch not supported", tree.pos)
      case If(cond, then, alternative) =>
        val (rewrittenCond, cells) = extractCells(scope, cond)

        val rewrittenIf = If(rewrittenCond, translate(scope.enter, then), translate(scope.enter, alternative))

        cellFlatAppTree(rewrittenIf, tree, cells)
    }

    def translateMatch(scope: Scope, tree: Match): Tree = {
      def translateCase(scope: Scope, selector: Tree, tree: CaseDef): (CaseDef, List[TermName]) = tree match {
        case CaseDef(pattern, guard, body) =>
          val (rewrittenPattern, patternCells) = extractCells(scope, pattern)

          //extracting binds from `<empty> match {case <pattern> if <guard> => <empty>}` like syntax tree
          val oneCaseMatch = Match(EmptyTree, List(CaseDef(pattern, guard, EmptyTree)))
          val binds = extractBinds(scope, oneCaseMatch)

          //new scope with extracted binds
          val newScope = binds.foldLeft(scope)(_ + _)

          val (rewrittenGuard, guardCells) = extractCells(newScope, guard)

          val rewrittenBody = translate(newScope, body)
          (
            CaseDef(rewrittenPattern, rewrittenGuard, rewrittenBody),
            (patternCells ++ guardCells).distinct
          )
      }

      tree match {
        case Match(EmptyTree, cases) =>
          // used as argument of type `PartialFunction`
          // currently cannot be converted to Cell
          val noCells = cases.forall { cse =>
            val (_, cells) = extractCells(scope.enter, cse)
            cells.isEmpty
          }
          if (noCells)
            Match(EmptyTree, cases)
          else
            throw new TranslateException("Cannot convert anonymous partial function", tree.pos)
        case Match(selector, cases) =>
          val tmpName = freshTermName("match_selector")

          val rewrittenSelector = ValDef(NoMods, tmpName, TypeTree(NoType), translate(scope.enter, selector))
          val (rewrittenCases, deps) = cases.map(cse => translateCase(scope.enter, selector, cse)).unzip
          val rewrittenMatch = Match(Apply(Ident(tmpName), List()), rewrittenCases)

          Block(
            List(rewrittenSelector),
            cellFlatAppTree(rewrittenMatch, tree, tmpName :: deps.flatten)
          )
      }
    }

    def translateBlock(scope: Scope, tree: Block): Tree = tree match {
      case Block(stats, expr) =>
        def translateStats(scope: Scope, stats: List[Tree]): (Scope, List[Tree]) = {
          val newStats = ListBuffer.empty[Tree]
          val newScope = stats.foldLeft(scope) {
            case (s, stat) =>
              val (newScope, newTree) = translateStat(s, stat)
              newStats += newTree
              newScope
          }
          newScope -> newStats.toList
        }

        def translateStat(scope: Scope, tree: Tree): (Scope, Tree) = tree match {
          case ValDef(mods, _, _, _) if mods.hasFlag(Flag.MUTABLE) =>
            throw new TranslateException("`var` statements not allowed", tree.pos)
          case ValDef(mods, name, tpt, rhs) if mods.toString.contains("<paramaccessor>") => //constructor argument. dirty hack
            val tpe = typeCheck(scope, rhs).tpe
            val bind = Bind(name, TypeTree(tpe), isCell = false)
            scope + bind -> tree
          case ValDef(mods, name, tpt, rhs) =>
            val tpe = typeCheck(scope, rhs).tpe
            val bind = Bind(name, TypeTree(tpe), isCell = true)
            (scope + bind) -> ValDef(mods, name, wrapTypeTree(tpt), translate(scope.enter + bind, rhs))
          case DefDef(mods, _, _, _, tpe, rhs) =>
            scope.stat(tree) -> tree
          case ClassDef(mods, name, tparams, Template(parents, self, trees)) =>
            scope.stat(tree) -> ClassDef(mods, name, tparams, Template(parents, self, trees))
          case imp @ Import(_, _) => scope.stat(imp) -> tree
          case Apply(Ident(name), List(arg)) if name.decoded == "validate" =>
            val (rewrittenArg, cells) = extractCells(scope, arg)
            scope -> cellValidate(rewrittenArg, cells)
          case _ =>
            scope.stat(tree) -> translate(scope, tree)
        }

        val (newScope, newStats) = translateStats(scope.enter, stats)

        Block(
          newStats,
          translate(newScope, expr)
        )
    }

    def translateNew(scope: Scope, tree: New): Tree = tree match {
      case New(t) =>
        val (rewrittenNew, cells) = extractCells(scope, t)
        cellAppTree(New(rewrittenNew), tree, cells)
    }

    def translateApply(scope: Scope, tree: Tree): Tree = {
      def rewriteFun(scope: Scope, tree: Tree): (Tree, List[ValDef]) = {
        tree match {
          case Select(Literal(_), _) => tree -> Nil
          case Literal(_) => tree -> Nil
          case Select(from, name) =>
            val tmpName = freshTermName("fun_select")
            val typedTree = typeCheck(scope, from)

            if (typedTree != EmptyTree)
              Select(Apply(Ident(tmpName), List()), name) -> List(ValDef(NoMods, tmpName, TypeTree(NoType), translate(scope.enter, from)))
            else
              Select(from, name) -> Nil
          case from =>
            tree -> Nil
        }
      }

      def rewriteArg(scope: Scope, arg: Tree): (Tree, List[ValDef]) = arg match {
        case Literal(_) => arg -> Nil
        case Function(_, _) => arg -> Nil
        case _ =>
          val tmp = freshTermName("apply_arg")
          Apply(Ident(tmp), Nil) -> List(ValDef(NoMods, tmp, TypeTree(NoType), translate(scope.enter, arg)))
      }

      def rewriteArgs(scope: Scope, args: List[Tree]): (List[Tree], List[List[ValDef]]) = {
        args.map {
          case AssignOrNamedArg(name, arg) =>
            val (newArg, defs) = rewriteArg(scope, arg)
            (AssignOrNamedArg(name, newArg): Tree) -> defs
          case arg =>
            rewriteArg(scope, arg)
        }.unzip
      }

      tree match {
        case Select(from, name) =>
          val (rewrittenSelect, valDefs) = rewriteFun(scope, tree)
          val cells = valDefs.map(_.name)

          Block(
            valDefs,
            if (isCellTree(scope, tree))
              cellFlatAppTree(rewrittenSelect, tree, cells)
            else
              cellAppTree(rewrittenSelect, tree, cells)
          )
        case Apply(fun, args) =>
          val (rewrittenFun, valDefFun) = rewriteFun(scope, fun)
          val (rewrittenArgs, valDefArgs) = rewriteArgs(scope, args)

          val valDefs = valDefFun ::: valDefArgs.flatten
          val cells = valDefs.map(_.name)

          Block(
            valDefs,
            if (isCellTree(scope, tree))
              cellFlatAppTree(Apply(rewrittenFun, rewrittenArgs), tree, cells)
            else
              cellAppTree(Apply(rewrittenFun, rewrittenArgs), tree, cells)
          )
      }
    }

    object Pure {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case Apply(Ident(name), List(arg)) if name.decoded == "pure" => Some(arg)
        case _ => None
      }
    }

    /** wraps tree in Cell */
    def translate(scope: Scope, tree: Tree): Tree = tree match {
      case t if isCellTree(scope, t) => t
      case Pure(arg) => cellAppTree(arg, arg, Nil)    // pure { <arg> } => Cell.app(<arg>)()
      case ifTree: If => translateIf(scope.enter, ifTree)
      case matchTree: Match => translateMatch(scope.enter, matchTree)
      case blockTree: Block => translateBlock(scope.enter, blockTree)
      case applyTree: Apply => translateApply(scope.enter, applyTree)
      case selectTree: Select => translateApply(scope.enter, selectTree)
      case newTree: New => translateNew(scope.enter, newTree)
      case literal: Literal => cellConstTree(literal)
      case i @ Ident(name) if name.isTermName && !scope.local.contains(name.toTermName) => cellAppTree(i, i, Nil)
      case i @ Ident(name) if name.isTermName && scope.local(name.toTermName).isCell => i
      case i @ Ident(name) if name.isTermName => cellAppTree(i, i, Nil)
      case EmptyTree => EmptyTree
      case Annotated(annotation, t) => Annotated(annotation, translate(scope, t))
      case _ => throw new TranslateException(s"Unsupported tree: ${showRaw(tree)}", tree.pos)
    }


    Block(
      List(
        Import(
          Select(Select(Select(Ident(newTermName("com")), newTermName("github")), newTermName("darl")), newTermName("flow")),
          List(ImportSelector(nme.WILDCARD, 45, null, -1))
        )
      ),
      translate(Scope.empty, tree)
    )
  }
}
