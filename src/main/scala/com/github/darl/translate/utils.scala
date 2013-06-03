package com.github.darl.translate

import com.github.darl.flow.Cell
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => u}
import scala.tools.reflect.ToolBox
import u._

object utils {

  case class Bind(name: TermName, tpt: TypeTree, isCell: Boolean) {
    def placeholder: Tree = ValDef(Modifiers(), name, tpt, Ident(newTermName("???").encodedName))   // val $name: $tpt = ???
  }
  type Frame = List[Bind]
  type Statements = List[Tree]

  class Scope(val frames: List[Frame], val stats: List[Statements]) {
    def stat(s: Tree) = new Scope(frames, (s :: stats.head) :: stats.tail)
    def +(bind: Bind) = new Scope((bind :: frames.head) :: frames.tail, stats).stat(bind.placeholder)

    def enter: Scope = new Scope(Nil :: frames, Nil :: stats)
    lazy val local: Map[TermName, Bind] = frames.foldRight(Map.empty[TermName, Bind]) {
      case (frame, res) => res ++ frame.map(b => b.name -> b)
    }
  }
  object Scope {
    def empty = new Scope(Nil, Nil)
  }

  /** type checking tree with enclosing scope */
  def typeCheck[U <: u.type](scope: Scope, tree: Tree)(implicit tb: ToolBox[U]): Tree = tree match {
    case _ =>
      def inScope(tree: Tree) = {
        val definitions = ListBuffer.empty[Tree]
        for (imp <- scope.stats.flatten.reverse) definitions += imp
        Block(definitions.toList, tree)
      }
      val exprTree = inScope(tree).duplicate.asInstanceOf[tb.u.Tree]
      val result = util.Try {
        tb.typeCheck(exprTree).asInstanceOf[Tree]
      }.getOrElse(EmptyTree)
//      println(s"TypeCheck($tree) = ${result.tpe}")
      result
  }

  val tempNameCounter = new AtomicInteger(0)
  /** generates unique term name with given root */
  def freshTermName(root: String) = {
    val num = tempNameCounter.incrementAndGet()
    newTermName(s"__temp$$$root$$$num")
  }


  def isCellType(tpe: Type): Boolean = tpe <:< weakTypeOf[Cell[_]]
  def isCellTree[U <: u.type](scope: Scope, tree: Tree)(implicit tb: ToolBox[U]): Boolean =
    isCellType(typeCheck(scope, tree).tpe)

  def wrapTypeTree(tpt: Tree): Tree = tpt match {
    case Ident(name) => AppliedTypeTree(Ident(newTypeName("Cell")), List(tpt))
    case EmptyTree => EmptyTree
    case TypeTree() => TypeTree()
    case tpTree : TypTree => AppliedTypeTree(Ident(newTypeName("Cell")), List(tpt))
  }

  def cellConstTree(tree: Tree) = {
    Apply(
      Select(Ident(newTermName("Cell")), newTermName("constant")),
      List(tree, Literal(Constant(show(tree))))
    )
  }
  def cellAppTree(tree: Tree, originalTree: Tree, cells: List[TermName]) = {
    Apply(
      Apply(
        Select(Ident(newTermName("Cell")), newTermName("app")),
        List(tree, Literal(Constant(show(originalTree))))
      ),
      cells.map(c => Ident(c))
    )
  }
  def cellFlatAppTree(tree: Tree, originalTree: Tree, cells: List[TermName]) = {
    Apply(
      Apply(
        Select(Ident(newTermName("Cell")), newTermName("flatApp")),
        List(tree, Literal(Constant(show(originalTree))))
      ),
      cells.map(c => Ident(c))
    )
  }

  def cellValidate(tree: Tree, cells: List[TermName]) = {
    Apply(
      Apply(
        Select(Ident(newTermName("Cell")), newTermName("validate")),
        List(tree, Literal(Constant(show(tree))))
      ),
      cells.map(c => Ident(c))
    )
  }

}
