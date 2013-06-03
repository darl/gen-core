package com.github.darl.translate

import com.github.darl.Generator
import com.github.darl.retry.{RetryStrategy, Retry}
import scala.reflect.runtime.{universe => u}
import scala.tools.reflect._
import com.github.darl.runtime.Problem
import com.github.darl.flow.Cell
import scala.collection.mutable.ArrayBuffer

/**
 * User: Vladislav Dolbilov (darl@yandex-team.ru)
 * Date: 30.05.13 12:32
 */
class FlowScalaGenerator extends Generator with Retry {
  def generate(source: String, params: Map[String, String]) = {
    val ru = u.runtimeMirror(getClass.getClassLoader)
    val tb = ToolBox(ru).mkToolBox(mkConsoleFrontEnd())

    val tree = tb.parse(
      """import com.github.darl.runtime._
        |
      """.stripMargin + source)

    val flowTree = translate(tree)(tb)

    println("Original tree: \n" + u.show(tree))
    println("Translate result: \n" + u.show(flowTree))

    val block: () => Any = tb.compile(flowTree)

    Cell.validators.withValue(ArrayBuffer.empty) {
      val result = tryComplete(block.apply(), RetryStrategy())

      val res = result.asInstanceOf[Cell[Problem]]
      val validators = Cell.validators.value
      def tryCalc = tryComplete(
        res.apply(),
        RetryStrategy()
      )
      var runs = 0
      while(!validators.forall(_.apply())) {
        for (validator <- validators if !validator()) validator.invalidate()
        runs += 1
        if (runs >= 100) sys.error("Max generations try count")
      }
      tryCalc
    }
  }
}
