package com.github.darl.simple

import com.github.darl.Generator
import com.github.darl.runtime.Problem

import scala.reflect.runtime.{universe => u}
import scala.tools.reflect._
import com.github.darl.retry.{Retry, RetryStrategy}

/**
  * User: Vladislav Dolbilov (darl@yandex-team.ru)
 * Date: 23.05.13 11:19
 */
class SimpleScalaGenerator extends Generator with Retry{
  def generate(source: String, params: Map[String, String]) = {
    val ru = u.runtimeMirror(getClass.getClassLoader)
    val tb = ToolBox(ru).mkToolBox(mkConsoleFrontEnd())

    val tree = tb.parse(
      """import com.github.darl.runtime._
        |
      """.stripMargin + source)

    val block: () => Any = tb.compile(tree)

    val result = tryComplete(block.apply(), RetryStrategy())

    result.asInstanceOf[Problem]
  }
}
