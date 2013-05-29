package com.github.darl.retry

import com.github.darl.Generator

import java.lang.reflect.InvocationTargetException
import scala.util.control.Exception

/**
 * User: Vladislav Dolbilov (darl@yandex-team.ru)
 * Date: 23.05.13 11:47
 */
case class RetryStrategy(maxRetryExceptions: Int = 10000, maxRuntimeExceptions: Int = 1000, maxExceptions: Int = 10)

trait Retry { this: Generator =>

  val unwrapping = Exception.catching(classOf[InvocationTargetException]).withApply {
    case e: InvocationTargetException => throw e.getTargetException
  }

  def tryComplete[T](block: => T, strategy: RetryStrategy): T = {
    def doTry(retryEx: Int, runtimeEx: Int, generalEx: Int): T = {
      println("try number: " + (retryEx, runtimeEx, generalEx))
      try {
        unwrapping {
          block
        }
      } catch {
        case e: RetryException =>
          if (retryEx < strategy.maxRetryExceptions) doTry(retryEx + 1, runtimeEx, generalEx) else throw e
        case e: RuntimeException =>
          if (runtimeEx < strategy.maxRuntimeExceptions) doTry(retryEx, runtimeEx + 1, generalEx) else throw e

        case e: Exception =>
          if (generalEx < strategy.maxExceptions) doTry(retryEx, runtimeEx, generalEx + 1) else throw e
      }
    }
    doTry(0, 0, 0)
  }

}
