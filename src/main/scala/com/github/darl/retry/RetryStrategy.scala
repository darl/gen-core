package com.github.darl.retry

import java.lang.reflect.InvocationTargetException
import scala.util.control.Exception

/**
 * User: Vladislav Dolbilov (darl@yandex-team.ru)
 * Date: 23.05.13 11:47
 */
case class RetryStrategy(maxRetryCount: Int = 10)

object Retry extends Retry

trait Retry {

  val unwrapping = Exception.catching(classOf[InvocationTargetException]).withApply {
    case e: InvocationTargetException => throw e.getTargetException
  }

  def tryComplete[T](block: => T, clean: => Unit, strategy: RetryStrategy): T = {
    def doTry(tryNumber: Int): T = {
      try {
        unwrapping {
          block
        }
      } catch {
        case e: RetryException =>
          if (tryNumber < strategy.maxRetryCount) {
            clean
            doTry(tryNumber + 1)
          } else throw e
      }
    }
    doTry(0)
  }

  def tryComplete[T](block: => T, strategy: RetryStrategy): T = {
    def doTry(tryNumber: Int): T = {
      try {
        unwrapping {
          block
        }
      } catch {
        case e: RetryException =>
          if (tryNumber < strategy.maxRetryCount) doTry(tryNumber + 1) else throw e
      }
    }
    doTry(0)
  }

}
