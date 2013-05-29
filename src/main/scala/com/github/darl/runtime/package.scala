package com.github.darl

import com.github.darl.retry.RetryException

/**
 * User: Vladislav Dolbilov (darl@yandex-team.ru)
 * Date: 23.05.13 11:45
 */
package object runtime {
  def validate(condition: => Boolean) {
    if (!condition) {
      throw new RetryException
    }
  }

  def validate(condition: => Boolean, msg: String) {
    if (!condition) {
      throw new RetryException(msg)
    }
  }

  case class AndRange(from: Int, to: Int)
  implicit class IntDsl(a: Int) {
    def and(b: Int) = AndRange(a, b)
    def between(range: AndRange): Boolean = a >= range.from && a <= range.to
  }
}
