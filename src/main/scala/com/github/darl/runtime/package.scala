package com.github.darl

import com.github.darl.retry.RetryException

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
    /**
     * {{{1 and 5}}}
     * equals to
     * {{{AndRange(1, 5)}}}
     */
    def and(b: Int) = AndRange(a, b)

    /**
     * {{{5 between (1 and 7)}}}
     * equals to
     * {{{5 >= 1 && 5 <= 7}}}
     */
    def between(range: AndRange): Boolean = a >= range.from && a <= range.to
  }
}
