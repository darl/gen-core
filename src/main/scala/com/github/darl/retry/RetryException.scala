package com.github.darl.retry

/**
 * User: Vladislav Dolbilov (darl@yandex-team.ru)
 * Date: 23.05.13 11:46
 */
class RetryException(msg: String) extends RuntimeException {
  def this() = this("")
}
