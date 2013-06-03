package com.github.darl.util

import org.slf4j.LoggerFactory

/**
 * User: Vladislav Dolbilov (darl@yandex-team.ru)
 * Date: 03.06.13 15:58
 */
trait Logging {
  protected[this] val logger = LoggerFactory.getLogger(getClass)
}
