package com.github.darl

import com.github.darl.runtime.Problem

/**
 * User: Vladislav Dolbilov (darl@yandex-team.ru)
 * Date: 23.05.13 1:20
 */
trait Generator {
  def generate(source: String, params: Map[String, String]): Problem
}
