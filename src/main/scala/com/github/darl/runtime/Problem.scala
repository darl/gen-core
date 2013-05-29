package com.github.darl.runtime

/**
 * User: Vladislav Dolbilov (darl@yandex-team.ru)
 * Date: 23.05.13 11:13
 */
case class Problem(statement: String, answer: String = "", data: Map[String, String] = Map.empty) {
  override def toString = s"""Задание: $statement
                             |Ответ: $answer
                             |Данные: $data
                              """.stripMargin
}
