package com.github.darl

import com.github.darl.flow.Cell
import com.github.darl.runtime.random

/**
 * User: Vladislav Dolbilov (darl@yandex-team.ru)
 * Date: 30.05.13 1:32
 */
object FlowTest extends App {
  val a = Cell.app(random.from(1 to 10))()
  val b = Cell.app(random.from(1 to 10))()

  val c = Cell.app(a() + b())(a, b)
  val d = Cell.constant(10)
  val e = Cell.app(c() + d())(c, d)

  while(c() > 3) {
    c.invalidate()
  }
  println(e())
}
