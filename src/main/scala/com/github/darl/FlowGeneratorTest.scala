package com.github.darl

import com.github.darl.translate.FlowScalaGenerator

/**
 * User: Vladislav Dolbilov (darl@yandex-team.ru)
 * Date: 30.05.13 12:42
 */
object FlowGeneratorTest extends App {
  val gen = new FlowScalaGenerator

  val res = gen.generate(
    """
      |def gcd(a: Int, b: Int) = {
      |  def gcd0(a: Int, b: Int, steps: Int): (Int, Int) = {
      |    if (b == 0) (a, steps)
      |    else gcd0(b, a % b, steps + 1)
      |  }
      |  if (a > b) gcd0(a, b, 0)
      |  else gcd0(b, a, 0)
      |}
      |
      |val a = random.from(100 to 999)
      |val b = random.from(100 to 1000)
      |val (g, steps) = gcd(a, b)
      |validate(g != 1)
      |validate(steps between (4 and 6))
      |
      |Problem(
      |  statement = s"Чему равен НОД чисел $a и $b?",
      |  answer = s"НОД($a, $b)=$g, шагов: $steps"
      |)
    """.stripMargin, Map.empty)
  println(res)
}
