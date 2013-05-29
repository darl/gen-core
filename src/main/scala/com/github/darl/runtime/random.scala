package com.github.darl.runtime

import scala.util.{Random => sr}

/**
 * User: Vladislav Dolbilov (darl@yandex-team.ru)
 * Date: 30.05.13 1:31
 */
object random { p =>
  private val threadLocalRand = new ThreadLocal[sr] {
    override def initialValue = new sr()
  }
  private[random] def r = threadLocalRand.get

  def init(seed: Long) { r.setSeed(seed) }
  def randomize() { r.setSeed(System.currentTimeMillis()) }

  def int(n: Int) = r.nextInt(n)

  def element[T](coll: Seq[T]): T = coll(r.nextInt(coll.size))

  def from[T](coll: Seq[T]): T = this.element(coll)
  def from[T](a1: T, a2: T, rest: T*): T = this.from(a1 +: a2 +: rest)

  def apply[T](coll: Seq[T]): T = this.from(coll)
  def apply[T](a1: T, a2: T, rest: T*): T = this.from(Seq(a1, a2) ++ rest)

  def coin(prop: Double = 0.5d) = r.nextDouble <= prop

  def shuffle[T](coll: Seq[T]) = r.shuffle(coll)

  def subset[T](coll: Seq[T], size: Int): Seq[T] =
    this.shuffle(coll).take(size)

  def subset[T](coll: Seq[T], sizeRange: Range): Seq[T] =
    subset(coll, size = from(sizeRange))

  def elements[T](coll: Seq[T], size: Int): Seq[T] =
    for (i <- 1 to size) yield coll(r.nextInt(coll.size))

  def elements[T](coll: Seq[T], sizeRange: Range): Seq[T] =
    elements(coll, size = from(sizeRange))


  implicit class RichSeqRandoms[T](val coll: Seq[T]) {
    def shuffle: Seq[T] = p.shuffle(coll)
    def subset(size: Int): Seq[T] = p.subset(coll, size)
    def subset(sizeRange: Range): Seq[T] = p.subset(coll, sizeRange)
    def elements(size: Int): Seq[T] = p.elements(coll, size)
    def elements(sizeRange: Range): Seq[T] = p.elements(coll, sizeRange)
  }
}
