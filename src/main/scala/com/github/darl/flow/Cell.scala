package com.github.darl.flow

/**
 * Base trait to store voidable value
 */
trait Cell[T] {
  /** returns inner value. calculates it if needed */
  def apply(): T

  /** clears this and all subscribed cells */
  def clear()

  /** clears this and all dependant cells */
  def invalidate()

  /** subscribe on this cell clears */
  def subscribe(c: Cell[_])
}

object Cell {
  def constant[T](c: T): Cell[T] = new ConstCell[T](c)

  def app[T](c: => T)(deps: Cell[_]*): Cell[T] = new AppCell[T](c)(deps)

  def flatApp[T](c: => Cell[T])(deps: Cell[_]*): Cell[T] = new FlatApp[T](c)(deps)
}
