package com.github.darl.flow

import util.DynamicVariable
import collection.mutable

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

  /** subscribe on this cell clears */
  def unsubscribe(c: Cell[_])
}

object Cell {
  def constant[T](c: T, name: String): Cell[T] = new ConstCell[T](c, name)

  def app[T](c: => T, name: String)(deps: Cell[_]*): Cell[T] = new AppCell[T](c, name)(deps)

  def flatApp[T](c: => Cell[T], name: String)(deps: Cell[_]*): Cell[T] = new FlatApp[T](c, name)(deps)

  val validators = new DynamicVariable[mutable.ArrayBuffer[Cell[Boolean]]](null)

  def validate(c: => Boolean, name: String)(deps: Cell[_]*): Cell[Boolean] = {
    val validator = new AppCell[Boolean](c, name)(deps)
    validators.value += validator
    validator
  }
}
