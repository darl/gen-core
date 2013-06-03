package com.github.darl

import com.github.darl.retry.{RetryStrategy, Retry}
import com.github.darl.util.Logging
import collection.mutable.ArrayBuffer

package object flow extends Logging {

  private[flow] class ConstCell[T](const: T, name: String) extends Cell[T] {
    def apply() = const
    def clear() {}
    def invalidate() {}
    def subscribe(c: Cell[_]) {}
  }

  private[flow] class AppCell[T](c: => T, name: String)(deps: Seq[Cell[_]]) extends Cell[T] {
    deps.foreach(_.subscribe(this))

    val subscribers = ArrayBuffer.empty[Cell[_]]
    var value: Option[T] = None

    def apply() = {
      value match {
        case Some(v) => v
        case None =>
          val newVal = Retry.tryComplete(c, this.invalidate(), RetryStrategy())
          println(s"calculated: $name = $newVal")
          value = Some(newVal)
          newVal
      }
    }

    def clear() {
      value = None
      subscribers.foreach(_.clear())
    }

    def invalidate() {
      clear()
      deps.foreach(_.invalidate())
    }
    def subscribe(c: Cell[_]) {
      subscribers += c
    }
  }

  private[flow] class FlatApp[T](c: => Cell[T], name: String)(deps: Seq[Cell[_]]) extends Cell[T] {
    deps.foreach(_.subscribe(this))

    val subscribers = ArrayBuffer.empty[Cell[_]]
    var cell: Option[Cell[T]] = None

    def apply() = {
      cell match {
        case Some(ce) => ce.apply()
        case None =>
          val newCell = Retry.tryComplete(c, this.invalidate(), RetryStrategy())
          println(s"calculated: $name = ${newCell()}")
          cell = Some(newCell)
          newCell.subscribe(this)
          newCell.apply()
      }
    }

    def clear() {
      cell.foreach(_.clear())
      cell = None
      subscribers.foreach(_.clear())
    }

    def invalidate() {
      cell.foreach(_.invalidate())
      clear()
      deps.foreach(_.invalidate())
    }

    def subscribe(c: Cell[_]) {
      subscribers += c
    }
  }

}
