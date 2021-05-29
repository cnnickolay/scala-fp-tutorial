package org.nikosoft.edu

import org.nikosoft.edu.Edu.Monad

case class Counter[T](t: T, total: Int)

class CounterM[T](val value: Counter[T]) extends Monad[T, CounterM] {
  override def map[K](t: T => K): CounterM[K] = new CounterM(value.copy(t = t(value.t)))
  override def flatMap[K](f: T => CounterM[K]): CounterM[K] = {
    val totalLeft = value.total
    val counter = f(value.t).value
    val totalRight = counter.total
    new CounterM(counter.copy(total = totalLeft + totalRight))
  }
}

object CounterM {
  def count[T](f: => T): CounterM[T] = new CounterM[T](Counter(f, 1))
}