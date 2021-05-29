package org.nikosoft.edu

import org.nikosoft.edu.Edu.Monad
import org.nikosoft.edu.Or.{Bad, Good}

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait Or[+T]

object Or {
  case class Good[T](value: T) extends Or[T]
  case class Bad(throwable: Throwable) extends Or[Nothing]
}

class OrM[T](val or: Or[T]) extends Monad[T, OrM] {
  override def map[K](t: T => K): OrM[K] = or match {
    case Good(value) => OrM(Good(t(value)))
    case bad: Bad => OrM(bad)
  }
  override def flatMap[K](f: T => OrM[K]): OrM[K] = or match {
    case Good(value) => f(value)
    case bad: Bad => OrM(bad)
  }
}
object OrM {
  def apply[T](t: Or[T]): OrM[T] = new OrM[T](t)
  def unsafe[T](f: => T): OrM[T] = Try(f) match {
    case Success(value) => OrM(Good(value))
    case Failure(exception) => OrM(Bad(exception))
  }
}
