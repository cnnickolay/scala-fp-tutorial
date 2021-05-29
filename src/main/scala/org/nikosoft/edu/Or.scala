package org.nikosoft.edu

import org.nikosoft.edu.Edu.Monad

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait Or[+T]

object Or {
  def unsafe[T](f: => T): Or[T] = Try(f) match {
    case Success(value) => Good(value)
    case Failure(exception) => Bad(exception)
  }
  case class Good[T](value: T) extends Or[T]
  case class Bad(throwable: Throwable) extends Or[Nothing]

  class OrM[T](val or: Or[T]) extends Monad[T, OrM] {
    override def map[K](t: T => K): OrM[K] = or match {
      case Good(value) => Good(t(value))
      case bad: Bad => bad
    }
    override def flatMap[K](f: T => OrM[K]): OrM[K] = or match {
      case Good(value) => f(value)
      case bad: Bad => bad
    }
  }

  implicit def toMonad[T](or: Or[T]): OrM[T] = new OrM[T](or)
}
