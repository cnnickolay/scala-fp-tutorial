package org.nikosoft.lessons

import org.nikosoft.lessons.Lesson1.Monad
import org.nikosoft.lessons.Lesson3.Or._
import org.nikosoft.lessons.Lesson3.OrM.unsafe

import scala.util.{Failure, Success, Try}

// creating a first useful monad
object Lesson3 {

  // first goes ADT
  trait Or[+T]

  object Or {
    case class Good[T](value: T) extends Or[T]
    case class Bad(throwable: Throwable) extends Or[Nothing]
  }

  // then goes implementation
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

  // let's put it together
/*
  for {
    _ <- unsafe()
  } yield ()
*/
}
