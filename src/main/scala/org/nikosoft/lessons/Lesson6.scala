package org.nikosoft.lessons

import org.nikosoft.lessons.Lesson4.Applicative
import org.nikosoft.lessons.Lesson6.Compute.Compute
import Lesson6.Compute._

// brewing a giga monad
object Lesson6 {

  object Compute {
    sealed trait Compute[+T]
    case class Pure[T](value: T) extends Compute[T]
    case class Failure(t: Throwable) extends Compute[Nothing]

    sealed trait PureMust[T] extends Compute[T]
    case class PureMustSome[T](value: Option[T]) extends PureMust[T]
    case object PureMustNone extends PureMust[Nothing]

    case class Error(error: String) extends Compute[Nothing]
    case class Errors(errors: List[String]) extends Compute[Nothing]
  }

  implicit val computeApplicative: Applicative[Compute] = new Applicative[Compute] {
    override def bind[A, B]: (A => Compute[B]) => Compute[A] => Compute[B] = ???
    override def fmap[A, B]: (A => B) => Compute[A] => Compute[B] = ???
    override def pure[T]: T => Compute[T] = Pure(_)
    override def `<*>`[A, B](f: Compute[A => B], fa: Compute[A]): Compute[B] = super.`<*>`(f, fa)
  }

}
