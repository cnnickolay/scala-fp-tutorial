package org.nikosoft.lessons

import org.nikosoft.lessons.Lesson4.{Applicative, ApplicativeRich}
import org.nikosoft.lessons.Lesson6.Compute.Compute
import Lesson6.Compute._
//import org.nikosoft.lessons.Lesson1._
import org.nikosoft.lessons.Lesson2._

import scala.util.Try

// brewing a giga monad
object Lesson6 {

  object Compute {
    sealed trait Compute[+T]
    case class Pure[T](value: T) extends Compute[T]
    case class Effect[T](value: () => T) extends Compute[T]
    case class Failure(t: Throwable) extends Compute[Nothing]

    sealed trait PureMust[T] extends Compute[T]
    case class PureMustSome[T](value: Option[T]) extends PureMust[T]
    case object PureMustNone extends PureMust[Nothing]

    case class Error(error: String) extends Compute[Nothing]
    case class Errors(errors: List[String]) extends Compute[Nothing]

    def pure[T](t: T): Compute[T] = Pure(t)
    def someMust[T](t: Option[T]): Compute[T] = PureMustSome(t)
    def error[T](err: String): Compute[T] = Error(err)
  }

  implicit val computeApplicative: Applicative[Compute] = new Applicative[Compute] {
    override def fmap[A, B]: (A => B) => Compute[A] => Compute[B] = f => {
      case Pure(value) => Pure(f(value))
      case PureMustSome(value) => PureMustSome(value.map(f))
      case PureMustNone => PureMustNone
      case Effect(value) => Effect(() => f(value()))
      case e: Error => e
      case e: Errors => e
      case f: Failure => f
    }
    override def bind[A, B]: (A => Compute[B]) => Compute[A] => Compute[B] = f => {
      case Pure(value) => f(value)
      case PureMustSome(Some(value)) => f(value)
      case PureMustNone | PureMustSome(None) => PureMustNone
      case Effect(value) => Try(value()).fold(t => Failure(t), f)
      case e: Error => e
      case e: Errors => e
      case f: Failure => f
    }
    override def `<*>`[A, B](f: Compute[A => B], fa: Compute[A]): Compute[B] = {
      val fb = super.`<*>`(f, fa)
      (f, fb) match {
        case (Errors(ts), Error(t2)) if !ts.contains(t2) => Errors(ts :+ t2)
        case (_, Error(ts)) => Errors(List(ts))
        case _ => fb
      }
    }

    override def pure[T](value: T): Compute[T] = Pure(value)
  }

  def main(args: Array[String]): Unit = {
    def func: String => String => Unit = _ => _ => ()
    val result1 = pure(func) <*> error("wrong param 1") <*> error("wrong param 2")
    println(result1)

    val result2 = for {
      i <- someMust(Some(10))
      res <- pure(i + 10)
    } yield res
    println(result2)
  }

}
