package org.nikosoft.lessons

import org.nikosoft.lessons.Lesson1._
import org.nikosoft.lessons.Lesson3.Or
import org.nikosoft.lessons.Lesson3.Or.{good, _}


object Lesson4 {

  // the intention
  case class Person(name: String, lastName: String, phone: Option[String])

  trait Pure[F[_]] {
    def pure[T]: T => F[T]
  }

  // Applicative type and implementation
  trait Applicative[F[_]] extends Monad[F] with Pure[F] {
    def `<*>`[A, B](f: F[A => B], fa: F[A]): F[B] = bind[A, B] { t => bind[A => B, B]{ m => pure(m(t)) }(f) }(fa)
  }

  implicit class ApplicativeRich[A, B, F[_] : Applicative](leftM: F[A => B]) {
    def `<*>`(f: F[A]): F[B] = implicitly[Applicative[F]].`<*>`(leftM, f)
  }

  implicit val orApplicative: Applicative[Or] = new Applicative[Or] {
    override def bind[A, B]: (A => Or[B]) => Or[A] => Or[B] = orM.bind
    override def fmap[A, B]: (A => B) => Or[A] => Or[B] = orM.fmap
    override def pure[T]: T => Or[T] = good
  }

  val createPerson: String => String => Option[String] => Person = (Person.apply _).curried

  def main(args: Array[String]): Unit = {
    val person: Or[Person] = good(createPerson) <*> ugly(throw new RuntimeException("sorry1")) <*> ugly(throw new RuntimeException("sorry")) <*> good(Option("0928344"))
    println(person)
  }
}
