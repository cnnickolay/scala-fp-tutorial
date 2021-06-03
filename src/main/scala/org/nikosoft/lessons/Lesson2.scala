package org.nikosoft.lessons

import org.nikosoft.lessons.Lesson1._

object Lesson2 {

  // the intention
  case class Person(name: String, lastName: String, phone: Option[String])

  private val createPerson: String => String => Option[String] => Person = (Person.apply _).curried

  val person: Consoliser[Person] = pure(createPerson) <*> pure("Niko") <*> pure("Che") <*> pure(Option("0928344"))

  // Applicative type and implementation
  trait Applicative[A, B] {
    protected def `<*>`[F[T] <: Monad[T, F] : Pure](leftM: F[A => B], f: F[A]): F[B] = f.flatMap { a =>
      leftM.map{ fa =>
        fa(a)
      }
    }
  }

  implicit class ApplicativeConsolise[A, B](leftM: Consoliser[A => B]) extends Applicative[A, B] {
    def `<*>`(f: Consoliser[A]): Consoliser[B] = super.`<*>`[Consoliser](leftM, f)
  }

  def main(args: Array[String]): Unit = {
    // run
    println(person.value)
  }
}
