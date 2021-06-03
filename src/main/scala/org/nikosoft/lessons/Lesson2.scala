package org.nikosoft.lessons

import org.nikosoft.lessons.Lesson1._

object Lesson2 {

  class ScalaMonadWrapper[T, F[_] : Monad](m: F[T]) {
    def flatMap[B](f: T => F[B]): F[B] = implicitly[Monad[F]].bind(f)(m)
    def map[B](f: T => B): F[B] = implicitly[Monad[F]].fmap(f)(m)
  }

  object ScalaMonadWrapper {
    implicit def scalaify[A, F[_] : Monad](f: F[A]): ScalaMonadWrapper[A, F] = new ScalaMonadWrapper[A, F](f)
  }

  def main(args: Array[String]): Unit = {
    import ScalaMonadWrapper._
    val a = Consoliser(1.0).flatMap(stringGeneratorM).flatMap(integiserM).flatMap(bigDecimaliserM).value
    println(s"Effectful function via flatMap result ${Consoliser(1.0).flatMap(stringGeneratorM).flatMap(integiserM).flatMap(bigDecimaliserM).value}")
  }

}
