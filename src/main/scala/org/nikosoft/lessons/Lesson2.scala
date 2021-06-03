package org.nikosoft.lessons

import org.nikosoft.lessons.Lesson1._

object Lesson2 {

  class ScalaMonad[T, F[T1] <: Monad[T1, F]](m: F[T]) {
    def flatMap[B](f: T => F[B]): F[B] = m.bind(f)(m)
    def map[B](f: T => B): F[B] = m.fmap(f)(m)
  }

  implicit def scalaifyMonad[T, F[T1] <: Monad[T1, F]](monad: F[T]): ScalaMonad[T, F] = new ScalaMonad[T, F](monad)

  def main(args: Array[String]): Unit = {
      val a = Consoliser(1.0).flatMap(stringGeneratorM).flatMap(integiserM).flatMap(bigDecimaliserM).value
      println(s"Effectful function via flatMap result ${Consoliser(1.0).flatMap(stringGeneratorM).flatMap(integiserM).flatMap(bigDecimaliserM).value}")
  }

}
