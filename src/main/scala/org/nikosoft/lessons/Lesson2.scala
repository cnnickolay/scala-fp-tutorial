package org.nikosoft.lessons

import org.nikosoft.lessons.Lesson1._
import scala.language.implicitConversions

object Lesson2 {

  class ScalaMonadWrapper[T, F[_] : Monad](m: F[T]) {
    def flatMap[B](f: T => F[B]): F[B] = implicitly[Monad[F]].bind(f)(m)
    def map[B](f: T => B): F[B] = implicitly[Monad[F]].fmap(f)(m)
  }

  implicit def scalaify[A, F[_] : Monad](f: F[A]): ScalaMonadWrapper[A, F] = new ScalaMonadWrapper[A, F](f)

  implicit class MonadRich[A, F[_] : Monad](left: F[A]) {
    def `>>=`[B](f: A => F[B]): F[B] = {
      val monad = implicitly[Monad[F]]
      monad.bind(f)(left)
    }

    def `<&>`[B](f: A => B): F[B] = {
      val monad = implicitly[Monad[F]]
      monad.fmap(f)(left)
    }
  }

  def main(args: Array[String]): Unit = {
    val a = SafeExec(CommandLineArguments(Nil)).flatMap(readUserNameM).flatMap(connectToDatabaseM).flatMap(readFromDatabaseM).value
    println(s"Effectful function via flatMap result $a")

    for {
      name      <- readUserNameM(CommandLineArguments(Nil))
      dbAdapter <- connectToDatabaseM(name)
      result    <- readFromDatabaseM(dbAdapter)
    } yield result

    val programM = readUserNameM(CommandLineArguments(Nil)) >>= connectToDatabaseM >>= readFromDatabaseM
    println(programM)
  }

}
