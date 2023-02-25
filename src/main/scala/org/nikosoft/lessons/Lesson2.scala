package org.nikosoft.lessons

import org.nikosoft.lessons.Lesson1._
import scala.language.implicitConversions

object Lesson2 {

  implicit class ScalaMonadWrapper[T, F[_] : Monad](m: F[T]) {
    def flatMap[B](f: T => F[B]): F[B] = implicitly[Monad[F]].bind(f)(m)
    def map[B](f: T => B): F[B] = implicitly[Monad[F]].fmap(f)(m)
  }

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
    val a: SafeExec[Result] = readUserNameM[SafeExec](CommandLineArguments(Nil)).flatMap(connectToDatabaseM[SafeExec]).flatMap(readFromDatabaseM[SafeExec])
    println(s"Effectful function via flatMap result $a")

    val result: SafeExec2[Result] = for {
      name      <- readUserNameM[SafeExec2](CommandLineArguments(Nil))
      dbAdapter <- connectToDatabaseM[SafeExec2](name)
      result    <- readFromDatabaseM[SafeExec2](dbAdapter)
    } yield result

    println(result)

    val programM = readUserNameM[SafeExec](CommandLineArguments(Nil)) >>= connectToDatabaseM[SafeExec] >>= readFromDatabaseM[SafeExec]
    println(programM)
  }

}
