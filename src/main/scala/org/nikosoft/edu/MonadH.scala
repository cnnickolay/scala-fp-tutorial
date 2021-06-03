package org.nikosoft.edu

object MonadH {

  // declarations
  trait Monad[A, F[T] <: Monad[T, F]] {
    // monad part
    def bind[B]: (A => F[B]) => F[A] => F[B]
    def `>>=`[B]: (A => F[B]) => F[A] => F[B] = bind

    // functor part
    def fmap[B]: (A => B) => F[A] => F[B]
    def `<@>`[B]: (A => B) => F[A] => F[B] = fmap
  }

  trait ScalaM[A, F[T] <: Monad[T, F]]{
    def flatMap[B](f: A => F[B]): F[B]
    def map[B](f: A => B): F[B]
  }

  trait Pure[A, F[T] <: Monad[T, F]] {
    def pure: A => F[A]
  }

  // example implementation
  case class ValueStore[T](value: T)

  case class MyMonadM[T](value: ValueStore[T]) extends Monad[T, MyMonadM] {
    override def bind[B]: (T => MyMonadM[B]) => MyMonadM[T] => MyMonadM[B] = f => {
      case MyMonadM(ValueStore(v)) => f(v)
    }
    override def fmap[B]: (T => B) => MyMonadM[T] => MyMonadM[B] = f => {
      case MyMonadM(ValueStore(v)) => MyMonadM(ValueStore(f(v)))
    }
  }

  class ScalaMonad[T, F[T1] <: Monad[T1, F]](m: F[T]) extends ScalaM[T, F] {
    override def flatMap[B](f: T => F[B]): F[B] = m.bind(f)(m)
    override def map[B](f: T => B): F[B] = m.fmap(f)(m)
  }

  implicit def scalaifyMonad[T, F[T1] <: Monad[T1, F]](monad: F[T]): ScalaM[T, F] = new ScalaMonad[T, F](monad)

  // running
  val result = for {
    r <- MyMonadM(ValueStore(10 + 20))
    r1 <- MyMonadM(ValueStore(10 + r))
  } yield r1
  println(result.value)
}
