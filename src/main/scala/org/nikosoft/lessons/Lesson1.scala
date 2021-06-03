package org.nikosoft.lessons

object Lesson1 {

  // pure functions and composition
  def stringGenerator: Double => String = _.toString
  def integiser: String => Int = _.toDouble.toInt
  def bigDecimaliser: Int => BigDecimal = v => BigDecimal.valueOf(v)

  def megaFunc = stringGenerator andThen integiser andThen bigDecimaliser

  // effectful functions and composition
  def stringGeneratorM: Double => Consoliser[String] = d => Consoliser(d.toString)
  def integiserM: String => Consoliser[Int] = s => Consoliser(s.toDouble.toInt)
  def bigDecimaliserM: Int => Consoliser[BigDecimal] = v => Consoliser(BigDecimal.valueOf(v))

  def megaFuncM = stringGeneratorM >=> integiserM >=> bigDecimaliserM

  trait Monad[F[_]] {
    // monad part
    def bind[A, B]: (A => F[B]) => F[A] => F[B]

    // functor part
    def fmap[A, B]: (A => B) => F[A] => F[B]
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

  // convenience functions
  implicit class MonadicComposition[T, K, F[_] : Monad](leftM: T => F[K]) {
    def `>=>`[L](f: K => F[L]): T => F[L] = { t =>
      leftM(t) >>= f
    }
  }

  // a monad for demonstration
  case class Consoliser[T](value: T)
  implicit val consoliserMonad: Monad[Consoliser] = new Monad[Consoliser] {
    override def bind[A, B]: (A => Consoliser[B]) => Consoliser[A] => Consoliser[B] = f => {
      case Consoliser(v) => f(v)
    }
    override def fmap[A, B]: (A => B) => Consoliser[A] => Consoliser[B] = f => {
      case Consoliser(v) => println("BOOM"); Consoliser(f(v))
    }
  }

  def main(args: Array[String]): Unit = {
    // run
    println(megaFuncM(5.0).value)
  }

}
