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

//  // types
//  trait Pure[F[T] <: Monad[T, F]] {
//    def pure[K]: K => F[K]
//  }

  trait Monad[A, F[T] <: Monad[T, F]] {
    // monad part
    def bind[B]: (A => F[B]) => F[A] => F[B]
    def `>>=`[B]: (A => F[B]) => F[A] => F[B] = bind

    // functor part
    def fmap[B]: (A => B) => F[A] => F[B]
    def `<@>`[B]: (A => B) => F[A] => F[B] = fmap
  }

  // convenience functions
  implicit class MonadicComposition[T, K, F[K1] <: Monad[K1, F]](leftM: T => F[K]) {
    def `>=>`[L](f: K => F[L]): T => F[L] = { t =>
      val fK = leftM(t)
      fK.bind(k => f(k))(fK) }
  }

//  def pure[T, F[T1] <: Monad[T1, F] : Pure](value: T): F[T] = {
//    implicitly[Pure[F]].pure[T](value)
//  }

  // a monad for demonstration

//  implicit val consoliserPure: Pure[Consoliser] = new Pure[Consoliser] {
//    override def pure[K]: K => Consoliser[K] = value => new Consoliser[K](value)
//  }

  case class Consoliser[T](value: T) extends Monad[T, Consoliser] {
    override def bind[B]: (T => Consoliser[B]) => Consoliser[T] => Consoliser[B] = f => {
      case Consoliser(v) => f(v)
    }
    override def fmap[B]: (T => B) => Consoliser[T] => Consoliser[B] = f => {
      case Consoliser(v) => println("BOOM"); Consoliser(f(v))
    }
  }

  def main(args: Array[String]): Unit = {
    // run
    println(megaFuncM(5.0).value)
  }

}
