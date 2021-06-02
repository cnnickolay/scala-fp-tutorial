package org.nikosoft.lessons

object Lesson1 {

  // pure functions and composition
  def stringGenerator: Double => String = _.toString
  def integiser: String => Int = _.toDouble.toInt
  def bigDecimaliser: Int => BigDecimal = v => BigDecimal.valueOf(v)

  def megaFunc = stringGenerator andThen integiser andThen bigDecimaliser

  // effectful functions and composition
  def stringGeneratorM: Double => Consoliser[String] = d => pure(d.toString)
  def integiserM: String => Consoliser[Int] = s => pure(s.toDouble.toInt)
  def bigDecimaliserM: Int => Consoliser[BigDecimal] = v => pure(BigDecimal.valueOf(v))

  def megaFuncM = stringGeneratorM >=> integiserM >=> bigDecimaliserM

  // types
  trait Pure[F[T] <: Monad[T, F]] {
    def pure[K]: K => F[K]
  }

  trait Monad[T, F[T1] <: Monad[T1, F]] {
    def map[K](t: T => K): F[K]
    def flatMap[K](f: T => F[K]): F[K]
  }

  // convenience functions
  implicit class MonadicComposition[T, K, F[K1] <: Monad[K1, F]](leftM: T => F[K]) {
    def `>=>`[L](f: K => F[L]): T => F[L] = { t => leftM(t).flatMap(k => f(k)) }
  }

  def pure[T, F[T1] <: Monad[T1, F] : Pure](value: T): F[T] = {
    implicitly[Pure[F]].pure[T](value)
  }

  // a monad for demonstration

  implicit val consoliserPure: Pure[Consoliser] = new Pure[Consoliser] {
    override def pure[K]: K => Consoliser[K] = value => new Consoliser[K](value)
  }

  class Consoliser[T](val value: T) extends Monad[T, Consoliser] {
    override def map[K](t: T => K): Consoliser[K] = pure[K, Consoliser](t(value))
    override def flatMap[K](f: T => Consoliser[K]): Consoliser[K] = {
      println("BOOM")
      f(value)
    }
  }

  def main(args: Array[String]): Unit = {
    // run
    println(megaFuncM(5.0).value)

    val a = pure(1.0).flatMap(stringGeneratorM).flatMap(integiserM).flatMap(bigDecimaliserM).value
    println(s"Effectful function via flatMap result ${pure(1.0).flatMap(stringGeneratorM).flatMap(integiserM).flatMap(bigDecimaliserM).value}")
  }

}
