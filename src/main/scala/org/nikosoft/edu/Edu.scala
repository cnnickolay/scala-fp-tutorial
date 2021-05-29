package org.nikosoft.edu

object Edu extends App {

  trait Pure[F[_]] {
    def pure[K]: K => F[K]
  }
  trait Monad[T, F[T1] <: Monad[T1, F]] {
    def map[K](t: T => K): F[K]
    def flatMap[K](f: T => F[K]): F[K]
  }

  class Consoliser[T](val value: T) extends Monad[T, Consoliser] {
    override def map[K](t: T => K): Consoliser[K] = t(value).pure
    override def flatMap[K](f: T => Consoliser[K]): Consoliser[K] = {
      println("BOOM")
      f(value)
    }
  }
  implicit val consoliserPure: Pure[Consoliser] = new Pure[Consoliser] {
    override def pure[K]: K => Consoliser[K] = value => new Consoliser[K](value)
  }

  implicit class MonadWrapper[K](value: K) {
    def pure[M[T] <: Monad[T, M]](implicit pureM: Pure[M]): M[K] = pureM.pure(value)
  }

  implicit class MonadicBinding[T, F[T1] <: Monad[T1, F]](leftM: F[T]) {
    def `>>=`[K](f: T => F[K]): F[K] = leftM.flatMap(value => f(value))
  }

  implicit class Applicative[A, B, F[T] <: Monad[T, F]](leftM: F[A => B]) {
    def `<*>`(f: F[A]): F[B] = f.flatMap(a => leftM.map(fa => fa(a)))
  }

  implicit class MonadicComposition[T, K, F[K1] <: Monad[K1, F]](leftM: T => F[K]) {
    def `>=>`[L](f: K => F[L]): T => F[L] = { t => leftM(t).flatMap(k => f(k)) }
  }

  implicit class MonadicMap[A, B](f: A => B) {
    def `<&>`[F[T] <: Monad[T, F]](right: F[A]): F[B] = right.map(f)
    def `<@>`[F[T] <: Monad[T, F]](right: F[A]): F[B] = right.map(f)
    def fmap[F[T] <: Monad[T, F]](right: F[A]): F[B] = right.map(f)
  }

  implicit class MonadicMapReverse[A, F[T] <: Monad[T, F]](right: F[A]) {
    def `<&>`[B](f: A => B): F[B] = right.map(f)
  }

  // pure
  def stringGenerator: Double => String = _.toString
  def integiser: String => Int = _.toDouble.toInt
  def bigDecimaliser: Int => BigDecimal = v => BigDecimal.valueOf(v)

  def megaFunc = stringGenerator andThen integiser andThen bigDecimaliser

  // monadic
  def stringGeneratorM: Double => Consoliser[String] = _.toString.pure
  def integiserM: String => Consoliser[Int] = _.toDouble.toInt.pure
  def bigDecimaliserM: Int => Consoliser[BigDecimal] = v => BigDecimal.valueOf(v).pure

  def megaFuncM = stringGeneratorM >=> integiserM >=> bigDecimaliserM

  val result = megaFunc(1)
  println(s"Pure function result $result")

  val resultM = megaFuncM(1)
  println(s"Effectful function result ${resultM.value}")

  println(s"Effectful function via flatMap result ${1.0.pure.flatMap(stringGeneratorM).flatMap(integiserM).flatMap(bigDecimaliserM).value}")

  def f: Int => String => Double => BigDecimal => Unit = _ => _ => _ => _ => 1.0

  def tuple4[A, B, C, D]: A => B => C => D => (A, B, C, D) = a => b => c => d => (a, b, c, d)

  val appMonad = tuple4[Int, String, Double, BigDecimal].pure <*> 5.pure <*> "".pure <*> 0.1.pure <*> BigDecimal.valueOf(0).pure <&> { case (a, b, c, d) => () }

  case class Person(name: String, lastName: String)

  for {
    person <- (Person.apply _).curried <@> "Niko".pure <*> "Che".pure
    _ = println(person)
    _ <- appMonad
    a1 <- 1.0.pure
    a2 <- stringGeneratorM(a1)
    a3 <- integiserM(a2)
    a4 <- bigDecimaliserM(a3)
  } yield a4 + 1

}
