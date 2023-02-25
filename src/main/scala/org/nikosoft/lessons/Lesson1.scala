package org.nikosoft.lessons

object Lesson1 {

  case class CommandLineArguments(value: List[String])
  case class UserName(value: String)
  case class Password(value: String)
  case class DBAdapter()
  case class Result(value: String)

  // pure functions and composition
  def readUserName: CommandLineArguments => (UserName, Password) = _ => (UserName("Fan"), Password("Tozzi"))
  def connectToDatabase: ((UserName, Password)) => DBAdapter = _ => DBAdapter()
  def readFromDatabase: DBAdapter => Result = _ => Result("Some secret data")

  def program: CommandLineArguments => Result = readUserName andThen connectToDatabase andThen readFromDatabase

  // effectful functions and composition
  def readUserNameM[T[_] : Monad](args: CommandLineArguments): T[(UserName, Password)] = pure[(UserName, Password), T]((UserName("Fan"), Password("Tozzi")))
  def connectToDatabaseM[T[_] : Monad](nameAndPass: (UserName, Password)): T[DBAdapter] = pure[DBAdapter, T](DBAdapter())
  def readFromDatabaseM[T[_] : Monad](adapter: DBAdapter): T[Result] = pure[Result, T](Result("Some secret data"))

  def programM[T[_] : Monad](args: CommandLineArguments): T[Result] = (readUserNameM[T] _ >=> connectToDatabaseM[T] >=> readFromDatabaseM[T])(args)

  trait Monad[F[_]] {
    // function creating monad
    def pure[T](value: T): F[T]

    // monad part
    def bind[A, B]: (A => F[B]) => F[A] => F[B]

    // functor part
    def fmap[A, B]: (A => B) => F[A] => F[B]
  }

  def pure[T, F[_] : Monad](v: T): F[T] = {
    val monad = implicitly[Monad[F]]
    monad.pure(v)
  }

  // convenience functions
  implicit class MonadicComposition[T, K, F[_] : Monad](leftM: T => F[K]) {
    def `>=>`[L](f: K => F[L]): T => F[L] = { t =>
      implicitly[Monad[F]].bind(f)(leftM(t))
    }
  }

  // a monad for demonstration
  case class SafeExec[T](value: T)
  case class SafeExec2[T](value: T)
  implicit val safeExecMonad: Monad[SafeExec] = new Monad[SafeExec] {
    override def bind[A, B]: (A => SafeExec[B]) => SafeExec[A] => SafeExec[B] = f => {
      case SafeExec(v) => f(v)
    }
    override def fmap[A, B]: (A => B) => SafeExec[A] => SafeExec[B] = f => {
      case SafeExec(v) => SafeExec(f(v))
    }
    override def pure[T](value: T): SafeExec[T] = SafeExec(value)
  }
  implicit val safeExecMonad2: Monad[SafeExec2] = new Monad[SafeExec2] {
    override def bind[A, B]: (A => SafeExec2[B]) => SafeExec2[A] => SafeExec2[B] = f => {
      case SafeExec2(v) => f(v)
    }
    override def fmap[A, B]: (A => B) => SafeExec2[A] => SafeExec2[B] = f => {
      case SafeExec2(v) => SafeExec2(f(v))
    }
    override def pure[T](value: T): SafeExec2[T] = SafeExec2(value)
  }

  def main(args: Array[String]): Unit = {
    // run
    println(programM[SafeExec](CommandLineArguments(Nil)))
  }

}
