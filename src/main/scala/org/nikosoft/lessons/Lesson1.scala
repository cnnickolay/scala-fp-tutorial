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
  def readUserNameM: CommandLineArguments => SafeExec[(UserName, Password)] = _ => SafeExec(UserName("Fan"), Password("Tozzi"))
  def connectToDatabaseM: ((UserName, Password)) => SafeExec[DBAdapter] = _ => SafeExec(DBAdapter())
  def readFromDatabaseM: DBAdapter => SafeExec[Result] = _ => SafeExec(Result("Some secret data"))

  def programM: CommandLineArguments => SafeExec[Result] = readUserNameM >=> connectToDatabaseM >=> readFromDatabaseM

  trait Monad[F[_]] {
    // monad part
    def bind[A, B]: (A => F[B]) => F[A] => F[B]

    // functor part
    def fmap[A, B]: (A => B) => F[A] => F[B]
  }

  // convenience functions
  implicit class MonadicComposition[T, K, F[_] : Monad](leftM: T => F[K]) {
    def `>=>`[L](f: K => F[L]): T => F[L] = { t =>
      implicitly[Monad[F]].bind(f)(leftM(t))
    }
  }

  // a monad for demonstration
  case class SafeExec[T](value: T)
  implicit val safeExecMonad: Monad[SafeExec] = new Monad[SafeExec] {
    override def bind[A, B]: (A => SafeExec[B]) => SafeExec[A] => SafeExec[B] = f => {
      case SafeExec(v) => f(v)
    }
    override def fmap[A, B]: (A => B) => SafeExec[A] => SafeExec[B] = f => {
      case SafeExec(v) => SafeExec(f(v))
    }
  }

  def main(args: Array[String]): Unit = {
    // run
    println(programM(CommandLineArguments(Nil)).value)
  }

}
