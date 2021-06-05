package org.nikosoft.lessons

import org.nikosoft.lessons.Lesson1._
import org.nikosoft.lessons.Lesson2._
import org.nikosoft.lessons.Lesson2.MonadRich
import org.nikosoft.lessons.Lesson3.Or._

import scala.util.{Failure, Success, Try}

// creating a first useful monad
object Lesson3 {

  // first goes ADT
  trait Or[+T]

  object Or {
    case class Good[T](value: T) extends Or[T]
    case class Bad(throwable: Throwable) extends Or[Nothing]
    class Ugly[T](input: => T) extends Or[T] {
      def value: T = input
    }
    object Ugly {
      def apply[T](v: => T): Ugly[T] = new Ugly(v)
    }

    implicit val orM: Monad[Or] = new Monad[Or] {
      override def bind[A, B]: (A => Or[B]) => Or[A] => Or[B] = f => {
        case gM: Good[A] => f(gM.value)
        case bM: Bad => bM
        case uM: Ugly[A] => Try(uM.value).fold(t => Bad(t), g => f(g))
      }

      override def fmap[A, B]: (A => B) => Or[A] => Or[B] = f => {
        case gM: Good[A] => Good(f(gM.value))
        case bM: Bad => bM
        case uM: Ugly[A] => Ugly(f(uM.value))
      }
    }

    def good[T](v: T): Or[T] = Good(v)
    def bad[T](t: Throwable): Or[T] = Bad(t)
    def ugly[T](v: => T): Or[T] = Ugly(v)
    def compute[T](v: Or[T]): Or[T] = v >>= good
  }

  def main(args: Array[String]): Unit = {
    // let's put it together
    val result = for {
      i <- ugly(10 - 19)
      r <- ugly(i / 0)
      k <- good(r)
      res <- ugly(k + 10)
    } yield res + 10

    println(result)
  }
}
