package org.nikosoft.lessons

import org.nikosoft.lessons.Lesson1._
import org.nikosoft.lessons.Lesson3.Or
import org.nikosoft.lessons.Lesson3.Or.{good, _}
import org.nikosoft.lessons.Lesson4.{Applicative, Person, createPerson}

// making applicative harvesting all errors
object Lesson5 {

  implicit val orApplicative: Applicative[Or] = new Applicative[Or] {
    override def bind[A, B]: (A => Or[B]) => Or[A] => Or[B] = orM.bind
    override def fmap[A, B]: (A => B) => Or[A] => Or[B] = orM.fmap
    override def pure[T]: T => Or[T] = good
    override def `<*>`[A, B](f: Or[A => B], fa: Or[A]): Or[B] = {
      val value = super.`<*>`(f, fa)
      (f, fa) match {
        case (Bad(t1), _: Ugly[A]) => value match {
          case Bad(t2) => bad(new Throwable(s"${t1.getMessage}, ${t2.getMessage}"))
          case _ => value
        }
        case _ => value
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val person: Or[Person] = good(createPerson) <*> ugly(throw new RuntimeException("name is wrong")) <*> ugly(throw new RuntimeException("lastname is wrong")) <*> good(Option("0928344"))
    println(person)
  }

}
