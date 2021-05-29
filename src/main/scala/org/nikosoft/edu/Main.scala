package org.nikosoft.edu

import org.nikosoft.edu.CounterM.count
import org.nikosoft.edu.Edu.MonadWrapper
import org.nikosoft.edu.OrM.{OrM, unsafe}

object Main extends App {
  val result = for {
    r1 <- unsafe(1 + 10)
    r2 <- unsafe(r1 + 10)
  } yield r2

  val result2 = for {
    r1 <- unsafe(1 + 10)
    r2 <- unsafe(r1 / 0)
    r3 <- unsafe(r2 * r1)
  } yield r3

  println(result.or)
  println(result2.or)

  val total = (for {
    t1 <- count(10)
    t2 <- count(10)
    _ <- count(10)
    _ <- count(10)
  } yield ()).value.total

  println(s"Total $total")


}