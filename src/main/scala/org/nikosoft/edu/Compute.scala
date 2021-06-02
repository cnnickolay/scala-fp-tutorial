package org.nikosoft.edu

sealed trait Compute[+T]
case class Pure[T](value: T) extends Compute[T]
case class Failure(t: Throwable) extends Compute[Nothing]

sealed trait PureMust[T] extends Compute[T]
case class PureMustSome[T](value: Option[T]) extends PureMust[T]
case object PureMustNone extends PureMust[Nothing]

case class Error(error: String) extends Compute[Nothing]
case class Errors(errors: List[String]) extends Compute[Nothing]