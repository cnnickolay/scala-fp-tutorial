package org.nikosoft.edu

sealed trait Compute[+T] {
  def runtime: Runtime
}
case class Pure[T](value: T, runtime: Runtime) extends Compute[T]
case class Failure(t: Throwable, runtime: Runtime) extends Compute[Nothing]

sealed trait PureMust[T] extends Compute[T]
case class PureMustSome[T](value: Option[T], runtime: Runtime) extends PureMust[T]
case class PureMustNone(runtime: Runtime) extends PureMust[Nothing]

case class Runtime(totalComputations: Int, totalComputationTime: Long)