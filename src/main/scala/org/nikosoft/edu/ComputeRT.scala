package org.nikosoft.edu

import org.nikosoft.edu.ComputeRT.{RuntimeM, safeExec}
import org.nikosoft.edu.Edu.Monad

import scala.util.Try

class ComputeRT[T](val compute: Compute[T], val rt: RuntimeM) extends Monad[T, ComputeRT] {
  override def map[K](t: T => K): ComputeRT[K] = safeExec {
    compute match {
      case Pure(value) => new ComputeRT[K](Pure(t(value)), rt)
      case PureMustSome(Some(value)) => new ComputeRT[K](PureMustSome(Some(t(value))), rt)
      case PureMustSome(None) => new ComputeRT[K](PureMustNone, rt)
      case PureMustNone => new ComputeRT[K](PureMustNone, rt)
      case f: Failure => new ComputeRT(f, rt)
      case err: Error => new ComputeRT(err, rt)
      case err: Errors => new ComputeRT(err, rt)
    }
  }

  override def flatMap[K](f: T => ComputeRT[K]): ComputeRT[K] = safeExec {
    compute match {
      case Pure(value) => f(value)
      case PureMustSome(Some(value)) => f(value)
      case PureMustSome(None) => new ComputeRT[K](PureMustNone, this.rt)
      case PureMustNone => new ComputeRT[K](PureMustNone, this.rt)
      case f: Failure => new ComputeRT(f, rt)
      case err: Error => new ComputeRT(err, rt)
      case err: Errors => new ComputeRT(err, rt)
    }
  }
}

object ComputeRT {
  case class RuntimeM(totalComputations: Int, totalComputationTime: Long)
  val emptyRtM: RuntimeM = RuntimeM(0, 0)
  implicit class ComputeRTWrapSome[T, F[T1] <: Option[T1]](f: => F[T]) {
    def liftMust: ComputeRT[T] = new ComputeRT[T](PureMustSome(f), emptyRtM)
  }
  implicit class ComputeRTWrap[T](f: => T) {
    def liftPure: ComputeRT[T] = new ComputeRT[T](Pure(f), emptyRtM)
  }
  implicit class ComputeRTError[T](f: => String) {
    def error: ComputeRT[T] = new ComputeRT[T](Error(f), emptyRtM)
  }
  def safeExec[T](t: => ComputeRT[T]): ComputeRT[T] = Try(t) match {
    case scala.util.Success(v) => v
    case scala.util.Failure(t) => new ComputeRT(Failure(t), emptyRtM)
  }
}

