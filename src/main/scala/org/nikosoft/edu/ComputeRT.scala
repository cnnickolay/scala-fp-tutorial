package org.nikosoft.edu

import org.nikosoft.edu.ComputeRT.safeExec
import org.nikosoft.edu.Edu.Monad

import scala.util.Try

class ComputeRT[T](val compute: Compute[T]) extends Monad[T, ComputeRT] {
  override def map[K](t: T => K): ComputeRT[K] = safeExec {
    compute match {
      case Pure(value, runtime) => new ComputeRT[K](Pure(t(value), runtime.copy(runtime.totalComputations + 1)))
      case PureMustSome(Some(value), runtime) => new ComputeRT[K](PureMustSome(Some(t(value)), runtime.copy(runtime.totalComputations + 1)))
      case PureMustSome(None, runtime) => new ComputeRT[K](PureMustNone(runtime))
      case PureMustNone(runtime) => new ComputeRT[K](PureMustNone(runtime))
      case f: Failure => new ComputeRT(f)
    }
  }

  override def flatMap[K](f: T => ComputeRT[K]): ComputeRT[K] = safeExec {
    compute match {
      case Pure(value, runtime) => f(value)
      case PureMustSome(Some(value)) => f(value)
      case PureMustSome(None) => new ComputeRT[K](PureMustNone)
      case PureMustNone => new ComputeRT[K](PureMustNone)
      case f: Failure => new ComputeRT(f)
    }
  }
}

object ComputeRT {
  implicit class ComputeRTWrapSome[T, F[T1] <: Some[T1]](f: => F[T]) {
    def liftMust: ComputeRT[T] = new ComputeRT[T](PureMustSome(f))
  }
  implicit class ComputeRTWrap[T](f: => T) {
    def liftPure: ComputeRT[T] = new ComputeRT[T](Pure(f))
  }
  def safeExec[T](t: => ComputeRT[T]): ComputeRT[T] = Try(t) match {
    case scala.util.Success(v) => v
    case scala.util.Failure(t) => new ComputeRT(Failure(t))
  }
}

