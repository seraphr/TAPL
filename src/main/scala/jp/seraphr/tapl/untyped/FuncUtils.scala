package jp.seraphr.tapl.untyped

object FuncUtils {
  class SupportPipe[A](aArg: A) {
    def |>[B](f: A => B) = f(aArg)
  }

  implicit def toPipe[A](aArg: A) = new SupportPipe(aArg)
}