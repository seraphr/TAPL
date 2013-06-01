package jp.seraphr.tapl.untyped

object FuncUtils {
  implicit class SupportPipe[A](aArg: A) {
    def |>[B](f: A => B) = f(aArg)
  }
}