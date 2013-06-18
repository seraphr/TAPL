package jp.seraphr.tapl.utils

import scala.util.Try
import scala.util.Success
import scala.util.Failure

object TryUtils {
  implicit class TryUtil[_E](aTry: Try[_E]) {
    def filterWithError(f: _E => Boolean, aException: _E => Throwable): Try[_E] = {
      aTry.flatMap(e => {
        if (f(e))
          aTry
        else
          Failure(aException(e))
      })
    }
  }
}