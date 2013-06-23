package jp.seraphr.tapl.simplebool

import org.scalatest.FunSuite
import scala.util.Success
import scala.util.Failure

/**
 * 結構適当なテスト
 */
class EvaluatorTest extends FunSuite {
  import Evaluator._
  import LambdaCalcs._

  val e = (t: Term) => eval(Nil, t) match {
    case Success((t, v)) => v
    case Failure(e)      => throw e
  }
  case class LambdaEquals(aLeft: Term, aRight: Term)

  implicit class AssertUtil(aLeft: Term) {
    def ====(aRight: Term) = LambdaEquals(aLeft, aRight)
  }

  def assert(aEq: LambdaEquals): Unit = {
    val LambdaEquals(l, r) = aEq
    assert(e(equal ~ l ~ r) === tru)
  }

  //  test("test fugafuga") {
  //    val tOne = eval(Nil, equal ~ (plus ~ _1 ~ _3) ~ (times ~ _2 ~ _2))
  //
  //    tOne.foreach {
  //      case (t, v) => {
  //        println(s"type of v => ${t}")
  //        println(printTerm(v))
  //        println(v)
  //        println(tru)
  //        println(v == tru)
  //      }
  //    }
  //  }

  /**
   * 型レベルのみのテスト
   */
  test("types") {
    e(pair ~ _3 ~ _2)
    e(first ~ (pair ~ _3 ~ _2))
    e(second ~ (pair ~ _3 ~ _2))
  }

  //  test("test") {
  //    assert(ltest ~ tru ~ _3 ~ _4 ==== _3)
  //    assert(ltest ~ fls ~ _3 ~ _4 ==== _4)
  //    assert(ltest ~ (isZero ~ _0) ~ _3 ~ _4 ==== _3)
  //    assert(ltest ~ (isZero ~ _1) ~ _3 ~ _4 ==== _4)
  //  }
  //
  //  test("times") {
  //
  //    assert(times ~ _1 ~ _4 ==== _4)
  //    assert(times ~ _2 ~ _2 ==== _4)
  //    assert(times ~ _3 ~ _4 ==== times ~ _4 ~ _3)
  //
  //    assert(times ~ _0 ~ _4 ==== _0)
  //    assert(times ~ _2 ~ _0 ==== _0)
  //  }
  //
  //  test("pred") {
  //    assert(pred ~ _4 ==== _3)
  //    assert(pred ~ _3 ==== _2)
  //    assert(pred ~ _2 ==== _1)
  //    assert(pred ~ _1 ==== _0)
  //    assert(pred ~ _0 ==== _0)
  //  }
  //
  //  test("subtract") {
  //    assert(subtract ~ _4 ~ _3 ==== _1)
  //    assert(subtract ~ _4 ~ _4 ==== _0)
  //    assert(subtract ~ _3 ~ _4 ==== _0)
  //    assert(subtract ~ _3 ~ _0 ==== _3)
  //  }
  //
  //  test("nat") {
  //    assert(nat(2) ==== _2)
  //    assert(nat(3) ==== _3)
  //    assert(nat(4) ==== _4)
  //
  //    assert(nat(10) ==== plus ~ _5 ~ _5)
  //  }
}