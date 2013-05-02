package jp.seraphr.tapl.untyped

import org.scalatest.FunSuite

/**
 * 結構適当なテスト
 */
class EvaluatorTest extends FunSuite {
  import Evaluator._
  import LambdaCalcs._

  test("test fugafuga") {
    val tOne = eval(Nil, equal ~ (plus ~ _1 ~ _3) ~ (times ~ _2 ~ _2))
    println(printTerm(tOne))
    println(tOne)
    println(tru)
    println(tOne == tru)
  }

  case class LambdaEquals(aLeft: Term, aRight: Term)

  class AssertUtil(aLeft: Term){
    def ====(aRight: Term) = LambdaEquals(aLeft, aRight)
  }

  implicit def toAssert(aLeft: Term) = new AssertUtil(aLeft)

  val e = (t: Term) => eval(Nil, t)
  def assert(aEq: LambdaEquals): Unit = {
    val LambdaEquals(l, r) = aEq
    assert(e(equal ~ l ~ r) === tru)
  }

  test("equal") {

    assert(e(equal ~ _4 ~ _4) === tru)
    assert(e(equal ~ _4 ~ _3) === fls)
    assert(e(equal ~ _0 ~ _0) === tru)
    assert(e(equal ~ _1 ~ _0) === fls)
  }

  test("times") {

    assert(times ~ _1 ~ _4 ==== _4)
    assert(times ~ _2 ~ _2 ==== _4)
    assert(times ~ _3 ~ _4 ==== times ~ _4 ~ _3)

    assert(times ~ _0 ~ _4 ==== _0)
    assert(times ~ _2 ~ _0 ==== _0)
  }
}