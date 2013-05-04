package jp.seraphr.tapl.untyped

import org.scalatest.FunSuite

/**
 * 結構適当なテスト
 */
class EvaluatorTest extends FunSuite {
  import Evaluator._
  import LambdaCalcs._

  val e = (t: Term) => eval(Nil, t)
  case class LambdaEquals(aLeft: Term, aRight: Term)

  class AssertUtil(aLeft: Term) {
    def ====(aRight: Term) = LambdaEquals(aLeft, aRight)
  }

  def assert(aEq: LambdaEquals): Unit = {
    val LambdaEquals(l, r) = aEq
    assert(e(equal ~ l ~ r) === tru)
  }

  implicit def toAssert(aLeft: Term) = new AssertUtil(aLeft)

    test("test fugafuga") {
      val tOne = eval(Nil, equal ~ (plus ~ _1 ~ _3) ~ (times ~ _2 ~ _2))
      println(printTerm(tOne))
      println(tOne)
      println(tru)
      println(tOne == tru)
    }

    test("equal") {
      assert(e(equal ~ _4 ~ _4) === tru)
      assert(e(equal ~ _4 ~ _3) === fls)
      assert(e(equal ~ _0 ~ _0) === tru)
      assert(e(equal ~ _1 ~ _0) === fls)
    }

    test("if"){
      assert(_if ~ tru ~ _3 ~ _4 ==== _3)
      assert(_if ~ fls ~ _3 ~ _4 ==== _4)
      assert(_if ~ (isZero ~ _0) ~ _3 ~ _4 ==== _3)
      assert(_if ~ (isZero ~ _1) ~ _3 ~ _4 ==== _4)
    }

    test("times") {

      assert(times ~ _1 ~ _4 ==== _4)
      assert(times ~ _2 ~ _2 ==== _4)
      assert(times ~ _3 ~ _4 ==== times ~ _4 ~ _3)

      assert(times ~ _0 ~ _4 ==== _0)
      assert(times ~ _2 ~ _0 ==== _0)
    }

    test("pred") {
      assert(pred ~ _4 ==== _3)
      assert(pred ~ _3 ==== _2)
      assert(pred ~ _2 ==== _1)
      assert(pred ~ _1 ==== _0)
      assert(pred ~ _0 ==== _0)
    }

    test("subtract"){
      assert(subtract ~ _4 ~ _3 ==== _1)
      assert(subtract ~ _4 ~ _4 ==== _0)
      assert(subtract ~ _3 ~ _4 ==== _0)
      assert(subtract ~ _3 ~ _0 ==== _3)
    }

  test("nat"){
    assert(nat(2) ==== _2)
    assert(nat(3) ==== _3)
    assert(nat(4) ==== _4)

    assert(nat(10) ==== plus ~ _5 ~ _5)
  }

  test("factorial") {
    assert(factorial ~ _4 ==== times ~ _4 ~ (times ~ _3 ~ (times ~ _2 ~ _1)))
//    assert(factorial ~ _5 ==== times ~ _5 ~ (times ~ _4 ~ (times ~ _3 ~ (times ~ _2 ~ _1))))

    // 6の階乗は実行に20分くらいかかった
//    assert(factorial ~ _6 ==== times ~ _6 ~ (times ~ _5 ~ (times ~ _4 ~ (times ~ _3 ~ (times ~ _2 ~ _1)))))

  }
}