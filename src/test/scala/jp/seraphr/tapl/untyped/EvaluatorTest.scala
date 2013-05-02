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
}