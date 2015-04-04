package jp.seraphr.tapl.arithmetic

import org.scalatest.FunSuite

/**
 * 結構適当なテスト
 */
class EvaluatorTest extends FunSuite {
  import Evaluator._

  test("isNumericVal") {
    assert(isNumericVal(TmZero) === true)
    assert(isNumericVal(TmTrue) === false)
    assert(isNumericVal(TmFalse) === false)
    assert(isNumericVal(TmIf(TmTrue, TmTrue, TmFalse)) === false)

    assert(isNumericVal(TmSucc(TmZero)) === true)
    assert(isNumericVal(TmSucc(TmSucc(TmZero))) === true)
    assert(isNumericVal(TmSucc(TmPred(TmZero))) === false)
    assert(isNumericVal(TmSucc(TmTrue)) === false)
    assert(isNumericVal(TmSucc(TmFalse)) === false)

    assert(isNumericVal(TmPred(TmZero)) === false)
    assert(isNumericVal(TmPred(TmPred(TmZero))) === false)
    assert(isNumericVal(TmPred(TmTrue)) === false)
    assert(isNumericVal(TmPred(TmFalse)) === false)

    assert(isNumericVal(TmIsZero(TmZero)) === false)
    assert(isNumericVal(TmIsZero(TmTrue)) === false)
  }

  test("isVal") {
    assert(isVal(TmZero) === true)
    assert(isVal(TmTrue) === true)
    assert(isVal(TmFalse) === true)
    assert(isVal(TmSucc(TmZero)) === true)
    assert(isVal(TmSucc(TmSucc(TmZero))) === true)

    assert(isVal(TmIf(TmTrue, TmTrue, TmFalse)) === false)
    assert(isVal(TmSucc(TmPred(TmZero))) === false)
    assert(isVal(TmSucc(TmTrue)) === false)
    assert(isVal(TmSucc(TmFalse)) === false)

    assert(isVal(TmPred(TmZero)) === false)
    assert(isVal(TmPred(TmPred(TmZero))) === false)
    assert(isVal(TmPred(TmTrue)) === false)
    assert(isVal(TmPred(TmFalse)) === false)

    assert(isVal(TmIsZero(TmZero)) === false)
    assert(isVal(TmIsZero(TmTrue)) === false)
  }

  test("eval") {
    assert(evalAsString(z) === Some("0"))
    assert(evalAsString(s(z)) === Some("1"))
    assert(evalAsString(s(s(z))) === Some("2"))
    assert(evalAsString(p(z)) === Some("0"))
    assert(evalAsString(p(p(z))) === Some("0"))
    assert(evalAsString(s(p(p(z)))) === Some("1"))
    assert(evalAsString(p(p(s(s(s(z)))))) === Some("1"))

    assert(evalAsString(t) === Some("true"))
    assert(evalAsString(f) === Some("false"))
    assert(evalAsString(is(z)) === Some("true"))

    assert(evalAsString(if_(t, t, f)) === Some("true"))
    assert(evalAsString(if_(f, t, f)) === Some("false"))

    assert(evalAsString(if_(t, s(s(z)), s(z))) === Some("2"))
    assert(evalAsString(if_(f, s(s(z)), s(z))) === Some("1"))

    assert(evalAsString(if_(is(z), s(s(z)), s(z))) === Some("2"))
    assert(evalAsString(if_(is(s(z)), s(s(z)), s(z))) === Some("1"))

    val tTrue = if_(is(z), t, f)
    val tFalse = if_(is(s(z)), t, f)
    assert(evalAsString(if_(tTrue, s(s(z)), s(z))) === Some("2"))
    assert(evalAsString(if_(tFalse, s(s(z)), s(z))) === Some("1"))

    assert(evalAsString(is(t)) === None)
    assert(evalAsString(s(t)) === None)
    assert(evalAsString(p(t)) === None)
    assert(evalAsString(if_(z, t, f)) === None)

  }

  def s(aTerm: Term) = TmSucc(aTerm)
  def p(aTerm: Term) = TmPred(aTerm)
  val z = TmZero
  val t = TmTrue
  val f = TmFalse
  def if_(t1: Term, t2: Term, t3: Term) = TmIf(t1, t2, t3)
  def is(t1: Term) = TmIsZero(t1)
}