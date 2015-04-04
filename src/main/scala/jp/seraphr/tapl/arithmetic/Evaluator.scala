package jp.seraphr.tapl.arithmetic

import scala.annotation.tailrec

object Evaluator {
  def isNumericVal(aTerm: Term): Boolean = aTerm match {
    case TmZero => true
    case TmSucc(t) => isNumericVal(t)
    case _ => false
  }

  def isVal(aTerm: Term): Boolean = aTerm match {
    case TmTrue => true
    case TmFalse => true
    case t if isNumericVal(t) => true
    case _ => false
  }

  def eval1(aTerm: Term): Option[Term] = aTerm match {
    case TmIf(TmTrue, t2, t3) => Some(t2)
    case TmIf(TmFalse, t2, t3) => Some(t3)
    case TmIf(t1, t2, t3) => eval1(t1).map(TmIf(_, t2, t3))
    case TmSucc(t1) => eval1(t1).map(TmSucc(_))
    case TmPred(TmZero) => Some(TmZero)
    case TmPred(TmSucc(nv)) if isNumericVal(nv) => Some(nv)
    case TmPred(t1) => eval1(t1).map(TmPred(_))
    case TmIsZero(TmZero) => Some(TmTrue)
    case TmIsZero(TmSucc(nv)) if isNumericVal(nv) => Some(TmFalse)
    case TmIsZero(t1) => eval1(t1).map(TmIsZero(_))
    case _ => None
  }

  @tailrec
  def eval(aTerm: Term): Term = eval1(aTerm) match {
    case Some(t) => eval(t)
    case _ => aTerm
  }

  def evalAsString(aTerm: Term): Option[String] = valueToString(eval(aTerm))

  def valueToString(aTerm: Term): Option[String] = {
    def v2s(aTerm: Term): String = aTerm match {
      case TmTrue => "true"
      case TmFalse => "false"
      case _ => nv2i(aTerm, 0).toString
    }

    @tailrec
    def nv2i(aTerm: Term, aVal: Int): Int = aTerm match {
      case TmZero => aVal
      case TmSucc(t) => nv2i(t, aVal + 1)
      case _ => throw new RuntimeException("ここに到達してはなりません。 恐らくプログラマのミスです。")
    }

    if (isVal(aTerm)) Some(v2s(aTerm)) else None
  }
}