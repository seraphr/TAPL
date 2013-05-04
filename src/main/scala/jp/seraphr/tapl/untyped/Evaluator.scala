package jp.seraphr.tapl.untyped

import scala.annotation.tailrec

object Evaluator extends UntypedContext {
  def mapVar(t: Term)(f: (Int, Int) => Term): Term = {
    def walk(c: Int, t: Term): Term = t match {
      case TmVar(n) => f(c, n)
      case TmAbs(tHint, t1) => TmAbs(tHint, walk(c + 1, t1))
      case TmApp(t1, t2) => TmApp(walk(c, t1), walk(c, t2))
    }

    walk(0, t)
  }

  def shiftTerm(d: Int)(t: Term): Term = {
    mapVar(t) { (c, n) =>
      if (n >= c)
        TmVar(n + d)
      else
        TmVar(n)
    }
    //    def walk(c: Int, t: Term): Term = t match {
    //      case TmVar(n) => {
    //        if (n >= c)
    //          TmVar(n + d)
    //        else
    //          TmVar(n)
    //      }
    //      case TmAbs(tHint, t1) => TmAbs(tHint, walk(c + 1, t1))
    //      case TmApp(t1, t2) => TmApp(walk(c, t1), walk(c, t2))
    //    }
    //
    //    walk(0, t)
  }

  /**
   * @param j 代入対象のt内の変数番号
   * @param s 代入する項
   * @param t 代入される項
   */
  def substitution(j: Int, s: Term, t: Term): Term = {
    mapVar(t) { (c, n) =>
      if (n == j + c) shiftTerm(c)(s) else TmVar(n)
    }

    //    def walk(c: Int, t: Term): Term = t match {
    //      case TmVar(n) => if (n == j + c) shiftTerm(c)(s) else TmVar(n)
    //      case TmAbs(tHint, t1) => TmAbs(tHint, walk(c + 1, t1))
    //      case TmApp(t1, t2) => TmApp(walk(c, t1), walk(c, t2))
    //    }
    //
    //    walk(0, t)
  }

  def add(a: Int, b: Int) = a + b

  /**
   * 第一引数への代入 = ベータ簡約
   * sをtのTopに対して代入
   *
   * @param s 代入する項
   * @param t 代入される項
   */
  def substitutionTop(s: Term, t: Term): Term = {
    import FuncUtils._

    val tShiftMinus = shiftTerm(-1) _
    val tShiftPlus = shiftTerm(1) _
    val tSubstTop: Term => Term = substitution(0, _, t)

    s |> tShiftPlus |> tSubstTop |> tShiftMinus
  }

  def isVal(aContext: Context, t: Term) = t match {
    case TmAbs(_, _) => true
    case _ => false
  }

  def eval1(aContext: Context, t: Term): Option[Term] = {
    //    println(printTerm(t))
    //    println(printTermDeBruijn(t))

    t match {
      case TmApp(TmAbs(tHint, t12), v2) if isVal(aContext, v2) =>
        Some(substitutionTop(v2, t12))
      case TmApp(v1, t2) if isVal(aContext, v1) =>
        eval1(aContext, t2).map(TmApp(v1, _))
      case TmApp(t1, t2) => {
        eval1(aContext, t1).map(TmApp(_, t2))
      }
      case _ => None
    }
  }

  def eval(aContext: Context, aTerm: Term): Term = {
    @tailrec
    def innerEval(aContext: Context, aInnerTerm: Term/*, aPastTerms: List[Term] = Nil*/): Term = {
      eval1(aContext, aInnerTerm) match {
        case Some(t) =>
          /*println(printTerm(t, LambdaCalcs.valueMap));*/ innerEval(aContext, t/*, aPastTerms*/)
        case _ => aInnerTerm
      }
    }

    innerEval(aContext, aTerm)
  }

  def printTerm(aTerm: Term, aValueMap: Map[TmAbs, String] = Map()): String = {
    def printTermInner(aTermInner: Term, aContext: Context, aOuter: Boolean): String = aTermInner match {
      case t @ TmAbs(_, _) if aValueMap.contains(t) => aValueMap(t)
      case TmAbs(tHint, tTerm) => {
        val (tCnt, tName) = pickFreshName(aContext, tHint)
        val tTemplate = if (aOuter) "(lambda %s.%s)" else "lambda %s.%s"

        tTemplate.format(tName, printTermInner(tTerm, tCnt, false))
      }
      case TmApp(t1, t2 @ TmApp(_, _)) => "%s (%s)".format(printTermInner(t1, aContext, true), printTermInner(t2, aContext, true))
      case TmApp(t1, t2) => "%s %s".format(printTermInner(t1, aContext, true), printTermInner(t2, aContext, true))
      case TmVar(n) => indexToName(aContext, n)
    }

    printTermInner(aTerm, Nil, false)
  }

  def printTermDeBruijn(aTerm: Term): String = {
    def printTermDeBruijnInner(aTermInner: Term, aOuter: Boolean): String = aTermInner match {
      case TmAbs(tHint, tTerm) => {
        val tTemplate = if (aOuter) "(lambda.%s)" else "lambda.%s"
        tTemplate.format(printTermDeBruijnInner(tTerm, false))
      }
      case TmApp(t1, t2 @ TmApp(_, _)) => "%s (%s)".format(printTermDeBruijnInner(t1, true), printTermDeBruijnInner(t2, true))
      case TmApp(t1, t2) => "%s %s".format(printTermDeBruijnInner(t1, true), printTermDeBruijnInner(t2, true))
      case TmVar(n) => n.toString
    }

    printTermDeBruijnInner(aTerm, false)
  }

  def pickFreshName(aContext: Context, aHint: String): (Context, String) = {
    if (isNameBound(aContext, aHint))
      pickFreshName(aContext, aHint + "'")
    else
      ((aHint, NameBind) :: aContext, aHint)
  }

  def isNameBound(aContext: Context, aName: String): Boolean = aContext match {
    case Nil => false
    case (tName, _) :: xs if tName == aName => true
    case (tName, _) :: xs => isNameBound(xs, aName)
  }

  def indexToName(aContext: Context, aIndex: Int): String = {
    val (tName, _) = aContext(aIndex)
    tName
  }
}