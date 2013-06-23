package jp.seraphr.tapl.simplebool

import scala.annotation.tailrec
import jp.seraphr.tapl.utils._
import com.sun.org.apache.bcel.internal.generic.ACONST_NULL
import scala.util.Try
import scala.util.Success

object Evaluator extends SimpleTypedContext {
  def mapVar(t: Term)(f: (Int, Int) => Term): Term = {
    def walk(c: Int, t: Term): Term = t match {
      case TmVar(n)              => f(c, n)
      case TmAbs(tHint, typ, t1) => TmAbs(tHint, typ, walk(c + 1, t1))
      case TmApp(t1, t2)         => TmApp(walk(c, t1), walk(c, t2))
      case TmTrue                => t
      case TmFalse               => t
      case TmIf(t1, t2, t3)      => TmIf(walk(c, t1), walk(c, t2), walk(c, t3))
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
    case TmTrue         => true
    case TmFalse        => true
    case TmAbs(_, _, _) => true
    case _              => false
  }

  def eval1(aContext: Context, t: Term): Option[Term] = {
    //    println(printTerm(t))
    //    println(printTermDeBruijn(t))

    t match {
      case TmApp(TmAbs(tHint, _, t12), v2) if isVal(aContext, v2) =>
        Some(substitutionTop(v2, t12))
      case TmApp(v1, t2) if isVal(aContext, v1) =>
        eval1(aContext, t2).map(TmApp(v1, _))
      case TmApp(t1, t2) => {
        eval1(aContext, t1).map(TmApp(_, t2))
      }
      case TmIf(TmTrue, t2, t3)  => Some(t2)
      case TmIf(TmFalse, t2, t3) => Some(t3)
      case TmIf(t1, t2, t3) => {
        eval1(aContext, t1).map(TmIf(_, t2, t3))
      }
      case _ => None
    }
  }

  def eval(aContext: Context, aTerm: Term): Try[(Type, Term)] = {
    @tailrec
    def innerEval(aContext: Context, aInnerTerm: Term /*, aPastTerms: List[Term] = Nil*/ ): Term = {
      eval1(aContext, aInnerTerm) match {
        case Some(t) =>
          /*println(printTerm(t, LambdaCalcs.valueMap));*/ innerEval(aContext, t /*, aPastTerms*/ )
        case _ => aInnerTerm
      }
    }

    typeOf(aContext, aTerm).map(t => (t, innerEval(aContext, aTerm)))
  }

  def printTerm(aTerm: Term, aValueMap: Map[TmAbs, String] = Map()): String = {
    def printTermInner(aTermInner: Term, aContext: Context, aOuter: Boolean): String = aTermInner match {
      case t @ TmAbs(_, _, _) if aValueMap.contains(t) => aValueMap(t)
      case TmAbs(tHint, _, tTerm) => {
        val (tCnt, tName) = pickFreshName(aContext, tHint)
        val tTemplate = if (aOuter) "(lambda %s.%s)" else "lambda %s.%s"

        tTemplate.format(tName, printTermInner(tTerm, tCnt, false))
      }
      case TmApp(t1, t2 @ TmApp(_, _)) => "%s (%s)".format(printTermInner(t1, aContext, true), printTermInner(t2, aContext, true))
      case TmApp(t1, t2)               => "%s %s".format(printTermInner(t1, aContext, true), printTermInner(t2, aContext, true))
      case TmVar(n)                    => indexToName(aContext, n)
    }

    printTermInner(aTerm, Nil, false)
  }

  def printTermDeBruijn(aTerm: Term): String = {
    def printTermDeBruijnInner(aTermInner: Term, aOuter: Boolean): String = aTermInner match {
      case TmAbs(tHint, _, tTerm) => {
        val tTemplate = if (aOuter) "(lambda.%s)" else "lambda.%s"
        tTemplate.format(printTermDeBruijnInner(tTerm, false))
      }
      case TmApp(t1, t2 @ TmApp(_, _)) => "%s (%s)".format(printTermDeBruijnInner(t1, true), printTermDeBruijnInner(t2, true))
      case TmApp(t1, t2)               => "%s %s".format(printTermDeBruijnInner(t1, true), printTermDeBruijnInner(t2, true))
      case TmVar(n)                    => n.toString
    }

    printTermDeBruijnInner(aTerm, false)
  }

  def pickFreshName(aContext: Context, aHint: String): (Context, String) = {
    if (isNameBound(aContext, aHint))
      pickFreshName(aContext, aHint + "'")
    else
      (addBinding(aContext, aHint, NameBind), aHint)
  }

  def isNameBound(aContext: Context, aName: String): Boolean = aContext match {
    case Nil                                => false
    case (tName, _) :: xs if tName == aName => true
    case (tName, _) :: xs                   => isNameBound(xs, aName)
  }

  def indexToName(aContext: Context, aIndex: Int): String = {
    val (tName, _) = aContext(aIndex)
    tName
  }

  def getTypeFromContext(aContext: Context, i: Int): Type = getBinding(aContext, i) match {
    case VarBind(typ) => typ
    case _            => throw new RuntimeException(s"与えられた変数(${i} : ${indexToName(aContext, i)})のBindingはVarBindではありません。")
  }

  def getBinding(aContext: Context, i: Int): Binding = aContext(i)._2

  import jp.seraphr.tapl.utils.Applicative._
  import jp.seraphr.tapl.utils.ApplicativeInstances._
  import jp.seraphr.tapl.utils.TryUtils._

  def typeOf(aContext: Context, aTerm: Term): Try[Type] = aTerm match {
    case TmVar(n) => Try { getTypeFromContext(aContext, n) }
    case TmAbs(name, typ, t) => {
      val tNewContext = addBinding(aContext, name, VarBind(typ))
      val tDomType = typeOf(tNewContext, t)

      tDomType.map(TyArrow(typ, _))
    }
    case TmApp(t1, t2) => {
      val tType1 = typeOf(aContext, t1)
      val tType2 = typeOf(aContext, t2)

      (tType1, tType2) {
        case (TyArrow(cod, dom), arg) if cod == arg => dom
        case (func @ TyArrow(cod, dom), arg)        => throw new RuntimeException(s"parameter type mismatch. ${arg} is applied to ${func}")
        case (func, _)                                      => throw new ApplyTypeMismatchException(s"find ${func} but arrow type expected")
      }
    }
    case TmTrue  => Success(TyBool)
    case TmFalse => Success(TyBool)
    case TmIf(cond, tru, fls) => {
      for {
        tTypeOfCond <- typeOf(aContext, cond).filterWithError(_ == TyBool, e => new IfCondTypeException(s"条件部の型(=${e})はbooleanである必要があります。"))
        tTypeOfTTerm <- typeOf(aContext, tru)
        tTypeOfFTerm <- typeOf(aContext, fls).filterWithError(_ == tTypeOfTTerm, e => new IfArmsTypeException(s"true部(=${tTypeOfTTerm})、false部(=${e})の型が一致しません。"))
      } yield tTypeOfTTerm
    }
  }
}