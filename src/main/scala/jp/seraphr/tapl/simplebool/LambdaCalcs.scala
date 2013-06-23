package jp.seraphr.tapl.simplebool

import scala.util.DynamicVariable
import scala.annotation.tailrec
import language.implicitConversions
import jp.seraphr.tapl.utils.FuncUtils

object LambdaBuilder {
  import FuncUtils._

  private val mVariables = new DynamicVariable[List[VarBox]](Nil)
  case class VarBox(var v: TmVar) {
    def shift = mapSelf(_ + 1)
    def unshift = mapSelf(_ - 1)
    def mapSelf(f: Int => Int) = {
      v = TmVar(f(v.n))
      this
    }
  }

  implicit def boxToVar(aBox: VarBox) = aBox.v

  def withArgs(aArgs: VarBox*)(f: => TmAbs): TmAbs = {
    try {
      mVariables.withValue(mVariables.value.map(_.shift) ++ aArgs)(f)
    } finally {
      mVariables.value.foreach(_.unshift)
    }
  }

  //  def shiftForce(d: Int)(t: Term): Term = mapVar(t)((c, n) => TmVar(n + d))

  val abs: String => Type => Term => TmAbs = s => typ => t => TmAbs(s, typ, t)
  //  val app: Term => Term => TmApp = t2 => t1 => TmApp(t1, t2)

  def lambda1(aArg1: (String, Type))(f: VarBox => Term): TmAbs = {
    val tVar = VarBox(TmVar(0))
    val (tArg1, tType1) = aArg1

    withArgs(tVar) {
      TmAbs(tArg1, tType1, f(tVar))
    }
  }

  def lambda1(aType1: Type)(f: VarBox => Term): TmAbs = lambda1(("x", aType1))(f)

  def lambda2(aArg1: (String, Type), aArg2: (String, Type))(f: (VarBox, VarBox) => Term): TmAbs = {
    val tVar1 = VarBox(TmVar(1))
    val tVar2 = VarBox(TmVar(0))
    val (tArg1, tType1) = aArg1
    val (tArg2, tType2) = aArg2

    withArgs(tVar1, tVar2) {
      val tTerm = f(tVar1, tVar2)
      tTerm |> abs(tArg2)(tType2) |> abs(tArg1)(tType1)
    }
  }
  def lambda2(aType1: Type, aType2: Type)(f: (VarBox, VarBox) => Term): TmAbs = lambda2(("x", aType1), ("y", aType2))(f)

  def lambda3(aArg1: (String, Type), aArg2: (String, Type), aArg3: (String, Type))(f: (VarBox, VarBox, VarBox) => Term): TmAbs = {
    val tVar1 = VarBox(TmVar(2))
    val tVar2 = VarBox(TmVar(1))
    val tVar3 = VarBox(TmVar(0))
    val (tArg1, tType1) = aArg1
    val (tArg2, tType2) = aArg2
    val (tArg3, tType3) = aArg3

    withArgs(tVar1, tVar2, tVar3) {
      val tTerm = f(tVar1, tVar2, tVar3)
      tTerm |> abs(tArg3)(tType3) |> abs(tArg2)(tType2) |> abs(tArg1)(tType1)
    }
  }
  def lambda3(aType1: Type, aType2: Type, aType3: Type)(f: (VarBox, VarBox, VarBox) => Term): TmAbs = lambda3(("x", aType1), ("y", aType2), ("z", aType3))(f)

  def lambda4(aArg1: (String, Type), aArg2: (String, Type), aArg3: (String, Type), aArg4: (String, Type))(f: (VarBox, VarBox, VarBox, VarBox) => Term): TmAbs = {
    val tVar1 = VarBox(TmVar(3))
    val tVar2 = VarBox(TmVar(2))
    val tVar3 = VarBox(TmVar(1))
    val tVar4 = VarBox(TmVar(0))
    val (tArg1, tType1) = aArg1
    val (tArg2, tType2) = aArg2
    val (tArg3, tType3) = aArg3
    val (tArg4, tType4) = aArg4

    withArgs(tVar1, tVar2, tVar3, tVar4) {
      val tTerm = f(tVar1, tVar2, tVar3, tVar4)
      tTerm |> abs(tArg4)(tType4) |> abs(tArg3)(tType3) |> abs(tArg2)(tType2) |> abs(tArg1)(tType1)
    }
  }
  def lambda4(aType1: Type, aType2: Type, aType3: Type, aType4: Type)(f: (VarBox, VarBox, VarBox, VarBox) => Term): TmAbs = lambda4(("a", aType1), ("b", aType2), ("c", aType3), ("d", aType4))(f)
}

object LambdaCalcs {
  import Evaluator.mapVar
  import LambdaBuilder._
  import scala.collection.mutable
  import TypeBuildableInstances._
  type Bool = TyBool.type

  class AppUtil(t1: Term) {
    def ~(t2: Term) = TmApp(t1, t2)
  }

  implicit def toApp(t1: Term) = new AppUtil(t1)
  implicit def toApp(t1: VarBox) = new AppUtil(t1.v)

  private val valueMapBuilder = Map.newBuilder[Term, String]

  val tru = TmTrue
  val fls = TmFalse
  val ltru = lambda2(t[NatType], t[NatType])((a, b) => a)
  val lfls = lambda2(t[NatType], t[NatType])((a, b) => b)

  val _if: (Term, Term, Term) => TmIf = TmIf(_, _, _)
  val ltest = lambda3(t[NatType => NatType => NatType], t[NatType], t[NatType])((a, b, c) => a ~ b ~ c)
  val and = lambda2(TyBool, TyBool)((l, r) => _if(l, r, TmFalse))
  val or = lambda2(TyBool, TyBool)((l, r) => _if(l, TmTrue, r))
  val not = lambda1(TyBool)(b => _if(b, TmTrue, TmFalse))

  /**
   * first secondを与えられたあとの、数値ペアの型
   */
  type PairTypeFunc = (NatType => NatType => NatType) => NatType
  val pair = lambda3(t[NatType], t[NatType], t[NatType => NatType => NatType])((f, s, b) => b ~ f ~ s)
  val first = lambda1(t[PairTypeFunc])(_ ~ ltru)
  val second = lambda1(t[PairTypeFunc])(_ ~ lfls)

  type NatType = (Bool => Bool) => Bool => Bool
  private val lambdaSZ = lambda2(("s", t[Bool => Bool]), ("z", TyBool)) _
  val _0 = lambdaSZ((s, z) => z)
  val _1 = lambdaSZ((s, z) => s ~ z)
  val _2 = lambdaSZ((s, z) => s ~ (s ~ z))
  val _3 = lambdaSZ((s, z) => s ~ (s ~ (s ~ z)))
  val _4 = lambdaSZ((s, z) => s ~ (s ~ (s ~ (s ~ z))))
  val _5 = lambdaSZ((s, z) => s ~ (s ~ (s ~ (s ~ (s ~ z)))))
  val _6 = lambdaSZ((s, z) => s ~ (s ~ (s ~ (s ~ (s ~ (s ~ z))))))
  val _7 = lambdaSZ((s, z) => s ~ (s ~ (s ~ (s ~ (s ~ (s ~ (s ~ z)))))))
  val _8 = lambdaSZ((s, z) => s ~ (s ~ (s ~ (s ~ (s ~ (s ~ (s ~ (s ~ z))))))))
  val _9 = lambdaSZ((s, z) => s ~ (s ~ (s ~ (s ~ (s ~ (s ~ (s ~ (s ~ (s ~ z)))))))))

  def nat(n: Int): Term = {
    @tailrec
    def inner(nn: Int, aResult: Term): Term = nn match {
      case 0 => aResult
      //      case 1 => plus ~ aResult ~ _1
      //      case 2 => plus ~ aResult ~ _2
      //      case 3 => plus ~ aResult ~ _3
      //      case 4 => plus ~ aResult ~ _4
      //      case 5 => plus ~ aResult ~ _5
      //      case 6 => plus ~ aResult ~ _6
      //      case 7 => plus ~ aResult ~ _7
      //      case 8 => plus ~ aResult ~ _8
      //      case 9 => plus ~ aResult ~ _9
      case _ => inner(nn - 1, succ ~ aResult)
    }

    n match {
      case 0 => _0
      case 1 => _1
      case 2 => _2
      case 3 => _3
      case 4 => _4
      case 5 => _5
      case 6 => _6
      case 7 => _7
      case 8 => _8
      case 9 => _9
      case _ => inner(n, _0)
    }
  }

  val succ = lambda3(t[NatType], t[Bool => Bool], TyBool)((n, s, z) => s ~ (n ~ s ~ z))
  val plus = lambda4(t[NatType], t[NatType], t[Bool => Bool], TyBool)((m, n, s, z) => m ~ s ~ (n ~ s ~ z))
  val times = lambda4(("m", t[NatType]), ("n", t[NatType]), ("s", t[Bool => Bool]), ("z", TyBool))((m, n, s, z) => m ~ (n ~ s) ~ z)

  val isZero = lambda1(("n", t[NatType]))(_ ~ lambda1("_", TyBool)(_ => fls) ~ tru)

  val pred = {
    val zz = pair ~ _0 ~ _0
    val ss = lambda1("p", t[PairTypeFunc])(p => pair ~ (second ~ p) ~ (plus ~ _1 ~ (second ~ p)))

    lambda1(t[NatType])(m => first ~ (m ~ ss ~ zz))
  }

  val subtract = lambda2(t[NatType], t[NatType])((m, n) => n ~ pred ~ m)
  val equal = lambda2(t[NatType], t[NatType])((m, n) => and ~ (isZero ~ (subtract ~ m ~ n)) ~ (isZero ~ (subtract ~ n ~ m)))

  valueMapBuilder += ((ltru, "true"))
  valueMapBuilder += ((lfls, "false"))
  valueMapBuilder += ((_0, "0"))
  valueMapBuilder += ((_1, "1"))
  valueMapBuilder += ((_2, "2"))
  valueMapBuilder += ((_3, "3"))
  valueMapBuilder += ((_4, "4"))
  valueMapBuilder += ((isZero, "isZero"))
  valueMapBuilder += ((times, "times"))
  valueMapBuilder += ((succ, "succ"))

  val valueMap = valueMapBuilder.result
}