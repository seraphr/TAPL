package jp.seraphr.tapl.untyped

object LambdaCalcs {
  import FuncUtils._
  import Evaluator.mapVar

  class AppUtil(t1: Term) {
    def ~(t2: Term) = TmApp(t1, t2)
  }

  implicit def toApp(t1: Term) = new AppUtil(t1)

  //  def shiftForce(d: Int)(t: Term): Term = mapVar(t)((c, n) => TmVar(n + d))

  val abs: String => Term => TmAbs = s => t => TmAbs(s, t)
  //  val app: Term => Term => TmApp = t2 => t1 => TmApp(t1, t2)

  def lambda1(f: TmVar => Term): TmAbs = TmAbs("x", f(TmVar(0)))
  def lambda2(f: (TmVar, TmVar) => Term): TmAbs = {
    val tTerm = f(TmVar(1), TmVar(0))

    tTerm |> abs("y") |> abs("x")
  }

  def lambda3(f: (TmVar, TmVar, TmVar) => Term): TmAbs = {
    val tTerm = f(TmVar(2), TmVar(1), TmVar(0))

    tTerm |> abs("z") |> abs("y") |> abs("x")
  }

  def lambda4(f: (TmVar, TmVar, TmVar, TmVar) => Term): TmAbs = {
    val tTerm = f(TmVar(3), TmVar(2), TmVar(1), TmVar(0))

    tTerm |> abs("d") |> abs("c") |> abs("b") |> abs("a")
  }

  val tru = lambda2((a, b) => a)
  val fls = lambda2((a, b) => b)
  val test = lambda3((a, b, c) => a ~ b ~ c)
  val and = lambda2((l, r) => l ~ r ~ fls)
  val or = lambda2((l, r) => l ~ tru ~ r)
  val not = lambda1(_ ~ fls ~ tru)

  val pair = lambda3((f, s, b) => b ~ f ~ s)
  val first = lambda1(_ ~ tru)
  val second = lambda1(_ ~ fls)

  val _0 = lambda2((s, z) => z)
  val _1 = lambda2((s, z) => s ~ z)
  val _2 = lambda2((s, z) => s ~ (s ~ z))
  val _3 = lambda2((s, z) => s ~ (s ~ (s ~ z)))
  val _4 = lambda2((s, z) => s ~ (s ~ (s ~ (s ~ z))))

  val succ = lambda3((n, s, z) => s ~ (n ~ s ~ z))
  val plus = lambda4((m, n, s, z) => m ~ s ~ (n ~ s ~ z))
  val times = lambda2((m, n) => m ~ (plus ~ n) ~ _0)

  val isZero = lambda1(_ ~ lambda1(_ => fls) ~ tru)

  val pred = {
    val zz = pair ~ _0 ~ _0
    val ss = lambda1(p => pair ~ (second ~ p) ~ (plus ~ _1 ~ (second ~ p)))

    lambda1(m => first ~ (m ~ ss ~ zz))
  }

  val subtract = lambda2((m, n) => m ~ pred ~ n)

  val equal = lambda2((m, n) => and ~ (isZero ~ (subtract ~ m ~ n)) ~ (isZero ~ (subtract ~ n ~ m)))
}