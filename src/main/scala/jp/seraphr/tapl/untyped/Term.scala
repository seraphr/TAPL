package jp.seraphr.tapl.untyped

trait Term

case class TmVar(n: Int) extends Term
case class TmAbs(nameHint: String, t: Term) extends Term
case class TmApp(t1: Term, t2: Term) extends Term