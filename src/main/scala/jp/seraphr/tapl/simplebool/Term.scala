package jp.seraphr.tapl.simplebool

trait Term

case class TmVar(n: Int) extends Term
case class TmAbs(nameHint: String, typ: Type, t: Term) extends Term
case class TmApp(t1: Term, t2: Term) extends Term
case object TmTrue extends Term
case object TmFalse extends Term
case class TmIf(cond: Term, trueTerm: Term, falseTerm: Term) extends Term