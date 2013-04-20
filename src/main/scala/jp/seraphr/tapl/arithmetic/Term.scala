package jp.seraphr.tapl.arithmetic

sealed trait Term;

case object TmTrue extends Term
case object TmFalse extends Term
case class TmIf(condTerm: Term, trueTerm: Term, falseTerm: Term) extends Term
case object TmZero extends Term
case class TmSucc(term: Term) extends Term
case class TmPred(term: Term) extends Term
case class TmIsZero(term: Term) extends Term