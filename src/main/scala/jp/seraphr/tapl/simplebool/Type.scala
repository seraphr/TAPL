package jp.seraphr.tapl.simplebool

trait Type
case object TyBool extends Type
case class TyArrow(cod: Type, dom: Type) extends Type