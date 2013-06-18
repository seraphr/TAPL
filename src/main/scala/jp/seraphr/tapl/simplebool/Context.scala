package jp.seraphr.tapl.simplebool

trait SimpleTypedContext {

  trait Binding
  case object NameBind extends Binding
  case class VarBind(typ: Type) extends Binding

  type Context = List[(String, Binding)]

  def addBinding(aContext: Context, aName: String, aBind: Binding) = (aName, aBind) :: aContext

}