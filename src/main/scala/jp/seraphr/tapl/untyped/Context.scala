package jp.seraphr.tapl.untyped

trait UntypedContext {
  trait Binding
  case object NameBind extends Binding

  type Context = List[(String, Binding)]

}