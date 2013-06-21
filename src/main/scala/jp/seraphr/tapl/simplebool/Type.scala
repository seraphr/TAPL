package jp.seraphr.tapl.simplebool

trait Type
case object TyBool extends Type
case class TyArrow(cod: Type, dom: Type) extends Type

trait TypeBuildable[_T]{
  def build: Type
}

object TypeBuildableInstances {
  def t[B: TypeBuildable] = implicitly[TypeBuildable[B]].build

  implicit def functionIsArrowBuildable[IN, OUT](implicit in: TypeBuildable[IN], out: TypeBuildable[OUT]): TypeBuildable[Function[IN, OUT]] = new TypeBuildable[Function[IN, OUT]]{
    def build: Type = TyArrow(in.build, out.build)
  }

  implicit object BoolIsTypeBuildable extends TypeBuildable[TyBool.type]{
    def build: Type = TyBool
  }
}