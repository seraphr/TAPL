package jp.seraphr.tapl.simplebool

import org.scalatest.FunSuite

class TypeBuildableTest extends FunSuite {
  import TypeBuildableInstances._
  type Bool = TyBool.type

  test("test TypeBuildables") {
    assert(t[Bool] === TyBool)
    assert(t[Bool => Bool] === TyArrow(TyBool, TyBool))
    assert(t[Bool => Bool => Bool] === TyArrow(TyBool, TyArrow(TyBool, TyBool)))
    assert(t[(Bool => Bool) => Bool] === TyArrow(TyArrow(TyBool, TyBool), TyBool))
  }
}