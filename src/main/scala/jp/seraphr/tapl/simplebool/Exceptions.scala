package jp.seraphr.tapl.simplebool

abstract class TypeException(aMes: String) extends Throwable(aMes)
class IfCondTypeException(aMes: String) extends TypeException(aMes)
class IfArmsTypeException(aMes: String) extends TypeException(aMes)
class ApplyTypeMismatchException(aMes: String) extends TypeException(aMes)