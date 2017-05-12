package exastencils.runner

import exastencils.core.UniversalSetter
import exastencils.util._

/// DerivedValue

abstract class DerivedParameter {
  def print() : String
  def apply() : Unit
}

/// DerivedValueWithAssign

case class DerivedParameterWithAssign(var name : String, var value : Any) extends DerivedParameter {
  def print() : String = name + " = " + value
  override def apply() : Unit = UniversalSetter(ResolveConfigCollection(name), name, resolveValue())

  def resolveValue() = {
    value match {
      case code : CodeWrapper => code.eval[Any]()
      case other              => other
    }
  }
}

/// DerivedValueWithAppend

case class DerivedParameterWithAppend(var name : String, var value : Any) extends DerivedParameter {
  def print() : String = name + " += " + value
  override def apply() : Unit = UniversalSetter(ResolveConfigCollection(name), name, resolveValue())

  def resolveValue() = {
    value match {
      case code : CodeWrapper => code.eval[Any]()
      case other              => other
    }
  }
}
