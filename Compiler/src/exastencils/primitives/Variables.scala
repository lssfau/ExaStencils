package exastencils.primitives

import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class FragCommMember(var name : String, var field : Field, var direction : String) extends Expression {
  override def cpp : String = resolveName

  def resolveName : String = {
    name match {
      case "reqOutstanding" => s"reqOutstanding_${direction}"
      case _                => s"UNRECOGNIZED VARIABLE name"
    }
  }

  def resolveDataType : Datatype = {
    name match {
      case "reqOutstanding" => new BooleanDatatype
      case _                => new UnitDatatype
    }
  }

  def resolveDefValue : Expression = {
    name match {
      case "reqOutstanding" => false
      case _                => 0
    }
  }
}

object VariableManager {

}