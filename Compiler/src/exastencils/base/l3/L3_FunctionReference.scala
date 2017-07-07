package exastencils.base.l3

import exastencils.base.l4._
import exastencils.prettyprinting._

/// L3_FunctionReference

trait L3_FunctionReference extends L3_Node with L3_Progressable with PrettyPrintable {
  def name : String
  def returnType : L3_Datatype
  override def progress : L4_FunctionReference
}

/// L3_PlainFunctionReference

trait L3_PlainFunctionReference extends L3_FunctionReference {
  override def prettyprint(out : PpStream) = out << name
}

/// L3_LeveledFunctionReference

trait L3_LeveledFunctionReference extends L3_FunctionReference {
  def level : Int
  override def prettyprint(out : PpStream) = out << name << '@' << level
}

/// L3_UnresolvedFunctionReference

case class L3_UnresolvedFunctionReference(var name : String, var level : Option[L3_LevelSpecification]) extends L3_FunctionReference {
  override def returnType = L3_UnknownDatatype

  override def prettyprint(out : PpStream) = {
    out << name
    if (level.isDefined) out << '@' << level.get
  }

  override def progress = L4_UnresolvedFunctionReference(name, L3_ProgressOption(level)(_.progress))
}
