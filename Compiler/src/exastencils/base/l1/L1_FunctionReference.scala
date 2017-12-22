package exastencils.base.l1

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.prettyprinting._

/// L1_FunctionReference

trait L1_FunctionReference extends L1_Node with L1_Progressable with PrettyPrintable {
  def name : String
  def returnType : L1_Datatype
  override def progress : L2_FunctionReference
}

/// L1_PlainFunctionReference

trait L1_PlainFunctionReference extends L1_FunctionReference {
  override def prettyprint(out : PpStream) = out << name
}

/// L1_LeveledFunctionReference

trait L1_LeveledFunctionReference extends L1_FunctionReference {
  def level : Int
  override def prettyprint(out : PpStream) = out << name << '@' << level
}

/// L1_UnresolvedFunctionReference

case class L1_UnresolvedFunctionReference(
    var name : String,
    var level : Option[L1_LevelSpecification]) extends L1_FunctionReference {

  override def returnType = L1_UnknownDatatype

  override def prettyprint(out : PpStream) = {
    out << name
    if (level.isDefined) out << '@' << level.get
  }

  override def progress = ProgressLocation(L2_UnresolvedFunctionReference(name, L1_ProgressOption(level)(_.progress), None))
}
