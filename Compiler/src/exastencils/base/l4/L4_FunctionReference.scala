package exastencils.base.l4

import exastencils.base.ir._
import exastencils.hack.ir.HACK_IR_UndeterminedFunctionReference
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_FunctionReference

trait L4_FunctionReference extends L4_Node with L4_Progressable with PrettyPrintable {
  def name : String
  def returnType : L4_Datatype
  override def progress : IR_FunctionReference
}

/// L4_PlainFunctionReference

trait L4_PlainFunctionReference extends L4_FunctionReference {
  override def prettyprint(out : PpStream) = out << name
}

/// L4_LeveledFunctionReference

trait L4_LeveledFunctionReference extends L4_FunctionReference {
  def level : Int
  override def prettyprint(out : PpStream) = out << name << '@' << level
}

/// L4_UnresolvedFunctionReference

case class L4_UnresolvedFunctionReference(
    var name : String,
    var level : Option[L4_LevelSpecification],
    var offset : Option[L4_ConstIndex]) extends L4_FunctionReference {

  override def returnType = L4_UnknownDatatype

  override def prettyprint(out : PpStream) = {
    out << name
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
  }

  override def progress : IR_FunctionReference = {
    Logger.warn(s"Progressing unresolved function reference on L4: $name" + (if (level.isDefined) s"@${ level.get.prettyprint() }" else ""))
    if (offset.isDefined) Logger.warn(s"  and ignoring offset ${ offset.get.prettyprint() }")
    HACK_IR_UndeterminedFunctionReference(prettyprint(), IR_UnknownDatatype)
  }
}
