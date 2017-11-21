package exastencils.layoutTransformation.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_ExpressionIndex
import exastencils.base.l4.L4_PlainVariableAccess
import exastencils.base.l4.L4_Statement
import exastencils.layoutTransformation.ir.IR_ExternalFieldAlias
import exastencils.layoutTransformation.ir.IR_FieldConcatenation
import exastencils.layoutTransformation.ir.IR_GenericTransform
import exastencils.layoutTransformation.ir.IR_LayoutTransformStatement
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

sealed abstract class L4_LayoutTransformStatement extends L4_Statement {
  override def progress : IR_LayoutTransformStatement
}

case class L4_ExternalFieldAlias(newName : String, oldName : String) extends L4_LayoutTransformStatement {

  override def prettyprint(out : PpStream) : Unit = out << "rename " << oldName << " as " << newName

  override def progress : IR_ExternalFieldAlias = IR_ExternalFieldAlias(newName, oldName)
}

case class L4_GenericTransform(fields : Seq[String], its : Array[L4_PlainVariableAccess], trafo : L4_ExpressionIndex) extends L4_LayoutTransformStatement {

  override def prettyprint(out : PpStream) = out << "transform " << fields.mkString(", ") << " with [" <<< (its, ", ") << "] => " << trafo

  override def progress = IR_GenericTransform(fields, its.map(_.progress), trafo.progress)
}

case class L4_FieldConcatenation(mergedFieldName : String, fieldsToMerge : ListBuffer[String]) extends L4_LayoutTransformStatement {

  if (fieldsToMerge.size < 2)
    Logger.error(s"there must be at least two fields to merge (for $mergedFieldName)")

  override def prettyprint(out : PpStream) = out << "concat " << fieldsToMerge.mkString(", ") << " into " << mergedFieldName

  override def progress : IR_FieldConcatenation = IR_FieldConcatenation(mergedFieldName, fieldsToMerge)
}
