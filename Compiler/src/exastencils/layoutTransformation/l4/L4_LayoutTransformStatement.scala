package exastencils.layoutTransformation.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_Access
import exastencils.base.l4.L4_ExpressionIndex
import exastencils.base.l4.L4_PlainVariableAccess
import exastencils.base.l4.L4_Statement
import exastencils.field.l4.L4_FieldAccess
import exastencils.layoutTransformation.ir.IR_ExternalFieldAlias
import exastencils.layoutTransformation.ir.IR_FieldConcatenation
import exastencils.layoutTransformation.ir.IR_GenericTransform
import exastencils.layoutTransformation.ir.IR_LayoutTransformStatement
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

sealed abstract class L4_LayoutTransformStatement extends L4_Statement {
  override def progress : IR_LayoutTransformStatement
}

case class L4_ExternalFieldAlias(newName : String, field : L4_Access) extends L4_LayoutTransformStatement {

  override def prettyprint(out : PpStream) : Unit = out << "rename " << field << " as " << newName

  override def progress : IR_ExternalFieldAlias = {
    val resolvedField = field match {
      case access : L4_FieldAccess => access.target.getProgressedObj()
      case _                       => Logger.error("Cannot determine field for L4_Access " + field)
    }
    IR_ExternalFieldAlias(newName, resolvedField)
  }
}

case class L4_GenericTransform(field : String, level : Int, its : Array[L4_PlainVariableAccess], trafo : L4_ExpressionIndex) extends L4_LayoutTransformStatement {

  override def prettyprint(out : PpStream) : Unit = out << "transform " << field << "@" << level << " with [" <<< (its, ", ") << "] => " << trafo

  override def progress : IR_GenericTransform = {
    IR_GenericTransform(field, level, its.map(_.progress), trafo.progress)
  }
}

case class L4_FieldConcatenation(mergedFieldName : String, fieldsToMerge : ListBuffer[L4_Access]) extends L4_LayoutTransformStatement {

  override def prettyprint(out : PpStream) : Unit = out << "concat " <<< (fieldsToMerge, " and ") << " into " << mergedFieldName

  override def progress : IR_FieldConcatenation = {
    val irFields = fieldsToMerge.map {
      case access : L4_FieldAccess => access.target.getProgressedObj()
      case access                  => Logger.error("Cannot determine field for L4_Access " + access)
    }
    IR_FieldConcatenation(mergedFieldName, irFields)
  }
}
