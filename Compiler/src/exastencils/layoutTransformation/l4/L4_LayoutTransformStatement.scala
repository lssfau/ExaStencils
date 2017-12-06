package exastencils.layoutTransformation.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_DeclarationLevelSpecification
import exastencils.base.l4.L4_ExpressionIndex
import exastencils.base.l4.L4_LevelSpecification
import exastencils.base.l4.L4_Node
import exastencils.base.l4.L4_PlainVariableAccess
import exastencils.base.l4.L4_Statement
import exastencils.layoutTransformation.ir.IR_ExternalFieldAlias
import exastencils.layoutTransformation.ir.IR_FieldConcatenation
import exastencils.layoutTransformation.ir.IR_GenericTransform
import exastencils.layoutTransformation.ir.IR_LayoutTransformStatement
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.prettyprinting.PrettyPrintable

sealed abstract class L4_LayoutTransformStatement extends L4_Statement {
  override def progress : IR_LayoutTransformStatement
}

case class L4_LeveledIdentifier(var id : String, var levels : Option[L4_DeclarationLevelSpecification]) extends L4_Node with PrettyPrintable {
  override def prettyprint(out : PpStream) : Unit = {
    out << id
    if (levels.isDefined)
      out << "@" << levels.get
  }
}

case class L4_ExternalFieldAlias(newName : String, old : L4_LeveledIdentifier) extends L4_LayoutTransformStatement {

  override def prettyprint(out : PpStream) : Unit = out << "rename " << old << " to " << newName

  override def progress() = IR_ExternalFieldAlias(newName, old.id, L4_LevelSpecification.extractLevelListDefAll(old.levels))
}

case class L4_GenericTransform(fields : Seq[L4_LeveledIdentifier], its : Array[L4_PlainVariableAccess], trafo : L4_ExpressionIndex) extends L4_LayoutTransformStatement {

  override def prettyprint(out : PpStream) = out << "transform " <<< (fields, ", ") << " with [" <<< (its, ", ") << "] => " << trafo

  override def progress() : IR_GenericTransform = {
    val irFields = fields.flatMap {
      case L4_LeveledIdentifier(id, levels) =>
        L4_LevelSpecification.extractLevelListDefAll(levels).view.map { (id, _) }
    }
    IR_GenericTransform(irFields, its.map(_.progress), trafo.progress)
  }
}

case class L4_FieldConcatenation(mergedFieldName : String, fieldsToMerge : Seq[String], var levels : Option[L4_DeclarationLevelSpecification]) extends L4_LayoutTransformStatement {

  if (fieldsToMerge.size < 2)
    Logger.error(s"there must be at least two fields to merge (for $mergedFieldName)")

  override def prettyprint(out : PpStream) : Unit = {
    out << "concat "
    if (levels.isDefined)
      out << "@" << levels.get
    out << fieldsToMerge.mkString(", ") << " into " << mergedFieldName
  }

  override def progress() = IR_FieldConcatenation(mergedFieldName, fieldsToMerge, L4_LevelSpecification.extractLevelListDefAll(levels))
}
