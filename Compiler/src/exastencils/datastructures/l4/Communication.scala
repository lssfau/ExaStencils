package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.l4._
import exastencils.field.l4.L4_FieldAccess
import exastencils.prettyprinting._
import exastencils.stencil.l4.L4_StencilFieldAccess

case class CommunicateTarget(var target : String, var begin : Option[L4_ConstIndex], var end : Option[L4_ConstIndex]) extends L4_Expression {
  override def prettyprint(out : PpStream) = {
    out << target
    if (begin.isDefined) out << ' ' << begin.get
    if (end.isDefined) out << " to " << end.get
  }

  override def progress : communication.CommunicateTarget = {
    communication.CommunicateTarget(
      target,
      if (begin.isDefined) Some(begin.get.progress.toExpressionIndex) else None, // TODO: remove .toExpressionIndex
      if (end.isDefined) Some(end.get.progress.toExpressionIndex) else None) // TODO: remove .toExpressionIndex
  }
}

case class ApplyBCsStatement(var field : L4_Access) extends L4_Statement {
  override def prettyprint(out : PpStream) = { out << "apply bc to " << field << '\n' }

  override def progress : communication.ApplyBCsStatement = {
    val resolvedField = field match {
      case f : L4_FieldAccess         => f.progress.fieldSelection
      case sf : L4_StencilFieldAccess => knowledge.FieldSelection(sf.target.getProgressedObject.field,
        sf.target.level,
        L4_FieldAccess.resolveSlot(sf.target.getProgressedObject.field, sf.slot),
        sf.arrayIndex)
    }
    communication.ApplyBCsStatement(resolvedField)
  }
}

case class CommunicateStatement(var field : L4_Access, var op : String, var targets : List[CommunicateTarget], var condition : Option[L4_Expression]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    if ("both" != op) out << op + ' '
    out << "communicate " <<< (targets, " ") << (if (targets.isEmpty) "" else " of ") << field
    if (condition.isDefined) out << "where " << condition
    out << '\n'
  }

  override def progress : communication.CommunicateStatement = {
    val progressedField = field match {
      case f : L4_FieldAccess         => f.progress.fieldSelection
      case sf : L4_StencilFieldAccess => knowledge.FieldSelection(sf.target.getProgressedObject.field,
        sf.target.level,
        L4_FieldAccess.resolveSlot(sf.target.getProgressedObject.field, sf.slot),
        sf.arrayIndex)
    }
    val progressedTargets : ListBuffer[communication.CommunicateTarget] = ListBuffer()

    if (targets.isEmpty)
      progressedTargets += CommunicateTarget("all", None, None).progress
    else
      for (t <- targets) progressedTargets += t.progress

    communication.CommunicateStatement(progressedField, op, progressedTargets, if (condition.isDefined) Some(condition.get.progress) else None)
  }
}
