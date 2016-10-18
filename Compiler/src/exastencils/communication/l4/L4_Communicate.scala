package exastencils.communication.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.l4._
import exastencils.communication.ir._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.field.l4.L4_FieldAccess
import exastencils.prettyprinting._
import exastencils.stencil.l4.L4_StencilFieldAccess

/// L4_Communicate

object L4_Communicate {
  def apply(field : L4_Access, op : String, targets : List[L4_CommunicateTarget], condition : Option[L4_Expression]) =
    new L4_Communicate(field, op, targets.to[ListBuffer], condition)
}

case class L4_Communicate(
    var field : L4_Access,
    var op : String,
    var targets : ListBuffer[L4_CommunicateTarget],
    var condition : Option[L4_Expression]) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    if ("both" != op) out << op + ' '
    out << "communicate " <<< (targets, " ") << (if (targets.isEmpty) "" else " of ") << field
    if (condition.isDefined) out << " where " << condition
    out << '\n'
  }

  override def progress : IR_Communicate = {
    // TODO: extract to strategy replacing stencil field accesses with corresponding field accesses
    val progressedField = field match {
      case f : L4_FieldAccess         => f.progress.fieldSelection
      case sf : L4_StencilFieldAccess => IR_FieldSelection(sf.target.getProgressedObject.field,
        sf.target.level,
        L4_FieldAccess.resolveSlot(sf.target.getProgressedObject.field, sf.slot),
        sf.arrayIndex)
    }
    val progressedTargets : ListBuffer[IR_CommunicateTarget] = ListBuffer()

    if (targets.isEmpty)
      progressedTargets += L4_CommunicateTarget("all", None, None).progress
    else
      for (t <- targets) progressedTargets += t.progress

    IR_Communicate(progressedField, op, progressedTargets, L4_ProgressOption(condition)(_.progress))
  }
}

/// L4_CommunicateTarget

case class L4_CommunicateTarget(
    var target : String,
    var begin : Option[L4_ConstIndex],
    var end : Option[L4_ConstIndex]) extends L4_Node with L4_Progressable with PrettyPrintable {

  override def prettyprint(out : PpStream) = {
    out << target
    if (begin.isDefined) out << ' ' << begin.get
    if (end.isDefined) out << " to " << end.get
  }

  // FIXME: remove .toExpressionIndex
  override def progress = IR_CommunicateTarget(target, L4_ProgressOption(begin)(_.progress.toExpressionIndex), L4_ProgressOption(end)(_.progress.toExpressionIndex))
}
