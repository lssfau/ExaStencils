package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils._
import exastencils.base.l4.L4_Expression
import exastencils.datastructures._
import exastencils.prettyprinting._

case class CommunicateTarget(var target : String, var begin : Option[Index], var end : Option[Index]) extends L4_Expression {
  override def prettyprint(out : PpStream) = {
    out << target
    if (begin.isDefined) out << ' ' << begin.get
    if (end.isDefined) out << " to " << end.get
  }

  override def progress : communication.CommunicateTarget = {
    communication.CommunicateTarget(
      target,
      if (begin.isDefined) Some(begin.get.progress) else None,
      if (end.isDefined) Some(end.get.progress) else None)
  }
}

case class ApplyBCsStatement(var field : Access) extends Statement {
  override def prettyprint(out : PpStream) = { out << "apply bc to " << field << '\n' }

  override def progress : communication.ApplyBCsStatement = {
    val resolvedField = field match {
      case f : FieldAccess         => f.progress.fieldSelection
      case sf : StencilFieldAccess => knowledge.FieldSelection(sf.resolveField,
        ir.IntegerConstant(sf.level.asInstanceOf[SingleLevelSpecification].level),
        FieldAccess.resolveSlot(sf.resolveField, sf.slot),
        sf.arrayIndex)
    }
    communication.ApplyBCsStatement(resolvedField)
  }
}

case class CommunicateStatement(var field : Access, var op : String, var targets : List[CommunicateTarget], var condition : Option[L4_Expression]) extends Statement {
  override def prettyprint(out : PpStream) = {
    if ("both" != op) out << op + ' '
    out << "communicate " <<< (targets, " ") << (if (targets.isEmpty) "" else " of ") << field
    if (condition.isDefined) out << "where " << condition
    out << '\n'
  }

  override def progress : communication.CommunicateStatement = {
    val progressedField = field match {
      case f : FieldAccess         => f.progress.fieldSelection
      case sf : StencilFieldAccess => knowledge.FieldSelection(sf.resolveField,
        ir.IntegerConstant(sf.level.asInstanceOf[SingleLevelSpecification].level),
        FieldAccess.resolveSlot(sf.resolveField, sf.slot),
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
