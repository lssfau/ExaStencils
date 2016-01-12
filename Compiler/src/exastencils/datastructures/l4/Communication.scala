package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.communication
import exastencils.datastructures._
import exastencils.knowledge
import exastencils.prettyprinting._

case class CommunicateTarget(var target : String, var begin : Option[Index], var end : Option[Index]) extends Expression {
  override def prettyprint(out : PpStream) = {
    out << target
    if (begin.isDefined) out << ' ' << begin.get
    if (end.isDefined) out << " to " << end.get
  }

  override def progressToIr : communication.CommunicateTarget = {
    communication.CommunicateTarget(
      target,
      if (begin.isDefined) Some(begin.get.progressToIr) else None,
      if (end.isDefined) Some(end.get.progressToIr) else None)
  }
}

case class ApplyBCsStatement(var field : Access) extends Statement {
  override def prettyprint(out : PpStream) = { out << "apply bc to " << field << '\n' }

  override def progressToIr : communication.ApplyBCsStatement = {
    val resolvedField = field match {
      case f : FieldAccess => f.progressToIr.fieldSelection
      case sf : StencilFieldAccess => knowledge.FieldSelection(sf.resolveField,
        ir.IntegerConstant(sf.level.asInstanceOf[SingleLevelSpecification].level),
        FieldAccess.resolveSlot(sf.resolveField, sf.slot),
        sf.arrayIndex)
    }
    communication.ApplyBCsStatement(resolvedField)
  }
}

case class CommunicateStatement(var field : Access, var op : String, var targets : List[CommunicateTarget]) extends Statement {
  override def prettyprint(out : PpStream) = {
    out <<
      (if ("both" == op) "" else (op + ' ')) <<
      "communicate " <<< (targets, " ") << (if (targets.isEmpty) "" else " of ") <<
      field << '\n'
  }

  override def progressToIr : communication.CommunicateStatement = {
    val progressedField = field match {
      case f : FieldAccess => f.progressToIr.fieldSelection
      case sf : StencilFieldAccess => knowledge.FieldSelection(sf.resolveField,
        ir.IntegerConstant(sf.level.asInstanceOf[SingleLevelSpecification].level),
        FieldAccess.resolveSlot(sf.resolveField, sf.slot),
        sf.arrayIndex)
    }
    val progressedTargets : ListBuffer[communication.CommunicateTarget] = ListBuffer()

    if (targets.isEmpty)
      progressedTargets += CommunicateTarget("all", None, None).progressToIr
    else
      for (t <- targets) progressedTargets += t.progressToIr

    communication.CommunicateStatement(progressedField, op, progressedTargets)
  }
}
