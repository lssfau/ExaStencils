package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.communication
import exastencils.prettyprinting._

case class CommunicateTarget(var target : String, var begin : Option[Index], var end : Option[Index]) extends Expression {
  def prettyprint(out : PpStream) = {
    out << target
    if (begin.isDefined) out << ' ' << begin.get
    if (end.isDefined) out << " to " << end.get
  }

  def progressToIr : communication.CommunicateTarget = {
    communication.CommunicateTarget(
      target,
      if (begin.isDefined) Some(begin.get.progressToIr) else None,
      if (end.isDefined) Some(end.get.progressToIr) else None)
  }
}

case class ApplyBCsStatement(var field : FieldAccess) extends Statement {
  def prettyprint(out : PpStream) = { out << "apply bc to " << field << '\n' }

  def progressToIr : communication.ApplyBCsStatement = {
    communication.ApplyBCsStatement(field.progressToIr.fieldSelection)
  }
}

case class CommunicateStatement(var field : FieldAccess, var op : String, var targets : List[CommunicateTarget]) extends Statement {
  def prettyprint(out : PpStream) = {
    out <<
      (if ("both" == op) "" else (op + ' ')) <<
      "communicate " <<< (targets, " ") << (if (targets.isEmpty) "" else " of ") <<
      field << '\n'
  }

  def progressToIr : communication.CommunicateStatement = {
    val progressedTargets : ListBuffer[communication.CommunicateTarget] = ListBuffer()
    val progressedField = field.progressToIr.fieldSelection

    if (targets.isEmpty)
      progressedTargets += CommunicateTarget("all", None, None).progressToIr
    else
      for (t <- targets) progressedTargets += t.progressToIr

    communication.CommunicateStatement(progressedField, op, progressedTargets)
  }
}
