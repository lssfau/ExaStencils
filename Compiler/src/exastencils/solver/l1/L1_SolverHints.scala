package exastencils.solver.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.l1.L1_Statement
import exastencils.prettyprinting.PpStream
import exastencils.solver.l2._
import exastencils.util.l1.L1_GeneralParameter

/// L1_SolverHints

object L1_SolverHints {
  def apply(hints : List[L1_SolverHint]) = new L1_SolverHints(hints.to[ListBuffer])
}

case class L1_SolverHints(var hints : ListBuffer[L1_SolverHint]) extends L1_Statement {
  override def prettyprint(out : PpStream) = out << "SolverHint {\n" <<< (hints, "\n") << "\n}"
  override def progress = L2_SolverHints(hints.map(_.progress))
}

/// L1_SolverHint

abstract class L1_SolverHint extends L1_Statement {
  override def progress : L2_SolverHint
}

/// L1_SolverParameter

case class L1_SolverParameter(var name : String, var value : Any) extends L1_SolverHint with L1_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def progress() = L2_SolverParameter(name, value)
}
