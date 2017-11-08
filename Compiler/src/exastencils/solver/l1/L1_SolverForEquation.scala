package exastencils.solver.l1

import scala.collection.mutable._

import exastencils.prettyprinting.PpStream
import exastencils.solver.l2.L2_SolverForEquation

/// L1_SolverForEquation

object L1_SolverForEquation {
  def apply(entries : List[L1_SolverForEqEntry]) = new L1_SolverForEquation(entries.to[ListBuffer])
}

case class L1_SolverForEquation(var entries : ListBuffer[L1_SolverForEqEntry]) extends L1_SolverHint {
  override def prettyprint(out : PpStream) = out << "generate solver for " <<< (entries, " and ")
  override def progress = L2_SolverForEquation(entries.map(_.progress))
}
