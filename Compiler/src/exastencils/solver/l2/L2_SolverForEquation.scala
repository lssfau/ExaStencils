package exastencils.solver.l2

import scala.collection.mutable._

import exastencils.base.ExaRootNode
import exastencils.prettyprinting.PpStream
import exastencils.solver.l3.L3_SolverForEquation

/// L2_SolverForEquation

object L2_SolverForEquation {
  def apply(entries : List[L2_SolverForEqEntry]) = new L2_SolverForEquation(entries.to[ListBuffer])
}

case class L2_SolverForEquation(var entries : ListBuffer[L2_SolverForEqEntry]) extends L2_SolverHint {
  override def prettyprint(out : PpStream) = out << "generate solver for " <<< (entries, " and ")
  override def process() = ExaRootNode.l2_root.nodes += this // append to l2 root; use automatic progression
  override def progress = L3_SolverForEquation(entries.map(_.progress))
}
