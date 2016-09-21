package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.omp
import exastencils.prettyprinting.PpStream

/// L4_LoopOverFragments

object L4_LoopOverFragments {
  def apply(statements : List[L4_Statement], reduction : Option[L4_Reduction]) =
    new L4_LoopOverFragments(statements.to[ListBuffer], reduction)
}

case class L4_LoopOverFragments(var statements : ListBuffer[L4_Statement], var reduction : Option[L4_Reduction]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "loop over fragments "
    if (reduction.isDefined) out << "with " << reduction.get
    out << "{\n" <<< statements << "}\n"
  }

  override def progress = new IR_LoopOverFragments(statements.map(_.progress), reduction.map(_.progress)) with omp.OMP_PotentiallyParallel
}
