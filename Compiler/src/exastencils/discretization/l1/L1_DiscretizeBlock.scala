package exastencils.discretization.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.l1.L1_Statement
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_DiscretizeBlock

object L1_DiscretizeBlock {
  def apply(stmts : List[L1_DiscretizationStatement]) = new L1_DiscretizeBlock(stmts.to[ListBuffer])
}

case class L1_DiscretizeBlock(var stmts : ListBuffer[L1_DiscretizationStatement]) extends L1_Statement {
  override def progress = Logger.error(s"Trying to progress L1 discretize block; this is not supported")
  override def prettyprint(out : PpStream) = out << "Discretize {\n" <<< (stmts, "\n") << "\n}"
}

/// L1_ProcessDiscretizeBlocks

object L1_ProcessDiscretizeBlocks extends DefaultStrategy("Process discretize blocks") {
  this += Transformation("Process", {
    case discr : L1_DiscretizeBlock =>
      discr.stmts.foreach(_.process())
      None // consume statements
  })
}
