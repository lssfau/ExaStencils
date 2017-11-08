package exastencils.discretization.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.l1.L1_Statement
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_DiscretizationHints

object L1_DiscretizationHints {
  def apply(hints : List[L1_DiscretizationHint]) = new L1_DiscretizationHints(hints.to[ListBuffer])
}

case class L1_DiscretizationHints(var hints : ListBuffer[L1_DiscretizationHint]) extends L1_Statement {
  override def prettyprint(out : PpStream) = out << "DiscretizationHint {\n" <<< (hints, "\n") << "\n}"
  override def progress = Logger.error(s"Trying to progress L1 discretize hints; this is not supported")
}

/// L1_DiscretizationHint

abstract class L1_DiscretizationHint extends L1_Statement {
  def process() : Unit
  override def progress = Logger.error(s"Trying to progress L1 discretization hint; this is not supported")
}

/// L1_ProcessDiscretizeBlocks

object L1_ProcessDiscretizationHints extends DefaultStrategy("Process discretization hints") {
  this += Transformation("Process", {
    case coll : L1_DiscretizationHints =>
      coll.hints.foreach(_.process())
      None // consume statements
  })
}
