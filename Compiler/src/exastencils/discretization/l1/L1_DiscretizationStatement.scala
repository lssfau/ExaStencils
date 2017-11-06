package exastencils.discretization.l1

import exastencils.base.l1._
import exastencils.logger.Logger

/// L1_DiscretizationStatement

abstract class L1_DiscretizationStatement extends L1_Statement {
  def process() : Unit
  override def progress = Logger.error(s"Trying to progress L1 discretize statement; this is not supported")
}
