package exastencils.runner

import exastencils.logger.Logger

/// Constraint

case class Constraint(var code : CodeWrapper) {
  def print() : String = code.print()

  def eval() : Boolean = {
    val ret = code.eval[Boolean]()
    if (RunnerConfig.debug)
      Logger.warn("" + ret)
    ret
  }
}
