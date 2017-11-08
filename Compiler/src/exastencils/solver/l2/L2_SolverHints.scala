package exastencils.solver.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_Statement
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l2.L2_GeneralParameter

/// L2_SolverHints

object L2_SolverHints {
  def apply(hints : List[L2_SolverHint]) = new L2_SolverHints(hints.to[ListBuffer])
}

case class L2_SolverHints(var hints : ListBuffer[L2_SolverHint]) extends L2_Statement {
  override def prettyprint(out : PpStream) = out << "SolverHint {\n" <<< (hints, "\n") << "\n}"
  override def progress = Logger.error(s"Trying to progress L2 solver hints; this is not supported")
}

/// L2_SolverHint

abstract class L2_SolverHint extends L2_Statement {
  def process() : Unit
}

/// L2_SolverParameter

case class L2_SolverParameter(var name : String, var value : Any) extends L2_SolverHint with L2_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def process() = set()
  override def progress = Logger.error(s"Trying to progress L2 solver parameter; this is not supported")
}

/// L2_ProcessDiscretizeBlocks

object L2_ProcessSolverHints extends DefaultStrategy("Process solver hints") {
  this += Transformation("Process", {
    case coll : L2_SolverHints =>
      // handle parameter updates first, then everything else
      val (param, nonParam) = coll.hints.partition(_.isInstanceOf[L2_SolverParameter])
      param.foreach(_.process())
      nonParam.foreach(_.process())

      None // consume statements
  })
}
