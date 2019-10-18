//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

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

/// L2_ProcessSolverHints

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
