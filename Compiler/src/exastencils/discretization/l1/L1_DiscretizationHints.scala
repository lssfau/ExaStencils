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

package exastencils.discretization.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.l1.L1_Statement
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l1.L1_GeneralParameter

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

/// L1_DiscretizationParameter

case class L1_DiscretizationParameter(var name : String, var value : Any) extends L1_DiscretizationHint with L1_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def process() = set()
}

/// L1_ProcessDiscretizationHints

object L1_ProcessDiscretizationHints extends DefaultStrategy("Process discretization hints") {
  this += Transformation("Process", {
    case coll : L1_DiscretizationHints =>
      // handle parameter updates first, then everything else
      val (param, nonParam) = coll.hints.partition(_.isInstanceOf[L1_DiscretizationParameter])
      param.foreach(_.process())
      nonParam.foreach(_.process())

      None // consume statements
  })
}
