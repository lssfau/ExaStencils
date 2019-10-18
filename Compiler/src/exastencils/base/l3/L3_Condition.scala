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

package exastencils.base.l3

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_IfCondition
import exastencils.prettyprinting._

/// L3_IfCondition

object L3_IfCondition {
  def apply(condition : L3_Expression, trueBody : L3_Statement) = new L3_IfCondition(condition, ListBuffer(trueBody), ListBuffer())
  def apply(condition : L3_Expression, trueBody : ListBuffer[L3_Statement]) = new L3_IfCondition(condition, trueBody, ListBuffer())
}

case class L3_IfCondition(var condition : L3_Expression, var trueBody : ListBuffer[L3_Statement], var falseBody : ListBuffer[L3_Statement]) extends L3_Statement {
  override def prettyprint(out : PpStream) = {
    out << "if ( " << condition << " ) {\n"
    out <<< (trueBody, "\n")
    if (falseBody.nonEmpty) {
      out << "\n} else {\n"
      out <<< (falseBody, "\n")
    }
    out << "\n}"
  }

  override def progress = ProgressLocation(L4_IfCondition(condition.progress, trueBody.map(_.progress), falseBody.map(_.progress)))
}
