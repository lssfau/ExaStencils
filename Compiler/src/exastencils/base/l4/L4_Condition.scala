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

package exastencils.base.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_IfCondition
import exastencils.prettyprinting.PpStream

/// L4_IfCondition

case class L4_IfCondition(var condition : L4_Expression, var trueBody : ListBuffer[L4_Statement], var falseBody : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "if ( " << condition << " ) {\n"
    out <<< (trueBody, "\n")
    if (falseBody.nonEmpty) {
      out << "\n} else {\n"
      out <<< (falseBody, "\n")
    }
    out << "\n}"
  }

  override def progress = ProgressLocation(IR_IfCondition(condition.progress, trueBody.map(_.progress), falseBody.map(_.progress)))
}
