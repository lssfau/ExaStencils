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

package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.l4.L4_WaLBerlaLoopOverField

/// L4_RepeatLoops

case class L4_RepeatLoops(var conditions : ListBuffer[L4_Expression], var stmts : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "repeat with {\n" <<< (conditions, ",\n") << ",\n" <<< (stmts, "\n") << "\n}"

  private def generateStmtsForColor(color : L4_Expression) = {
    val newStmts = Duplicate(stmts)
    newStmts.transform {
      case loop : L4_LoopOverField =>
        if (loop.condition.isDefined)
          loop.condition = Some(L4_AndAnd(loop.condition.get, color))
        else
          loop.condition = Some(color)
        loop
      case loop : L4_WaLBerlaLoopOverField =>
        if (loop.condition.isDefined)
          loop.condition = Some(L4_AndAnd(loop.condition.get, color))
        else
          loop.condition = Some(color)
        loop

      case other =>
        Logger.warn("Ignoring statement while repeating/coloring: " + other)
        other
    }
    newStmts
  }

  override def progress : IR_Scope = ProgressLocation {
    // TODO: extract loop duplication to separate transformation
    val newStmts = conditions.flatMap(generateStmtsForColor)
    IR_Scope(newStmts.map(_.progress : IR_Statement))
  }
}
