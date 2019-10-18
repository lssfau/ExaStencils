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

package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_ColorLoops
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_ColorLoops

case class L3_ColorLoops(var colorExps : ListBuffer[L3_Modulo], var stmts : ListBuffer[L3_Statement]) extends L3_Statement {

  for (cExp <- colorExps)
    cExp.right match {
      case L3_IntegerConstant(i) if (i > 0) => // everything is fine
      case _                                =>
        Logger.error("the divisor of all color expressions for a color with statement must be a positive integer constant")
    }

  override def prettyprint(out : PpStream) = out << "color with {\n" <<< (colorExps, ",\n") << ",\n" <<< (stmts, "\n") << "\n}"
  override def progress = ProgressLocation(L4_ColorLoops(colorExps.map(_.progress), stmts.map(_.progress)))
}
