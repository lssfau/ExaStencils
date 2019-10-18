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

package exastencils.util.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir._

/// L4_BuildString

case class L4_BuildString(var stringName : L4_Expression, var toPrint : ListBuffer[L4_Expression]) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "buildString ( " << stringName << ", " <<< (toPrint, ", ") << " )"
  override def progress = ProgressLocation(IR_BuildString(stringName.progress, toPrint.map(_.progress)))
}

/// L4_ResolveBuildStringFunctions

object L4_ResolveBuildStringFunctions extends DefaultStrategy("Resolve build string function references") {
  this += new Transformation("Resolve", {
    case L4_ExpressionStatement(L4_FunctionCall(L4_UnresolvedFunctionReference("buildString", level, offset), args)) =>
      if (level.isDefined) Logger.warn(s"Found leveled buildString function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found buildString function with offset; offset is ignored")
      L4_BuildString(args(0), args.slice(1, args.size))
  })
}
