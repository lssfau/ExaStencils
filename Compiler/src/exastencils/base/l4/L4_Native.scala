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
import exastencils.base.ir.IR_Native
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_Native

case class L4_Native(nativeCode : String) extends L4_Expression {
  override def prettyprint(out : PpStream) = out << "native ( " << s"'$nativeCode'"  << " )"
  override def progress = ProgressLocation(IR_Native(nativeCode))
}

/// L4_ResolveNativeFunctions

object L4_ResolveNativeFunctions extends DefaultStrategy("Resolve native function references") {
  this += new Transformation("Resolve", {
    case L4_FunctionCall(L4_UnresolvedFunctionReference("native", level, offset), args) =>
      if (level.isDefined) Logger.warn(s"Found leveled native function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found native function with offset; offset is ignored")
      args match {
        case ListBuffer(code : L4_StringLiteral)  => L4_Native(code.value)
        case ListBuffer(code : L4_StringConstant) => L4_Native(code.value)
        case _                                    =>
          Logger.warn("Ignoring native function with unsupported arguments " + args)
          L4_NullStatement
      }
  })
}
