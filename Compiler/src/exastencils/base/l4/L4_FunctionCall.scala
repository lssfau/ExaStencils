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

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.prettyprinting._

/// L4_FunctionCall

object L4_FunctionCall {
  def apply(function : L4_FunctionReference, arguments : Option[List[L4_Expression]]) =
    new L4_FunctionCall(function, arguments.getOrElse(List()).to[ListBuffer])

  def apply(function : L4_FunctionReference, args : L4_Expression*) =
    new L4_FunctionCall(function, args.to[ListBuffer])
}

case class L4_FunctionCall(var function : L4_FunctionReference, var arguments : ListBuffer[L4_Expression]) extends L4_Expression {
  def name = function.name
  override def prettyprint(out : PpStream) = { out << function << " ( " <<< (arguments, ", ") << " )" }
  override def progress = ProgressLocation(IR_FunctionCall(function.progress, arguments.map(s => s.progress)))
}
