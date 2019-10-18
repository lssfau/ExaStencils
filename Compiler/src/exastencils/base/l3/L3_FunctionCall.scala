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
import exastencils.base.l4._
import exastencils.prettyprinting._

/// L3_FunctionCall

object L3_FunctionCall {
  def apply(function : L3_FunctionReference, args : L3_Expression*) =
    new L3_FunctionCall(function, args.to[ListBuffer])
}

case class L3_FunctionCall(var function : L3_FunctionReference, var arguments : ListBuffer[L3_Expression]) extends L3_Expression {
  def prettyprint(out : PpStream) = { out << function << " ( " <<< (arguments, ", ") << " )" }
  override def progress = ProgressLocation(L4_FunctionCall(function.progress, arguments.map(_.progress)))
  def name = function.name
}
