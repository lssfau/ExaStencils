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

package exastencils.base.l2

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.prettyprinting._

/// L2_FunctionCall

object L2_FunctionCall {
  def apply(function : L2_FunctionReference, args : L2_Expression*) =
    new L2_FunctionCall(function, args.to[ListBuffer])
}

case class L2_FunctionCall(var function : L2_FunctionReference, var arguments : ListBuffer[L2_Expression]) extends L2_Expression {
  def prettyprint(out : PpStream) = { out << function << " ( " <<< (arguments, ", ") << " )" }
  override def progress = ProgressLocation(L3_FunctionCall(function.progress, arguments.map(_.progress)))
  def name = function.name
}
