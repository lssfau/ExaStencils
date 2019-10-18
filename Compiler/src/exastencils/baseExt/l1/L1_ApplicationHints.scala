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

package exastencils.baseExt.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l1.L1_Statement
import exastencils.baseExt.l2._
import exastencils.prettyprinting.PpStream
import exastencils.util.l1.L1_GeneralParameter

/// L1_ApplicationHints

object L1_ApplicationHints {
  def apply(hints : List[L1_ApplicationHint]) = new L1_ApplicationHints(hints.to[ListBuffer])
}

case class L1_ApplicationHints(var hints : ListBuffer[L1_ApplicationHint]) extends L1_Statement {
  override def prettyprint(out : PpStream) = out << "ApplicationHint {\n" <<< (hints, "\n") << "\n}"
  override def progress = ProgressLocation(L2_ApplicationHints(hints.map(_.progress)))
}

/// L1_ApplicationHint

abstract class L1_ApplicationHint extends L1_Statement {
  override def progress : L2_ApplicationHint
}

/// L1_ApplicationParameter

case class L1_ApplicationParameter(var name : String, var value : Any) extends L1_ApplicationHint with L1_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def progress = ProgressLocation(L2_ApplicationParameter(name, value))
}
