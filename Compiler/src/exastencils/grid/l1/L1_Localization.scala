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

package exastencils.grid.l1

import exastencils.base.ProgressLocation
import exastencils.base.l1._
import exastencils.grid.l2._
import exastencils.prettyprinting._

/// L1_Localization

object L1_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: sth more elegant?
  def dimToString(dim : Int) = ('x'.toInt + dim).toChar
  def stringToDim(dim : Char) = dim.toInt - 'x'.toInt

  // TODO: max number of dimensions?
  def availableLocalizations = List(L1_AtNode, L1_AtCellCenter, L1_AtFaceCenter(0), L1_AtFaceCenter(1), L1_AtFaceCenter(2), L1_AtBoundary)

  def resolve(name : String) = availableLocalizations.find(_.name.toLowerCase == name.toLowerCase).get
}

abstract class L1_Localization extends L1_Node with PrettyPrintable with L1_Progressable {
  def name : String
  override def prettyprint(out : PpStream) = out << name
  override def progress : L2_Localization
}

/// L1_AtNodes

case object L1_AtNode extends L1_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Node"
  override def progress = ProgressLocation(L2_AtNode)
}

/// L1_AtCells

case object L1_AtCellCenter extends L1_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Cell"
  override def progress = ProgressLocation(L2_AtCellCenter)
}

/// L1_AtFace

case class L1_AtFaceCenter(dim : Int) extends L1_Localization {
  def name = s"Face_${ L1_Localization.dimToString(dim) }"
  override def progress = ProgressLocation(L2_AtFaceCenter(dim))
}

/// L1_AtBoundary

// special object for accessing coordinates mapped to the boundary
case object L1_AtBoundary extends L1_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Boundary"
  override def progress = ProgressLocation(L2_AtBoundary)
}
