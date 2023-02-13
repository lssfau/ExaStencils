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

package exastencils.solver.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.fieldlike.l2.L2_FieldLikeCollections
import exastencils.prettyprinting._
import exastencils.solver.l3.L3_SolverForEqEntry

/// L2_SolverForEqEntry

case class L2_SolverForEqEntry(unknownName : String, eqName : String) extends L2_Node with L2_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << unknownName << " in " << eqName

  def getSolField(level : Int) = L2_FieldLikeCollections.getByIdentifier(unknownName, level).get
  def getEq(level : Int) = L2_EquationCollection.getByIdentifier(eqName, level).get

  override def progress = ProgressLocation(L3_SolverForEqEntry(unknownName, eqName))
}
