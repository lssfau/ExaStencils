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

package exastencils.solver.l1

import exastencils.base.ProgressLocation
import exastencils.base.l1._
import exastencils.field.l1.L1_FieldCollection
import exastencils.prettyprinting._
import exastencils.solver.l2.L2_SolverForEqEntry

/// L1_SolverForEqEntry

case class L1_SolverForEqEntry(unknownName : String, eqName : String) extends L1_Node with L1_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << unknownName << " in " << eqName

  def getSolField(level : Int) = L1_FieldCollection.getByIdentifier(unknownName, level).get
  def getEq(level : Int) = L1_EquationCollection.getByIdentifier(eqName, level).get

  override def progress = ProgressLocation(L2_SolverForEqEntry(unknownName, eqName))
}
