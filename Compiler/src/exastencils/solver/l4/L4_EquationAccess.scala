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

package exastencils.solver.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream
import exastencils.solver.ir.IR_EquationAccess

/// L4_EquationAccess

object L4_EquationAccess {
  def apply(access : L4_FutureEquationAccess) =
    new L4_EquationAccess(L4_EquationCollection.getByIdentifier(access.name, access.level).get, access.offset)
}

case class L4_EquationAccess(
    var target : L4_NamedEquation,
    var offset : Option[L4_ConstIndex] = None) extends L4_LeveledKnowledgeAccess with L4_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (offset.isDefined) out << '@' << offset.get
  }

  override def progress = ProgressLocation(IR_EquationAccess(target.getProgressedObj(), L4_ProgressOption(offset)(_.progress)))
}

/// L4_ResolveEquationAccesses

object L4_ResolveEquationAccesses extends DefaultStrategy("Resolve accesses to equations") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureEquationAccess if L4_EquationCollection.exists(access.name, access.level) =>
      access.toEquationAccess
  })
}
