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

package exastencils.solver.ir

import exastencils.base.ir._
import exastencils.knowledge.ir.IR_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream

/// IR_EquationAccess

case class IR_EquationAccess(
    var target : IR_NamedEquation,
    var offset : Option[IR_ConstIndex] = None) extends IR_LeveledKnowledgeAccess with IR_CanBeOffset {

  override def datatype = IR_UnknownDatatype

  def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

}
