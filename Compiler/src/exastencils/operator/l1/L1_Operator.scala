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

package exastencils.operator.l1

import exastencils.base.l1._
import exastencils.knowledge.l1.L1_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.operator.l2._
import exastencils.prettyprinting._

/// L1_Operator

case class L1_Operator(
    var name : String, // will be used to find the operator
    var level : Int, // the level the operator lives on
    var expr : L1_Expression) extends L1_LeveledKnowledgeObject[L2_Stencil] {

  override def prettyprintDecl(out : PpStream) = out << "Operator " << name << "@" << level << " = " << expr
  override def progressImpl() = Logger.error(s"Direct progression of L1 operator $name is not supported")
}
