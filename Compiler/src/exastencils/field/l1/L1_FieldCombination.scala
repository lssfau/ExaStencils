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

package exastencils.field.l1

import scala.collection.mutable.ListBuffer

import exastencils.core.Duplicate
import exastencils.field.l2.L2_FieldCombination
import exastencils.knowledge.l1.L1_LeveledKnowledgeObject
import exastencils.prettyprinting.PpStream

/// L1_FieldCombination

case class L1_FieldCombination(
    var name : String,
    var level : Int,
    var combinationType : String,
    var fields : ListBuffer[L1_Field]) extends L1_LeveledKnowledgeObject[L2_FieldCombination] {

  override def createDuplicate() : L1_FieldCombination = {
    L1_FieldCombination.tupled(Duplicate(L1_FieldCombination.unapply(this).get))
  }

  override def prettyprintDecl(out : PpStream) = {
    out << "FieldCombination " << name << '@' << level << " : \"" << combinationType << "\" = " << fields.map(_.name).mkString(", ")
  }

  override def progressImpl() = {
    L2_FieldCombination(name, level, combinationType, fields.map(_.getProgressedObj()))
  }
}
