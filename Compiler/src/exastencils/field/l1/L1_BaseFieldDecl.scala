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

import exastencils.base.l1._
import exastencils.boundary.l1.L1_NoBC
import exastencils.domain.l1._
import exastencils.parsers.l1.L1_ReservedSigns
import exastencils.prettyprinting._

/// L1_BaseFieldDecl

object L1_BaseFieldDecl {
  def apply(identifier : String, levels : Option[L1_LevelSpecification], domain : String, initial : Option[L1_Expression]) =
    new L1_BaseFieldDecl(identifier, levels, L1_FutureDomainAccess(domain), initial)
}

case class L1_BaseFieldDecl(
    var name : String,
    var levels : Option[L1_LevelSpecification],
    var domain : L1_Access,
    var initial : Option[L1_Expression]) extends L1_FieldDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field " << name
    if (levels.isDefined) out << '@' << levels.get
    out << s" ${ L1_ReservedSigns.elemOf._2 } " << domain
    if (initial.isDefined) out << " = " << initial.get
  }

  override def addToKnowledge() : Unit = {
    L1_FieldCollection.add(
      L1_Field(
        name,
        L1_LevelSpecification.asSingleLevel(levels),
        domain.asInstanceOf[L1_DomainAccess].target,
        initial,
        L1_NoBC))
  }
}
