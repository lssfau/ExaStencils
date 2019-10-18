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
import exastencils.base.l2.L2_RealDatatype
import exastencils.boundary.l1._
import exastencils.core.Duplicate
import exastencils.domain.l1.L1_Domain
import exastencils.field.l2.L2_Field
import exastencils.grid.l1.L1_Localization
import exastencils.knowledge.l1.L1_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.parsers.l1.L1_ReservedSigns
import exastencils.prettyprinting.PpStream

/// L1_Field

case class L1_Field(
    var name : String,
    var level : Int,
    var domain : L1_Domain,
    var initial : Option[L1_Expression],
    var boundary : L1_BoundaryCondition) extends L1_LeveledKnowledgeObject[L2_Field] {

  override def createDuplicate() : L1_Field = {
    L1_Field(name, level, Duplicate(domain), Duplicate(initial), Duplicate(boundary))
  }

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "Field " << name << "@" << level << " " << L1_ReservedSigns.elemOf._2 << " " << domain.name
    if (initial.isDefined) out << " = " << initial.get

    if (boundary != L1_NoBC) {
      out << "\n"
      out << "Field " << name << "@" << level << " " << L1_ReservedSigns.elemOf._2 << " " << L1_ReservedSigns.partial._2 << " " << domain.name << " = " << boundary
    }
  }

  def codeName = name + "_" + level
  def numDimsGrid = domain.numDims

  var localization : Option[L1_Localization] = None

  override def progressImpl() = {
    if (localization.isEmpty) Logger.error("Trying to progress L1 field without localization")
    L2_Field(
      name,
      level,
      domain.getProgressedObj(),
      L2_RealDatatype /*FIXME*/ ,
      localization.get.progress,
      L1_ProgressOption(initial)(_.progress),
      boundary.progress)
  }
}
