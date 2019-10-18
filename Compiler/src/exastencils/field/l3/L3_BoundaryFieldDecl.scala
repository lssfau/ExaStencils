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

package exastencils.field.l3

import exastencils.base.l3._
import exastencils.boundary.l3._
import exastencils.datastructures._
import exastencils.knowledge.l3._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L3_BoundaryFieldDecl

case class L3_BoundaryFieldDecl(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var boundary : L3_BoundaryCondition) extends L3_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " on boundary = " << boundary
  }

  override def progress = Logger.error(s"Trying to progress L3 boundary declaration for field $name; this is not supported")

  def addToKnowledge() : Unit = {
    val fieldToAdapt = L3_FieldCollection.getByIdentifier(name, L3_LevelSpecification.asSingleLevel(levels)).get
    fieldToAdapt.boundary = boundary
  }
}

/// L3_ProcessBoundaryDeclarations

object L3_ProcessBoundaryDeclarations extends DefaultStrategy("Integrate L3 boundary declarations with knowledge") {
  this += Transformation("Adapt bc's of new fields", {
    case decl : L3_BoundaryFieldDecl if L3_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
