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

package exastencils.field.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_FieldCombinationDecl

object L4_FieldCombinationDecl {
  def apply(name : String, levels : Option[L4_DeclarationLevelSpecification], combinationType : String, fields : List[L4_UnresolvedAccess]) =
    new L4_FieldCombinationDecl(name, levels, combinationType, fields.map(_.asInstanceOf[L4_Access]).to[ListBuffer])
}

case class L4_FieldCombinationDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var combinationType : String,
    var fields : ListBuffer[L4_Access]) extends L4_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "FieldCombination " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " : \"" << combinationType << "\" = " << fields.map(_.name).mkString(", ")
  }

  override def addToKnowledge() : Unit = {
    L4_FieldCombinationCollection.add(
      L4_FieldCombination(name, levels.get.resolveLevel, combinationType, fields.map(_.asInstanceOf[L4_FieldAccess].target)))
  }

  override def progress = Logger.error(s"Trying to progress l4 field combination declaration for $name; this is not supported")
}

/// L4_ProcessFieldCombinationDeclarations

object L4_ProcessFieldCombinationDeclarations extends DefaultStrategy("Integrate L4 field combination declarations with knowledge") {
  this += Transformation("Process field combination declarations", {
    case decl : L4_FieldCombinationDecl if L4_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
