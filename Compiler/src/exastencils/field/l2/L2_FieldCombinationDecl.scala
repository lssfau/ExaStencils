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

package exastencils.field.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l2._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_FieldCombinationDecl

object L2_FieldCombinationDecl {
  def apply(name : String, levels : Option[L2_LevelSpecification], combinationType : String, fields : List[L2_UnresolvedAccess]) =
    new L2_FieldCombinationDecl(name, levels, combinationType, fields.map(_.asInstanceOf[L2_Access]).to[ListBuffer])
}

case class L2_FieldCombinationDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var combinationType : String,
    var fields : ListBuffer[L2_Access]) extends L2_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "FieldCombination " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " : \"" << combinationType << "\" = " << fields.map(_.name).mkString(", ")
  }

  override def addToKnowledge() : Unit = {
    L2_FieldCombinationCollection.add(
      L2_FieldCombination(name, levels.get.resolveLevel, combinationType, fields.map(_.asInstanceOf[L2_FieldAccess].target)))
  }

  override def progress = Logger.error(s"Trying to progress l2 field combination declaration for $name; this is not supported")
}

/// L2_ProcessFieldCombinationDeclarations

object L2_ProcessFieldCombinationDeclarations extends DefaultStrategy("Integrate L2 field combination declarations with knowledge") {
  this += Transformation("Process field combination declarations", {
    case decl : L2_FieldCombinationDecl if L2_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
