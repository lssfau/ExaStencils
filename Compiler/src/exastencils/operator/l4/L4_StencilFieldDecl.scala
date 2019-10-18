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

package exastencils.operator.l4

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.knowledge.l4._
import exastencils.logger._
import exastencils.prettyprinting._

/// L4_StencilFieldDecl

object L4_StencilFieldDecl {
  def apply(name : String, levels : Option[L4_DeclarationLevelSpecification], stencil : String, field : String) =
    new L4_StencilFieldDecl(name, levels, L4_UnresolvedAccess(stencil), L4_UnresolvedAccess(field))
}

case class L4_StencilFieldDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var stencil : L4_Access,
    var field : L4_Access) extends L4_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "StencilField " << name << "< " << field.name << " => " << stencil.name << " >"
    if (levels.isDefined) out << '@' << levels.get
  }

  override def addToKnowledge() : Unit = {
    L4_StencilFieldCollection.add(
      L4_StencilField(name, L4_LevelSpecification.asSingleLevel(levels),
        stencil.asInstanceOf[L4_StencilAccess].target, field.asInstanceOf[L4_FieldAccess].target))
  }

  override def progress = Logger.error(s"Trying to progress L4 stencil template $name; this is not supported")
}

/// L4_PrepareStencilFieldDeclaration

object L4_PrepareStencilFieldDeclarations extends DefaultStrategy("Prepare knowledge for L4 stencil templates") {
  this += Transformation("Process new stencil templates", {
    case decl : L4_StencilFieldDecl =>
      //L4_FieldCollection.addDeclared(decl.name + "_Data", decl.levels)
      //L4_StencilCollection.addDeclared(decl.name + "_Stencil", decl.levels)
      L4_StencilFieldCollection.addDeclared(decl.name, decl.levels)

      decl // preserve declaration statement
  })
}

/// L4_ProcessStencilFieldDeclarations

object L4_ProcessStencilFieldDeclarations extends DefaultStrategy("Integrate L4 stencil template declarations with knowledge") {
  this += Transformation("Process new stencil templates", {
    case decl : L4_StencilFieldDecl if L4_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
