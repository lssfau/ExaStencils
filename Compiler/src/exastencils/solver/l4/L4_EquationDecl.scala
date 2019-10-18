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

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_EquationDecl

case class L4_EquationDecl(var name : String, var levels : Option[L4_DeclarationLevelSpecification], var equation : L4_Equation) extends L4_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "Equation " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " {\n" << equation << "\n}"
  }

  override def addToKnowledge() = L4_EquationCollection.add(L4_NamedEquation(name, L4_LevelSpecification.asSingleLevel(levels), equation))
  override def progress = Logger.error(s"Trying to progress L4 equation declaration for equation $name; this is not supported")
}

/// L4_PrepareEquationDeclaration

object L4_PrepareEquationDeclarations extends DefaultStrategy("Prepare knowledge for L4 equations") {
  this += Transformation("Process new equations", {
    case decl : L4_EquationDecl =>
      L4_EquationCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L4_ProcessEquationDeclarations

object L4_ProcessEquationDeclarations extends DefaultStrategy("Integrate L4 equation declarations with knowledge") {
  this += Transformation("Process equation declarations", {
    case decl : L4_EquationDecl if L4_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
