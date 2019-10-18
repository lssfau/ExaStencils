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

package exastencils.solver.l3

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.knowledge.l3.L3_LeveledKnowledgeDecl
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_EquationDecl

case class L3_EquationDecl(var name : String, var levels : Option[L3_LevelSpecification], var equation : L3_Equation) extends L3_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "Equation " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " {\n" << equation << "\n}"
  }

  override def addToKnowledge() = L3_EquationCollection.add(L3_NamedEquation(name, L3_LevelSpecification.asSingleLevel(levels), equation))
  override def progress = Logger.error(s"Trying to progress L3 equation declaration for equation $name; this is not supported")
}

/// L3_PrepareEquationDeclaration

object L3_PrepareEquationDeclarations extends DefaultStrategy("Prepare knowledge for L3 equations") {
  this += Transformation("Process new equations", {
    case decl : L3_EquationDecl =>
      L3_EquationCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L3_ProcessEquationDeclarations

object L3_ProcessEquationDeclarations extends DefaultStrategy("Integrate L3 equation declarations with knowledge") {
  this += Transformation("Process equation declarations", {
    case decl : L3_EquationDecl if L3_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
