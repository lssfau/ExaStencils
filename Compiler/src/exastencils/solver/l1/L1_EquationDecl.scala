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

package exastencils.solver.l1

import exastencils.base.l1._
import exastencils.datastructures._
import exastencils.knowledge.l1.L1_LeveledKnowledgeDecl
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_EquationDecl

case class L1_EquationDecl(var name : String, var levels : Option[L1_LevelSpecification], var equation : L1_Equation) extends L1_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "Equation " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " " << equation
  }

  override def addToKnowledge() = L1_EquationCollection.add(L1_NamedEquation(name, L1_LevelSpecification.asSingleLevel(levels), equation))
  override def progress = Logger.error(s"Trying to progress L1 equation declaration for equation $name; this is not supported")
}

/// L1_PrepareEquationDeclaration

object L1_PrepareEquationDeclarations extends DefaultStrategy("Prepare knowledge for L1 equations") {
  this += Transformation("Process new equations", {
    case decl : L1_EquationDecl =>
      L1_EquationCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L1_ProcessEquationDeclarations

object L1_ProcessEquationDeclarations extends DefaultStrategy("Integrate L1 equation declarations with knowledge") {
  this += Transformation("Process equation declarations", {
    case decl : L1_EquationDecl if L1_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
