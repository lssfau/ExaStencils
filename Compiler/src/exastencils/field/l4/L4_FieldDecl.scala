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

import exastencils.base.l4.L4_MayBlockResolution
import exastencils.datastructures._
import exastencils.fieldlike.l4.L4_FieldDeclLike
import exastencils.logger._

/// L4_FieldDecl

object L4_FieldDecl {
  var runningIndex = 0
}

abstract class L4_FieldDecl extends L4_FieldDeclLike {
  override def progress = Logger.error(s"Trying to progress l4 field declaration for field $name; this is not supported")
}

/// L4_PrepareFieldDeclaration

object L4_PrepareFieldDeclarations extends DefaultStrategy("Prepare knowledge for L4 fields") {
  this += Transformation("Process new fields", {
    case decl : L4_FieldDecl =>
      L4_FieldCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L4_ProcessFieldDeclarations

object L4_ProcessFieldDeclarations extends DefaultStrategy("Integrate L4 field declarations with knowledge") {
  this += Transformation("Process field declarations", {
    case decl : L4_FieldDecl if L4_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
