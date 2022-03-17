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
import exastencils.datastructures._
import exastencils.fieldlike.l4.L4_FieldLayoutDeclLike
import exastencils.grid.l4._
import exastencils.knowledge.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_FieldLayoutOption

case class L4_FieldLayoutOption(
    var name : String,
    var value : L4_ConstIndex,
    var hasCommunication : Boolean) extends L4_Node with PrettyPrintable {

  override def prettyprint(out : PpStream) = {
    out << name << " = " << value
    if (hasCommunication)
      out << " with communication"
  }
}

/// L4_FieldLayoutDecl

object L4_FieldLayoutDecl {
  def apply(name : String, levels : Option[L4_DeclarationLevelSpecification], datatype : L4_Datatype, localization : String, options : List[L4_FieldLayoutOption]) =
    new L4_FieldLayoutDecl(name, levels, datatype, L4_Localization.resolve(localization), options.to[ListBuffer])
}

case class L4_FieldLayoutDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var datatype : L4_Datatype,
    var localization : L4_Localization,
    var options : ListBuffer[L4_FieldLayoutOption]) extends L4_LeveledKnowledgeDecl with L4_FieldLayoutDeclLike {

  override def prettyprint(out : PpStream) : Unit = {
    out << "Layout " << name << "< " << datatype << ", " << localization << " >"
    if (levels.isDefined) out << '@' << levels.get
    out << " {\n" <<< (options, "\n") << "\n}"
  }

  def composeLayout(level : Int) : L4_FieldLayout = {

    val numGhost = evalFieldLayoutValue("ghostLayers")
    val numDup = evalFieldLayoutValue("duplicateLayers")
    val innerPoints = evalFieldLayoutInnerPoints(level, numDup, numGhost)

    // compile final layout
    L4_FieldLayout(
      name, level, numDimsGrid,
      datatype, localization,
      numGhost,
      evalFieldLayoutBoolean("ghostLayers"),
      numDup,
      evalFieldLayoutBoolean("duplicateLayers"),
      innerPoints)
  }

  override def addToKnowledge() : Unit = {
    val level = levels.get.resolveLevel
    L4_FieldLayoutCollection.add(composeLayout(level))
  }

  override def progress = Logger.error(s"Trying to progress l4 field layout declaration for $name; this is not supported")
}

/// L4_PrepareFieldLayoutDeclaration

object L4_PrepareFieldLayoutDeclarations extends DefaultStrategy("Prepare knowledge for L4 field layouts") {
  this += Transformation("Process new field layouts", {
    case decl : L4_FieldLayoutDecl =>
      L4_FieldLayoutCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L4_ProcessFieldLayoutDeclarations

object L4_ProcessFieldLayoutDeclarations extends DefaultStrategy("Integrate L4 field layout declarations with knowledge") {
  this += Transformation("Process field layout declarations", {
    case decl : L4_FieldLayoutDecl if L4_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
