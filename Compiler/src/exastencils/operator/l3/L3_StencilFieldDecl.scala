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

package exastencils.operator.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.baseExt.l3._
import exastencils.boundary.l3.L3_NoBC
import exastencils.datastructures._
import exastencils.domain.l3._
import exastencils.field.l3._
import exastencils.grid.l3.L3_Localization
import exastencils.knowledge.l3._
import exastencils.logger._
import exastencils.prettyprinting._

/// L3_StencilFieldDecl

object L3_StencilFieldDecl {
  def apply(name : String, levels : Option[L3_LevelSpecification], localization : String, domainName : String, entries : List[L3_StencilEntry]) =
    new L3_StencilFieldDecl(name, levels, L3_Localization.resolve(localization), domainName, entries.to[ListBuffer])
}

case class L3_StencilFieldDecl(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var localization : L3_Localization,
    var domainName : String,
    var entries : ListBuffer[L3_StencilEntry]) extends L3_LeveledKnowledgeDecl {

  def offsets = entries.map(_.asStencilOffsetEntry.offset)

  override def prettyprint(out : PpStream) = {
    out << "Operator " << name << " from StencilField on " << localization << " of " << domainName << " {\n"
    for (offset <- offsets)
      out << offset << " =>\n"
    out << "}"
  }

  override def addToKnowledge() : Unit = {
    val level = L3_LevelSpecification.asSingleLevel(levels)
    val domain = L3_DomainCollection.getByIdentifier(domainName).get

    // TODO: warn for divergent lengths?
    val numDims = offsets.map(_.length).max

    val field = L3_Field(name + "_Data", level, domain,
      L3_VectorDatatype(L3_RealDatatype /*FIXME: datatype*/ , offsets.length),
      localization, None, L3_NoBC)

    val stencil = L3_Stencil(name + "_Stencil", level, numDims, Array.fill(numDims)(1.0),
      offsets.zipWithIndex.map { case (offset, i) =>
        L3_StencilOffsetEntry(offset, L3_HigherDimSelection(L3_FieldAccess(field), L3_ConstIndex(i))).asStencilMappingEntry
      })

    L3_FieldCollection.add(field)
    L3_StencilCollection.add(stencil)
    L3_StencilFieldCollection.add(L3_StencilField(name, level, stencil, field))
  }

  override def progress = Logger.error(s"Trying to progress L3 stencil template $name; this is not supported")
}

/// L3_PrepareStencilFieldDeclaration

object L3_PrepareStencilFieldDeclarations extends DefaultStrategy("Prepare knowledge for L3 stencil templates") {
  this += Transformation("Process new stencil templates", {
    case decl : L3_StencilFieldDecl =>
      L3_FieldCollection.addDeclared(decl.name + "_Data", decl.levels)
      L3_StencilCollection.addDeclared(decl.name + "_Stencil", decl.levels)

      decl // preserve declaration statement
  })
}

/// L3_ProcessStencilFieldDeclarations

object L3_ProcessStencilFieldDeclarations extends DefaultStrategy("Integrate L3 stencil template declarations with knowledge") {
  this += Transformation("Process new stencil templates", {
    case decl : L3_StencilFieldDecl if L3_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
