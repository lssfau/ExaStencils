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

package exastencils.operator.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.field.l2.L2_FieldAccess
import exastencils.optimization.l2.L2_GeneralSimplifyWrapper
import exastencils.prettyprinting._

/// L2_OperatorFromEquation

object L2_OperatorFromEquation {
  def apply(levels : Option[L2_DeclarationLevelSpecification], entries : List[L2_OperatorFromEqEntry])
  = new L2_OperatorFromEquation(levels, entries.to[ListBuffer])
}

case class L2_OperatorFromEquation(
    var levels : Option[L2_LevelSpecification],
    var entries : ListBuffer[L2_OperatorFromEqEntry]) extends L2_StencilDecl {

  override def name : String = ???
  override def prettyprint(out : PpStream) : Unit = {
    out << "generate operators"
    if (levels.isDefined) out << " @" << levels.get
    out << " {\n" <<< (entries, "\n") << "\n}"
  }

  override def addToKnowledge() = {
    import L2_GenerateStencilFromEquation._

    def level = L2_LevelSpecification.asSingleLevel(levels)

    for (entry <- entries) {
      targetFields = entry.mapping.map(_.unknown.asInstanceOf[L2_FieldAccess])

      process(entry.resolveEquation())

      val eqWithStencils = L2_Equation(0.0, rhs)

      for (rawStencil <- stencils) {
        val stencilName = entry.mapping.find(rawStencil._1 == _.unknown).get.name
        val stencilNumDims = entry.mapping.find(rawStencil._1 == _.unknown).get.unknown.asInstanceOf[L2_FieldAccess].target.numDimsGrid
        val stencilEntries = rawStencil._2.map(entry => L2_StencilOffsetEntry(Duplicate(entry.offset), Duplicate(entry.coefficient)))

        val generateConstStencil = true /* TODO: expose in DSL */

        if (generateConstStencil) {
          val stencil = L2_Stencil(stencilName, level, stencilNumDims, Array.fill(stencilNumDims)(1), stencilEntries.map(_.asStencilMappingEntry))

          //Logger.debug("New stencil:\n" + stencil.printStencilToStr(true))

          L2_StencilCollection.add(stencil)

          val fieldAccess = Duplicate(rawStencil._1)
          fieldAccess.offset = None
          eqWithStencils.lhs += L2_StencilAccess(stencil) * fieldAccess
        } else {
          // inherit localization from target field
          val localization = entry.targetField.asInstanceOf[L2_FieldAccess].target.localization
          val decl = L2_StencilFieldDecl(stencilName, levels, localization, "global", stencilEntries.map(_.asInstanceOf[L2_StencilEntry]))
          decl.addToKnowledge()

          val fieldAccess = Duplicate(rawStencil._1)
          fieldAccess.offset = None
          eqWithStencils.lhs += L2_StencilFieldAccess(L2_StencilFieldCollection.getByIdentifier(stencilName, level).get) * fieldAccess
        }
      }

      entry.updateEquation(L2_GeneralSimplifyWrapper.process(eqWithStencils))
    }
  }

  override def progress = ???

  def addDeclarations() = entries.foreach(_.mapping.foreach(m => L2_StencilCollection.addDeclared(m.name, levels)))
}

/// L2_OperatorMapping

case class L2_OperatorMapping(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var unknown : L2_Access) extends L2_Node with PrettyPrintable {

  override def prettyprint(out : PpStream) = {
    out << unknown << " => " << name
    if (levels.isDefined) out << '@' << levels.get
  }
}
