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

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_BaseStencilDecl

object L4_BaseStencilDecl {
  def apply(name : String, levels : Option[L4_DeclarationLevelSpecification], entries : List[L4_StencilEntry]) =
    new L4_BaseStencilDecl(name, levels, entries.to[ListBuffer])
}

case class L4_BaseStencilDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var entries : ListBuffer[L4_StencilEntry]) extends L4_StencilDecl {

  override def prettyprint(out : PpStream) = {
    out << "Stencil " << name
    if (levels.isDefined) out << "@" << levels.get
    out << "{\n" <<< (entries, ",\n") << "\n}"
  }

  override def addToKnowledge() : Unit = {
    if (entries.isEmpty) {
      Logger.warn("Trying to add empty stencil to Knowledge - not supported")
      return
    }

    // TODO: check stencil - numDims for entries, stride, etc.
    val numDims = entries.map(_.numDims).max
    val colStride = entries.map(_.colStride).head

    val level = L4_LevelSpecification.asSingleLevel(levels)
    L4_StencilCollection.add(L4_Stencil(name, level, numDims, colStride, entries.map(_.asStencilMappingEntry)))
  }
}
