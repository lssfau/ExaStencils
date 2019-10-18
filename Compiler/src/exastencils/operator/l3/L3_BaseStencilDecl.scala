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
import exastencils.prettyprinting._

/// L3_BaseStencilDecl

object L3_BaseStencilDecl {
  def apply(name : String, levels : Option[L3_LevelSpecification], entries : List[L3_StencilEntry]) =
    new L3_BaseStencilDecl(name, levels, entries.to[ListBuffer])
}

case class L3_BaseStencilDecl(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var entries : ListBuffer[L3_StencilEntry]) extends L3_StencilDecl {

  override def prettyprint(out : PpStream) = {
    out << "Operator " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " from Stencil {\n" <<< (entries, "\n") << "\n}"
  }

  override def addToKnowledge() : Unit = {
    // TODO: check stencil - numDims for entries, stride, etc.
    val numDims = entries.map(_.numDims).max
    val colStride = entries.map(_.colStride).head

    val level = L3_LevelSpecification.asSingleLevel(levels)
    L3_StencilCollection.add(L3_Stencil(name, level, numDims, colStride, entries.map(_.asStencilMappingEntry)))
  }
}
