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

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.prettyprinting._

/// L2_BaseStencilDecl

object L2_BaseStencilDecl {
  def apply(name : String, levels : Option[L2_LevelSpecification], entries : List[L2_StencilEntry]) =
    new L2_BaseStencilDecl(name, levels, entries.to[ListBuffer])
}

case class L2_BaseStencilDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var entries : ListBuffer[L2_StencilEntry]) extends L2_StencilDecl {

  override def prettyprint(out : PpStream) = {
    out << "Operator " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " from Stencil {\n" <<< (entries, "\n") << "\n}"
  }

  override def addToKnowledge() : Unit = {
    // TODO: check stencil - numDims for entries, stride, etc.
    val numDims = entries.map(_.numDims).max
    val colStride = entries.map(_.colStride).head

    val level = L2_LevelSpecification.asSingleLevel(levels)
    L2_StencilCollection.add(L2_Stencil(name, level, numDims, colStride, entries.map(_.asStencilMappingEntry)))
  }
}
