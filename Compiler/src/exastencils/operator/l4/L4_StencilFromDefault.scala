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

import exastencils.base.l4._
import exastencils.config.Knowledge
import exastencils.grid.l4.L4_Localization
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_StencilFromDefault

abstract class L4_StencilFromDefault extends L4_StencilDecl {
  def name : String
  def generateStencil() : L4_Stencil
}

/// L4_DefaultRestrictionStencil

object L4_DefaultRestrictionStencil {
  def apply(name : String, levels : Option[L4_DeclarationLevelSpecification], localization : String, interpolation : String) =
    new L4_DefaultRestrictionStencil(name, levels, Knowledge.dimensionality, L4_Localization.resolve(localization), interpolation)
}

case class L4_DefaultRestrictionStencil(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var numDims : Int,
    var localization : L4_Localization,
    var interpolation : String) extends L4_StencilFromDefault {

  override def prettyprint(out : PpStream) = {
    out << "Stencil " << name << " from default restriction on " << localization << " with \"" << interpolation << '"'
  }

  override def progress = Logger.error(s"Trying to progress l4 default stencil $name; this is not supported")

  override def generateStencil() = L4_DefaultRestriction.generate(name, levels.get.resolveLevel, numDims, localization, interpolation)
  override def addToKnowledge() = L4_StencilCollection.add(generateStencil())
}

/// L4_DefaultProlongationStencil

object L4_DefaultProlongationStencil {
  def apply(name : String, levels : Option[L4_DeclarationLevelSpecification], localization : String, interpolation : String) =
    new L4_DefaultProlongationStencil(name, levels, Knowledge.dimensionality, L4_Localization.resolve(localization), interpolation)
}

case class L4_DefaultProlongationStencil(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var numDims : Int,
    var localization : L4_Localization,
    var interpolation : String) extends L4_StencilFromDefault {

  override def prettyprint(out : PpStream) = {
    out << "Stencil " << name << " from default prolongation on " << localization << " with \"" << interpolation << '"'
  }

  override def progress = Logger.error(s"Trying to progress l4 default stencil $name; this is not supported")

  override def generateStencil() = L4_DefaultProlongation.generate(name, levels.get.resolveLevel, numDims, localization, interpolation)
  override def addToKnowledge() = L4_StencilCollection.add(generateStencil())
}
