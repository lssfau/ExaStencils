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

import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.grid.l3.L3_Localization
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_StencilFromDefault

abstract class L3_StencilFromDefault extends L3_StencilDecl {
  def name : String
  def generateStencil() : L3_Stencil
}

/// L3_DefaultRestrictionStencil

object L3_DefaultRestrictionStencil {
  def apply(name : String, levels : Option[L3_LevelSpecification], localization : String, interpolation : String) =
    new L3_DefaultRestrictionStencil(name, levels, Knowledge.dimensionality, L3_Localization.resolve(localization), interpolation)
}

case class L3_DefaultRestrictionStencil(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var numDims : Int,
    var localization : L3_Localization,
    var interpolation : String) extends L3_StencilFromDefault {

  override def prettyprint(out : PpStream) = {
    out << "Stencil " << name << " from default restriction on " << localization << " with \"" << interpolation << '"'
  }

  override def progress = Logger.error(s"Trying to progress l3 default stencil $name; this is not supported")

  override def generateStencil() = L3_DefaultRestriction.generate(name, levels.get.resolveLevel, numDims, localization, interpolation)
  override def addToKnowledge() = L3_StencilCollection.add(generateStencil())
}

/// L3_DefaultProlongationStencil

object L3_DefaultProlongationStencil {
  def apply(name : String, levels : Option[L3_LevelSpecification], localization : String, interpolation : String) =
    new L3_DefaultProlongationStencil(name, levels, Knowledge.dimensionality, L3_Localization.resolve(localization), interpolation)
}

case class L3_DefaultProlongationStencil(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var numDims : Int,
    var localization : L3_Localization,
    var interpolation : String) extends L3_StencilFromDefault {

  override def prettyprint(out : PpStream) = {
    out << "Stencil " << name << " from default prolongation on " << localization << " with \"" << interpolation << '"'
  }

  override def progress = Logger.error(s"Trying to progress l3 default stencil $name; this is not supported")

  override def generateStencil() = L3_DefaultProlongation.generate(name, levels.get.resolveLevel, numDims, localization, interpolation)
  override def addToKnowledge() = L3_StencilCollection.add(generateStencil())
}
