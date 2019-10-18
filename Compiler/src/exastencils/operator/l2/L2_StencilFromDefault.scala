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

import exastencils.base.l2._
import exastencils.config.Knowledge
import exastencils.grid.l2.L2_Localization
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_StencilFromDefault

abstract class L2_StencilFromDefault extends L2_StencilDecl {
  def name : String
  def generateStencil() : L2_Stencil
}

/// L2_DefaultRestrictionStencil

object L2_DefaultRestrictionStencil {
  def apply(name : String, levels : Option[L2_LevelSpecification], localization : String, interpolation : String) =
    new L2_DefaultRestrictionStencil(name, levels, Knowledge.dimensionality, L2_Localization.resolve(localization), interpolation)
}

case class L2_DefaultRestrictionStencil(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var numDims : Int,
    var localization : L2_Localization,
    var interpolation : String) extends L2_StencilFromDefault {

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress l2 default stencil $name; this is not supported")

  override def generateStencil() = L2_DefaultRestriction.generate(name, levels.get.resolveLevel, numDims, localization, interpolation)
  override def addToKnowledge() = L2_StencilCollection.add(generateStencil())
}

/// L2_DefaultProlongationStencil

object L2_DefaultProlongationStencil {
  def apply(name : String, levels : Option[L2_LevelSpecification], localization : String, interpolation : String) =
    new L2_DefaultProlongationStencil(name, levels, Knowledge.dimensionality, L2_Localization.resolve(localization), interpolation)
}

case class L2_DefaultProlongationStencil(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var numDims : Int,
    var localization : L2_Localization,
    var interpolation : String) extends L2_StencilFromDefault {

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress l2 default stencil $name; this is not supported")

  override def generateStencil() = L2_DefaultProlongation.generate(name, levels.get.resolveLevel, numDims, localization, interpolation)
  override def addToKnowledge() = L2_StencilCollection.add(generateStencil())
}
