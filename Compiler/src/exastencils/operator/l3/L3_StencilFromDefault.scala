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

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
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

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress l3 default stencil $name; this is not supported")

  override def generateStencil() = L3_DefaultProlongation.generate(name, levels.get.resolveLevel, numDims, localization, interpolation)
  override def addToKnowledge() = L3_StencilCollection.add(generateStencil())
}
