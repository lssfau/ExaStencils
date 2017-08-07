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
  def apply(name : String, levels : Option[L4_LevelSpecification], localization : String, interpolation : String) =
    new L4_DefaultRestrictionStencil(name, levels, Knowledge.dimensionality, L4_Localization.resolve(localization), interpolation)
}

case class L4_DefaultRestrictionStencil(
    var name : String,
    var levels : Option[L4_LevelSpecification],
    var numDims : Int,
    var localization : L4_Localization,
    var interpolation : String) extends L4_StencilFromDefault {

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress l4 default stencil $name; this is not supported")

  override def generateStencil() = L4_DefaultRestriction.generate(name, levels.get.resolveLevel, numDims, localization, interpolation)
  override def addToKnowledge() = L4_StencilCollection.add(generateStencil())
}

/// L4_DefaultProlongationStencil

object L4_DefaultProlongationStencil {
  def apply(name : String, levels : Option[L4_LevelSpecification], localization : String, interpolation : String) =
    new L4_DefaultProlongationStencil(name, levels, Knowledge.dimensionality, L4_Localization.resolve(localization), interpolation)
}

case class L4_DefaultProlongationStencil(
    var name : String,
    var levels : Option[L4_LevelSpecification],
    var numDims : Int,
    var localization : L4_Localization,
    var interpolation : String) extends L4_StencilFromDefault {

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress l4 default stencil $name; this is not supported")

  override def generateStencil() = L4_DefaultProlongation.generate(name, levels.get.resolveLevel, numDims, localization, interpolation)
  override def addToKnowledge() = L4_StencilCollection.add(generateStencil())
}
