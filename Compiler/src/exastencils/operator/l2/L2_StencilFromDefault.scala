package exastencils.operator.l2

import exastencils.base.l2._
import exastencils.config.Knowledge
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
    new L2_DefaultRestrictionStencil(name, levels, Knowledge.dimensionality, localization, interpolation)
}

case class L2_DefaultRestrictionStencil(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var numDims : Int,
    var localization : String,
    var interpolation : String) extends L2_StencilFromDefault {

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress l2 default stencil $name; this is not supported")

  override def generateStencil() = {
    L2_DefaultRestriction.generate(name, levels.get.asInstanceOf[L2_SingleLevel].level,
      numDims, localization, interpolation)
  }

  override def addToKnowledge() = L2_StencilCollection.add(generateStencil())
}

/// L2_DefaultProlongationStencil

object L2_DefaultProlongationStencil {
  def apply(name : String, levels : Option[L2_LevelSpecification], localization : String, interpolation : String) =
    new L2_DefaultProlongationStencil(name, levels, Knowledge.dimensionality, localization, interpolation)
}

case class L2_DefaultProlongationStencil(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var numDims : Int,
    var localization : String,
    var interpolation : String) extends L2_StencilFromDefault {

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress l2 default stencil $name; this is not supported")

  override def generateStencil() = {
    L2_DefaultProlongation.generate(name, levels.get.asInstanceOf[L2_SingleLevel].level,
      numDims, localization, interpolation)
  }

  override def addToKnowledge() = L2_StencilCollection.add(generateStencil())
}
