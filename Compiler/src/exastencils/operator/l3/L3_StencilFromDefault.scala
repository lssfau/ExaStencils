package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_StencilFromDefault

abstract class L3_StencilFromDefault extends L3_StencilDecl {
  def name : String
  def generateEntries() : ListBuffer[L3_StencilEntry]
}

/// L3_DefaultRestrictionStencil

object L3_DefaultRestrictionStencil {
  def apply(name : String, localization : String, interpolation : String) =
    new L3_DefaultRestrictionStencil(name, None, Knowledge.dimensionality, localization, interpolation)
}

case class L3_DefaultRestrictionStencil(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var numDims : Int,
    var localization : String,
    var interpolation : String) extends L3_StencilFromDefault {

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress l3 default stencil $name; this is not supported")

  def generateEntries() = L3_DefaultRestriction.generate(numDims, localization, interpolation)

  override def addToKnowledge() : Unit = {
    val level = levels.get.asInstanceOf[L3_SingleLevel].level
    val entries = L3_DefaultRestriction.generate(numDims, localization, interpolation)/*FIXME*/ .map(_.asStencilMappingEntry)
    L3_StencilCollection.add(L3_Stencil(name, level, numDims, Array.fill(numDims)(2.0), entries))
  }
}

/// L3_DefaultProlongationStencil

object L3_DefaultProlongationStencil {
  def apply(name : String, localization : String, interpolation : String) =
    new L3_DefaultProlongationStencil(name, None, Knowledge.dimensionality, localization, interpolation)
}

case class L3_DefaultProlongationStencil(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var numDims : Int,
    var localization : String,
    var interpolation : String) extends L3_StencilFromDefault {

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress l3 default stencil $name; this is not supported")

  def generateEntries() = L3_DefaultProlongation.generate(numDims, localization, interpolation)

  override def addToKnowledge() : Unit = {
    val level = levels.get.asInstanceOf[L3_SingleLevel].level
    val entries = L3_DefaultProlongation.generate(numDims, localization, interpolation)/*FIXME*/ .map(_.asStencilMappingEntry)
    L3_StencilCollection.add(L3_Stencil(name, level, numDims, Array.fill(numDims)(0.5), entries))
  }
}
