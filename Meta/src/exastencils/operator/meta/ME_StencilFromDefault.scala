package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_StencilFromDefault extends Generatable {
  override def validLayers() = ListBuffer(L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_StencilFromDefault.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.operator.|LAYER_LC|

import scala.collection.mutable.ListBuffer

import exastencils.base.|LAYER_LC|._
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// |LAYER_UC|_StencilFromDefault

abstract class |LAYER_UC|_StencilFromDefault extends |LAYER_UC|_StencilDecl {
  def name : String
  def generateEntries() : ListBuffer[|LAYER_UC|_StencilEntry]
}

/// |LAYER_UC|_DefaultRestrictionStencil

object |LAYER_UC|_DefaultRestrictionStencil {
  def apply(name : String, localization : String, interpolation : String) =
    new |LAYER_UC|_DefaultRestrictionStencil(name, None, Knowledge.dimensionality, localization, interpolation)
}

case class |LAYER_UC|_DefaultRestrictionStencil(
    var name : String,
    var levels : Option[|LAYER_UC|_LevelSpecification],
    var numDims : Int,
    var localization : String,
    var interpolation : String) extends |LAYER_UC|_StencilFromDefault {

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress |LAYER_LC| default stencil $name; this is not supported")

  def generateEntries() = |LAYER_UC|_DefaultRestriction.generate(numDims, localization, interpolation)

  override def addToKnowledge() : Unit = {
    val level = levels.get.asInstanceOf[|LAYER_UC|_SingleLevel].level
    val entries = |LAYER_UC|_DefaultRestriction.generate(numDims, localization, interpolation)/*FIXME*/ .map(_.asStencilMappingEntry)
    |LAYER_UC|_StencilCollection.add(|LAYER_UC|_Stencil(name, level, numDims, Array.fill(numDims)(2.0), entries))
  }
}

/// |LAYER_UC|_DefaultProlongationStencil

object |LAYER_UC|_DefaultProlongationStencil {
  def apply(name : String, localization : String, interpolation : String) =
    new |LAYER_UC|_DefaultProlongationStencil(name, None, Knowledge.dimensionality, localization, interpolation)
}

case class |LAYER_UC|_DefaultProlongationStencil(
    var name : String,
    var levels : Option[|LAYER_UC|_LevelSpecification],
    var numDims : Int,
    var localization : String,
    var interpolation : String) extends |LAYER_UC|_StencilFromDefault {

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress |LAYER_LC| default stencil $name; this is not supported")

  def generateEntries() = |LAYER_UC|_DefaultProlongation.generate(numDims, localization, interpolation)

  override def addToKnowledge() : Unit = {
    val level = levels.get.asInstanceOf[|LAYER_UC|_SingleLevel].level
    val entries = |LAYER_UC|_DefaultProlongation.generate(numDims, localization, interpolation)/*FIXME*/ .map(_.asStencilMappingEntry)
    |LAYER_UC|_StencilCollection.add(|LAYER_UC|_Stencil(name, level, numDims, Array.fill(numDims)(0.5), entries))
  }
}
"""
  }
}
