package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_Statement
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_StencilFromDefault

trait L3_StencilFromDefault extends L3_Statement {
  def name : String
  def generateEntries() : ListBuffer[L3_StencilEntry]
}

/// L3_DefaultRestrictionStencil

object L3_DefaultRestrictionStencil {
  def apply(name : String, localization : String, interpolation : String) =
    new L3_DefaultRestrictionStencil(name, Knowledge.dimensionality, localization, interpolation)
}

case class L3_DefaultRestrictionStencil(
    var name : String,
    var numDims : Int,
    var localization : String,
    var interpolation : String) extends L3_StencilFromDefault {

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress l3 default stencil $name; this is not supported")

  def generateEntries() = L3_DefaultRestriction.generate(numDims, localization, interpolation)
}

/// L3_DefaultProlongationStencil

object L3_DefaultProlongationStencil {
  def apply(name : String, localization : String, interpolation : String) =
    new L3_DefaultProlongationStencil(name, Knowledge.dimensionality, localization, interpolation)
}

case class L3_DefaultProlongationStencil(
    var name : String,
    var numDims : Int,
    var localization : String,
    var interpolation : String) extends L3_StencilFromDefault {

  override def prettyprint(out : PpStream) = out << "Operator " << name << " from default restriction ( " << numDims << ", " << localization << ", " << interpolation << " )"
  override def progress = Logger.error(s"Trying to progress l3 default stencil $name; this is not supported")

  def generateEntries() = L3_DefaultProlongation.generate(numDims, localization, interpolation)
}
