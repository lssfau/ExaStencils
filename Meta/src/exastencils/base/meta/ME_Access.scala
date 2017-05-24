package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_Access extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_Access.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.base.|LAYER_LC|

import exastencils.base.|NEXT_LC|._
import exastencils.prettyprinting._

/// |LAYER_UC|_Access

trait |LAYER_UC|_Access extends |LAYER_UC|_Expression {
  def name : String
  override def progress : |NEXT_UC|_Access
}

/// |LAYER_UC|_VariableAccess

case class |LAYER_UC|_VariableAccess(var name : String, /*TODO: level */ var datatype : |LAYER_UC|_Datatype) extends |LAYER_UC|_Access {
  override def prettyprint(out : PpStream) : Unit = out << name
  override def progress = |NEXT_UC|_VariableAccess(name, datatype.progress)
}
"""
  }
}
