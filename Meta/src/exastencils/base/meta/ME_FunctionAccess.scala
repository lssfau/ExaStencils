package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_FunctionAccess extends Generatable {
  override def validLayers() = ListBuffer(IR)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_FunctionAccess.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.base.|LAYER_LC|

import exastencils.prettyprinting.PpStream

/// |LAYER_UC|_FunctionAccess

trait |LAYER_UC|_FunctionAccess extends |LAYER_UC|_Access {
  // name is read/write
  var name : String
  def datatype : |LAYER_UC|_Datatype

  override def prettyprint(out : PpStream) = out << name
}

/// |LAYER_UC|_UserFunctionAccess

case class |LAYER_UC|_UserFunctionAccess(var name : String, var datatype : |LAYER_UC|_Datatype) extends |LAYER_UC|_FunctionAccess
"""
  }
}
