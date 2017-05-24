package exastencils.field.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_FieldFromOther extends Generatable {
  override def validLayers() = ListBuffer(L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/field/|LAYER_LC|/|LAYER_UC|_FieldFromOther.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.field.|LAYER_LC|

import exastencils.base.|LAYER_LC|._
import exastencils.core.Duplicate
import exastencils.prettyprinting._

/// |LAYER_UC|_FieldFromOther

case class |LAYER_UC|_FieldFromOther(var name : String, var levels : Option[|LAYER_UC|_LevelSpecification], var src : |LAYER_UC|_Access) extends |LAYER_UC|_FieldDecl {
  override def prettyprint(out : PpStream) = out << "Field" << ' ' << name << "@" << levels << ' ' << "from" << ' ' << src
  override def addToKnowledge() : Unit = {
    val destField = Duplicate.forceClone(src.asInstanceOf[|LAYER_UC|_FieldAccess].target)
    destField.name = name
    |LAYER_UC|_FieldCollection.add(destField)
  }
}
"""
  }
}
