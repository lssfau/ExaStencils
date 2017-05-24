package exastencils.field.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_BaseFieldDecl extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/field/|LAYER_LC|/|LAYER_UC|_BaseFieldDecl.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.field.|LAYER_LC|

import exastencils.base.|LAYER_LC|._
import exastencils.boundary.|LAYER_LC|.|LAYER_UC|_NoBC
import exastencils.domain.|LAYER_LC|.|LAYER_UC|_DomainCollection
import exastencils.prettyprinting._

/// |LAYER_UC|_BaseFieldDecl

object |LAYER_UC|_BaseFieldDecl {
  def apply(identifier : String, levels : Option[|LAYER_UC|_LevelSpecification], datatype : Option[|LAYER_UC|_Datatype], localization : String, domain : String, initial : Option[|LAYER_UC|_Expression]) : |LAYER_UC|_BaseFieldDecl =
    |LAYER_UC|_BaseFieldDecl(identifier, levels, datatype.getOrElse(|LAYER_UC|_RealDatatype), localization, domain, initial)
}

case class |LAYER_UC|_BaseFieldDecl(
    var name : String,
    var levels : Option[|LAYER_UC|_LevelSpecification],
    var datatype : |LAYER_UC|_Datatype,
    var localization : String,
    var domain : String,
    var initial : Option[|LAYER_UC|_Expression]) extends |LAYER_UC|_FieldDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"

  override def addToKnowledge() : Unit = {
    |LAYER_UC|_FieldCollection.add(
      |LAYER_UC|_Field(
        name,
        |LAYER_UC|_LevelSpecification.asSingleLevel(levels),
        |LAYER_UC|_DomainCollection.getByIdentifier(domain).get,
        datatype,
        localization,
        initial,
        |LAYER_UC|_NoBC))
  }
}
"""
  }
}
