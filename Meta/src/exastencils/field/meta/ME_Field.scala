package exastencils.field.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_Field extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/field/|LAYER_LC|/|LAYER_UC|_Field.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.field.|LAYER_LC|"""
    printer <<< """"""
    printer <<< """import exastencils.base.|LAYER_LC|._"""
    if (L3 == layer) {
      printer <<< """import exastencils.baseExt.|LAYER_LC|._"""
    }
    printer <<< """import exastencils.boundary.|LAYER_LC|.|LAYER_UC|_BoundaryCondition"""
    printer <<< """import exastencils.domain.|LAYER_LC|.|LAYER_UC|_Domain"""
    if (L3 == layer) {
      printer <<< """import exastencils.field.|NEXT_LC|._"""
    }
    if (L2 == layer) {
      printer <<< """import exastencils.field.|NEXT_LC|.|NEXT_UC|_Field"""
    }
    printer <<< """import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_LeveledKnowledgeObject"""
    printer <<< """import exastencils.prettyprinting.PpStream"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_Field"""
    printer <<< """"""
    printer <<< """case class |LAYER_UC|_Field("""
    printer <<< """    var name : String,"""
    printer <<< """    var level : Int,"""
    printer <<< """    var domain : |LAYER_UC|_Domain,"""
    printer <<< """    var datatype : |LAYER_UC|_Datatype,"""
    printer <<< """    var localization : String,"""
    printer <<< """    var initial : Option[|LAYER_UC|_Expression],"""
    printer <<< """    var boundary : |LAYER_UC|_BoundaryCondition) extends |LAYER_UC|_LeveledKnowledgeObject[|NEXT_UC|_Field] {"""
    printer <<< """"""
    if (L3 == layer) {
      printer <<< """  def printDatatype(dt : |LAYER_UC|_Datatype) : String = {"""
    }
    if (L2 == layer) {
      printer <<< """  def fieldLayoutName = s"defLayout$localization""""
    }
    if (L3 == layer) {
      printer <<< """    dt match {"""
      printer <<< """      case dt : |LAYER_UC|_ScalarDatatype                 => dt.prettyprint()"""
      printer <<< """      case |LAYER_UC|_ComplexDatatype(inner)              => "Complex" + printDatatype(inner)"""
      printer <<< """      case |LAYER_UC|_VectorDatatype(inner, count, isRow) => "Vec" + printDatatype(inner) + count + (if (isRow) "Row" else "")"""
      printer <<< """      case |LAYER_UC|_MatrixDatatype(inner, n, m)         => "Mat" + printDatatype(inner) + n + "x" + m"""
      printer <<< """    }"""
      printer <<< """  }"""
      printer <<< """"""
      printer <<< """  def fieldLayoutName = s"defLayoutFor_${ printDatatype(datatype) }_on_$localization""""
      printer <<< """"""
    }
    printer <<< """  override def prettyprintDecl(out : PpStream) : Unit = ???"""
    printer <<< """"""
    printer <<< """  override def progressImpl() = {"""
    printer <<< """    |NEXT_UC|_Field("""
    printer <<< """      name,"""
    printer <<< """      level,"""
    if (L2 == layer) {
      printer <<< """      domain.getProgressedObj(),"""
    }
    if (L3 == layer) {
      printer <<< """      -1, // index is to be set later"""
    }
    if (L3 == layer) {
      printer <<< """      domain.name,"""
    }
    if (L2 == layer) {
      printer <<< """      datatype.progress,"""
    }
    if (L3 == layer) {
      printer <<< """      |NEXT_UC|_FieldLayoutCollection.getByIdentifier(fieldLayoutName, level).get, // |LAYER_LC| field layout is not available -> grab |NEXT_LC| layout directly"""
    }
    if (L2 == layer) {
      printer <<< """      localization,"""
    }
    if (L3 == layer) {
      printer <<< """      1, // one slot by default - may be increased later"""
    }
    if (L2 == layer) {
      printer <<< """      |LAYER_UC|_ProgressOption(initial)(_.progress),"""
    }
    printer <<< """      boundary.progress)"""
    printer <<< """  }"""
    printer <<< """}"""
    printer.toString
  }
}
