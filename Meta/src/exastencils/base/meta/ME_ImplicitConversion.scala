package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_ImplicitConversion extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4, IR)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_ImplicitConversion.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.base.|LAYER_LC|"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_ImplicitConversion {"""
    printer <<< """"""
    printer <<< """  import scala.language.implicitConversions"""
    printer <<< """"""
    printer <<< """  // constants"""
    printer <<< """"""
    printer <<< """  implicit def NumberToIntegerConstant(n : Int) : |LAYER_UC|_IntegerConstant = |LAYER_UC|_IntegerConstant(n)"""
    printer <<< """  implicit def NumberToIntegerConstant(n : Long) : |LAYER_UC|_IntegerConstant = |LAYER_UC|_IntegerConstant(n)"""
    printer <<< """  implicit def NumberToFloatConstant(n : Float) : |LAYER_UC|_RealConstant = |LAYER_UC|_RealConstant(n)"""
    printer <<< """  implicit def NumberToFloatConstant(n : Double) : |LAYER_UC|_RealConstant = |LAYER_UC|_RealConstant(n)"""
    printer <<< """"""
    printer <<< """  implicit def BooleanToBooleanConstant(b : Boolean) : |LAYER_UC|_BooleanConstant = |LAYER_UC|_BooleanConstant(b)"""
    printer <<< """"""
    printer <<< """  implicit def StringToStringLiteral(s : String) : |LAYER_UC|_StringLiteral = |LAYER_UC|_StringLiteral(s)"""
    printer <<< """"""
    printer <<< """  // expression -> statement"""
    printer <<< """"""
    printer <<< """  implicit def ExpressionToExpressionStatement(e : |LAYER_UC|_Expression) : |LAYER_UC|_Statement = |LAYER_UC|_ExpressionStatement(e)"""
    printer <<< """"""
    printer <<< """  // datatype"""
    printer <<< """"""
    printer <<< """  implicit def StringToDatatype(s : String) : |LAYER_UC|_Datatype = |LAYER_UC|_SpecialDatatype(s)"""
    if (IR == layer) {
      printer <<< """"""
      printer <<< """  // deprecated"""
      printer <<< """"""
      printer <<< """  @deprecated("should be removed completely; please, don't use it in new code", "15.09.2016")"""
      printer <<< """  implicit def StringToStatement(s : String) : |LAYER_UC|_Statement = |LAYER_UC|_ExpressionStatement(|LAYER_UC|_StringLiteral(s))"""
    }
    printer <<< """}"""
    printer.toString
  }
}
