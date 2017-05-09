package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_Statement extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4, IR)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_Statement.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.base.|LAYER_LC|"""
    printer <<< """"""
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """import exastencils.base.|NEXT_LC|._"""
    }
    printer <<< """import exastencils.prettyprinting._"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_Statement"""
    printer <<< """"""
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """trait |LAYER_UC|_Statement extends |LAYER_UC|_Node with |LAYER_UC|_Progressable with PrettyPrintable {"""
    }
    if (IR == layer) {
      printer <<< """trait |LAYER_UC|_Statement extends |LAYER_UC|_Node with PrettyPrintable"""
    }
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """  def progress : |NEXT_UC|_Statement"""
      printer <<< """}"""
    }
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_NullStatement"""
    printer <<< """"""
    printer <<< """case object |LAYER_UC|_NullStatement extends |LAYER_UC|_Statement {"""
    printer <<< """  exastencils.core.Duplicate.registerConstant(this)"""
    printer <<< """"""
    printer <<< """  override def prettyprint(out : PpStream) : Unit = {}"""
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_NullStatement"""
    }
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_ExpressionStatement"""
    printer <<< """"""
    printer <<< """case class |LAYER_UC|_ExpressionStatement(var expression : |LAYER_UC|_Expression) extends |LAYER_UC|_Statement {"""
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """  override def prettyprint(out : PpStream) : Unit = out << expression"""
    }
    if (IR == layer) {
      printer <<< """  override def prettyprint(out : PpStream) : Unit = out << expression << ';'"""
    }
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_ExpressionStatement(expression.progress)"""
    }
    printer <<< """}"""
    printer.toString
  }
}
