package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_Expression extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4, IR)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_Expression.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.base.|LAYER_LC|"""
    printer <<< """"""
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """import exastencils.base.|NEXT_LC|._"""
    }
    printer <<< """import exastencils.prettyprinting._"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_Expression"""
    printer <<< """"""
    if (IR == layer) {
      printer <<< """trait |LAYER_UC|_Expression extends |LAYER_UC|_Node with PrettyPrintable {"""
    }
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """trait |LAYER_UC|_Expression extends |LAYER_UC|_Node with |LAYER_UC|_Progressable with PrettyPrintable {"""
    }
    if (IR == layer) {
      printer <<< """  def datatype : |LAYER_UC|_Datatype"""
    }
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """  def progress : |NEXT_UC|_Expression"""
    }
    printer <<< """"""
    printer <<< """  def +(other : |LAYER_UC|_Expression) = |LAYER_UC|_Addition(this, other)"""
    printer <<< """  def -(other : |LAYER_UC|_Expression) = |LAYER_UC|_Subtraction(this, other)"""
    printer <<< """  def *(other : |LAYER_UC|_Expression) = |LAYER_UC|_Multiplication(this, other)"""
    printer <<< """  def /(other : |LAYER_UC|_Expression) = |LAYER_UC|_Division(this, other)"""
    printer <<< """"""
    printer <<< """  def Pow(other : |LAYER_UC|_Expression) = |LAYER_UC|_Power(this, other)"""
    printer <<< """  def Mod(other : |LAYER_UC|_Expression) = |LAYER_UC|_Modulo(this, other)"""
    printer <<< """  def Modulo(other : |LAYER_UC|_Expression) = |LAYER_UC|_Modulo(this, other)"""
    printer <<< """"""
    printer <<< """  // Scala does not allow .{+,-,*,/} and fails with Dot{+,-,*,/}"""
    printer <<< """  def :+(other : |LAYER_UC|_Expression) = |LAYER_UC|_ElementwiseAddition(this, other)"""
    printer <<< """  def :-(other : |LAYER_UC|_Expression) = |LAYER_UC|_ElementwiseSubtraction(this, other)"""
    printer <<< """  def :*(other : |LAYER_UC|_Expression) = |LAYER_UC|_ElementwiseMultiplication(this, other)"""
    printer <<< """  def :/(other : |LAYER_UC|_Expression) = |LAYER_UC|_ElementwiseDivision(this, other)"""
    printer <<< """"""
    printer <<< """  def DotPow(other : |LAYER_UC|_Expression) = |LAYER_UC|_ElementwisePower(this, other)"""
    printer <<< """  def DotMod(other : |LAYER_UC|_Expression) = |LAYER_UC|_ElementwiseModulo(this, other)"""
    printer <<< """  def DotModulo(other : |LAYER_UC|_Expression) = |LAYER_UC|_ElementwiseModulo(this, other)"""
    printer <<< """"""
    printer <<< """  def And(other : |LAYER_UC|_Expression) = |LAYER_UC|_AndAnd(this, other)"""
    printer <<< """  def AndAnd(other : |LAYER_UC|_Expression) = |LAYER_UC|_AndAnd(this, other)"""
    printer <<< """  def Or(other : |LAYER_UC|_Expression) = |LAYER_UC|_OrOr(this, other)"""
    printer <<< """  def OrOr(other : |LAYER_UC|_Expression) = |LAYER_UC|_OrOr(this, other)"""
    printer <<< """"""
    printer <<< """  def EqEq(other : |LAYER_UC|_Expression) = |LAYER_UC|_EqEq(this, other)"""
    printer <<< """  def Neq(other : |LAYER_UC|_Expression) = |LAYER_UC|_Neq(this, other)"""
    printer <<< """  def <(other : |LAYER_UC|_Expression) = |LAYER_UC|_Lower(this, other)"""
    printer <<< """  def <=(other : |LAYER_UC|_Expression) = |LAYER_UC|_LowerEqual(this, other)"""
    printer <<< """  def >(other : |LAYER_UC|_Expression) = |LAYER_UC|_Greater(this, other)"""
    printer <<< """  def >=(other : |LAYER_UC|_Expression) = |LAYER_UC|_GreaterEqual(this, other)"""
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """}"""
    }
    printer <<< """"""
    if (IR == layer) {
      printer <<< """  def <<(other : |LAYER_UC|_Expression) = |LAYER_UC|_LeftShift(this, other)"""
      printer <<< """}"""
      printer <<< """"""
    }
    printer <<< """/// |LAYER_UC|_NullExpression"""
    printer <<< """"""
    printer <<< """case object |LAYER_UC|_NullExpression extends |LAYER_UC|_Expression {"""
    printer <<< """  exastencils.core.Duplicate.registerConstant(this)"""
    if (IR == layer) {
      printer <<< """  override def datatype = |LAYER_UC|_UnitDatatype"""
    }
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """"""
    }
    printer <<< """  override def prettyprint(out : PpStream) : Unit = {}"""
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_NullExpression"""
    }
    printer <<< """}"""
    printer.toString
  }
}
