package exastencils.field.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_Addition
import exastencils.base.l4.L4_Assignment
import exastencils.base.l4.L4_FunctionCall
import exastencils.base.l4.L4_GreaterEqual
import exastencils.base.l4.L4_IfCondition
import exastencils.base.l4.L4_Multiplication
import exastencils.base.l4.L4_PlainInternalFunctionReference
import exastencils.base.l4.L4_PlainVariableAccess
import exastencils.base.l4.L4_RealConstant
import exastencils.base.l4.L4_RealDatatype
import exastencils.base.l4.L4_Statement
import exastencils.base.l4.L4_Subtraction
import exastencils.base.l4.L4_VariableAccess
import exastencils.base.l4.L4_VariableDeclaration
import exastencils.baseExt.l4.L4_LoopOverField

object L4_SummationAlgorithms {

  def genKahanLoop(tmpVar : (L4_VariableAccess, L4_FieldFieldConvolution)) : ListBuffer[L4_Statement] = {
    // unpack
    var sum = tmpVar._1
    var dt = tmpVar._1.datatype
    var lhs = tmpVar._2.lhs
    var rhs = tmpVar._2.rhs
    var tmpNumber : Char = tmpVar._1.name.last
    // running compensation for lost lower order digits
    var compensation = L4_PlainVariableAccess(s"compensation_${ tmpVar._1.name.last }", dt, false)
    // kahan sum loop
    var y = L4_PlainVariableAccess(s"y_${tmpNumber}", dt, false)
    var t = L4_PlainVariableAccess(s"t_${tmpNumber}", dt, false)
    var body = ListBuffer[L4_Statement](
      L4_Assignment(y, L4_Subtraction(L4_Multiplication(lhs, rhs), compensation), "="),
      L4_Assignment(t, L4_Addition(sum, y), "="),
      L4_Assignment(compensation, L4_Subtraction(t, sum), "="),
      L4_Assignment(compensation, L4_Subtraction(compensation,y), "="),
      L4_Assignment(sum, t, "=")
    )

    ListBuffer[L4_Statement](
      L4_VariableDeclaration(s"compensation_${tmpNumber}", None, dt, Some(L4_RealConstant(0)), false),
      L4_VariableDeclaration(s"y_${tmpNumber}", None, dt, Some(L4_RealConstant(0)), false),
      L4_VariableDeclaration(s"t_${tmpNumber}", None, dt, Some(L4_RealConstant(0)), false),
      L4_LoopOverField(lhs, body)
    )
  }

  def genNeumaierLoop(tmpVar : (L4_VariableAccess, L4_FieldFieldConvolution)) : ListBuffer[L4_Statement] = {
    // unpack
    var sum = tmpVar._1
    var dt = tmpVar._1.datatype
    var lhs = tmpVar._2.lhs
    var rhs = tmpVar._2.rhs
    var tmpNumber : Char = tmpVar._1.name.last
    // running compensation for lost lower order digits
    var compensation = L4_PlainVariableAccess(s"compensation_${ tmpVar._1.name.last }", dt, false)
    // neumaier sum loop
    var t = L4_PlainVariableAccess(s"t_${tmpNumber}", dt, false)
    var tmpMult = L4_PlainVariableAccess("tmpMult", dt, false)
    var tmpAdd = L4_PlainVariableAccess("tmpAdd", dt, false)
    var body = ListBuffer[L4_Statement](
      L4_Assignment(t, L4_Addition(sum, L4_Multiplication(lhs, rhs)), "="),
      L4_VariableDeclaration("tmpMult", None, dt, Some(L4_Multiplication(lhs, rhs)), false),
      L4_IfCondition(L4_GreaterEqual(L4_FunctionCall(L4_PlainInternalFunctionReference("abs", L4_RealDatatype), sum), L4_FunctionCall(L4_PlainInternalFunctionReference("abs", L4_RealDatatype), tmpMult)), ListBuffer[L4_Statement](
        L4_VariableDeclaration("tmpAdd", None, dt, Some(L4_Subtraction(sum, t)), false),
        L4_Assignment(compensation, L4_Addition(tmpAdd, tmpMult), "+="),
      ),
        ListBuffer[L4_Statement](
          L4_VariableDeclaration("tmpAdd", None, dt, Some(L4_Subtraction(tmpMult, t)), false),
          L4_Assignment(compensation, L4_Addition(tmpAdd, sum), "+=")
        )),
      L4_Assignment(sum, t, "=")

    )

    ListBuffer[L4_Statement](
      L4_VariableDeclaration(s"compensation_${tmpNumber}", None, dt, Some(L4_RealConstant(0)), false),
      L4_VariableDeclaration(s"t_${tmpNumber}", None, dt, Some(L4_RealConstant(0)), false),
      L4_LoopOverField(lhs, body),
      L4_Assignment(sum, L4_Addition(sum, compensation), "=")
    )
  }

}
