//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer
import scala.math.pow

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.ir._

/* ###################################################################################################################
################                 Implementation of tensor functions                        ##########################
################################################################################################################### */

/** Declaration of tensor functions and transformation nodes */
object IR_ResolveUserDefinedTensor2Functions extends DefaultStrategy("Resolve user defined functions") {
  var resolveFunctions = ListBuffer[String]()
  this.onBefore = () => {
    resolveFunctions.clear()
    resolveFunctions ++= ListBuffer(
      "dotProduct",
      "dotp",
      "deter",
      "determinant",
      "getElement",
      "setElement",
      "inverse",
      "dyadic",
      "trace",
      "add",
      "str",
      "compare"
    )
  }

  this += new Transformation("add assignments/decl to function returns to arguments", {

    // Tensor 1 Assignment of function return
    case IR_Assignment(dest, src : IR_FunctionCall, "=") if (!resolveFunctions.contains(src.name) &&
      dest.datatype.isInstanceOf[IR_TensorDatatype1] && src.datatype.isInstanceOf[IR_TensorDatatype1])            =>
      src.arguments += dest
      IR_ExpressionStatement(src)
    case IR_VariableDeclaration(datatype, name, Some(src : IR_FunctionCall), _) if
      (!resolveFunctions.contains(src.name) && datatype.isInstanceOf[IR_TensorDatatype1] &&
        src.datatype.isInstanceOf[IR_TensorDatatype1])                                                            =>
      val decl = IR_VariableDeclaration(datatype, name, None)
      src.arguments += IR_VariableAccess(decl)
      ListBuffer[IR_Statement](
        decl,
        IR_ExpressionStatement(src)
      )
    case IR_Assignment(dest, src : IR_FunctionCall, "+=") if (!resolveFunctions.contains(src.name) &&
      dest.datatype.isInstanceOf[IR_TensorDatatype1] && src.datatype.isInstanceOf[IR_TensorDatatype1])            =>
      Logger.error("+= tensor1 operator resolution not yet implemented")

    // Tensor 2 Assignment of function return
    case IR_Assignment(dest, src : IR_FunctionCall, "=") if (!resolveFunctions.contains(src.name) &&
      dest.datatype.isInstanceOf[IR_TensorDatatype2] && src.datatype.isInstanceOf[IR_TensorDatatype2])            =>
      src.arguments += dest
      IR_ExpressionStatement(src)
    case IR_VariableDeclaration(datatype, name, Some(src : IR_FunctionCall), _) if
      (!resolveFunctions.contains(src.name) && datatype.isInstanceOf[IR_TensorDatatype2] &&
      src.datatype.isInstanceOf[IR_TensorDatatype2])                                                              =>
      val decl = IR_VariableDeclaration(datatype, name, None)
      src.arguments += IR_VariableAccess(decl)
      ListBuffer[IR_Statement](
        decl,
        IR_ExpressionStatement(src)
      )
    case IR_Assignment(dest, src : IR_FunctionCall, "+=") if (!resolveFunctions.contains(src.name) &&
      dest.datatype.isInstanceOf[IR_TensorDatatype2] && src.datatype.isInstanceOf[IR_TensorDatatype2])          =>
      Logger.error("+= tensor2 operator resolution not yet implemented")

    // Tensor N Assignment of function return
    case IR_Assignment(dest, src : IR_FunctionCall, "=") if (!resolveFunctions.contains(src.name) &&
      dest.datatype.isInstanceOf[IR_TensorDatatypeN] && src.datatype.isInstanceOf[IR_TensorDatatypeN])          =>
      src.arguments += dest
      IR_ExpressionStatement(src)
    case IR_VariableDeclaration(datatype, name, Some(src : IR_FunctionCall), _) if
      (!resolveFunctions.contains(src.name) && datatype.isInstanceOf[IR_TensorDatatypeN] &&
        src.datatype.isInstanceOf[IR_TensorDatatypeN])                                                          =>
      val decl = IR_VariableDeclaration(datatype, name, None)
      src.arguments += IR_VariableAccess(decl)
      ListBuffer[IR_Statement](
        decl,
        IR_ExpressionStatement(src)
      )
    case IR_Assignment(dest, src : IR_FunctionCall, "+=") if (!resolveFunctions.contains(src.name) &&
      dest.datatype.isInstanceOf[IR_TensorDatatypeN] && src.datatype.isInstanceOf[IR_TensorDatatypeN])          =>
      Logger.error("+= tensorN operator resolution not yet implemented")
  })

  this += new Transformation("parameters and return types", {

    // Tensor 1 return types
    case arg : IR_FunctionArgument if (arg.datatype.isInstanceOf[IR_TensorDatatype1])                           =>
      arg.datatype = IR_ReferenceDatatype(arg.datatype)
      arg
    case func : IR_Function if (!resolveFunctions.contains(func.name) &&
      func.datatype.isInstanceOf[IR_TensorDatatype1])                                                           =>
      val tensor = func.datatype.asInstanceOf[IR_TensorDatatype1]
      func.parameters += IR_FunctionArgument("_tensor1_return", IR_ReferenceDatatype(tensor))
      func.datatype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp)) if (exp.datatype.isInstanceOf[IR_TensorDatatype1]) =>
          List(
            IR_Assignment(IR_VariableAccess("_tensor1_return", tensor), exp),
            IR_Return())
        case _                                                                      => List(stmt)
      })
      func

    // Tensor 2 return types
    case arg : IR_FunctionArgument if (arg.datatype.isInstanceOf[IR_TensorDatatype2]) =>
      arg.datatype = IR_ReferenceDatatype(arg.datatype)
      arg
    case func : IR_Function if (!resolveFunctions.contains(func.name) &&
      func.datatype.isInstanceOf[IR_TensorDatatype2])                                 =>
      val tensor = func.datatype.asInstanceOf[IR_TensorDatatype2]
      func.parameters += IR_FunctionArgument("_tensor2_return", IR_ReferenceDatatype(tensor))
      func.datatype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp)) if (exp.datatype.isInstanceOf[IR_TensorDatatype2])  =>
          List(
            IR_Assignment(IR_VariableAccess("_tensor2_return", tensor), exp),
            IR_Return())
        case _                                                                        => List(stmt)
      })
      func

    // Tensor N return types
    case arg : IR_FunctionArgument if (arg.datatype.isInstanceOf[IR_TensorDatatypeN]) =>
      arg.datatype = IR_ReferenceDatatype(arg.datatype)
      arg
    case func : IR_Function if (!resolveFunctions.contains(func.name) &&
      func.datatype.isInstanceOf[IR_TensorDatatypeN])                                =>
      val tensor = func.datatype.asInstanceOf[IR_TensorDatatypeN]
      func.parameters += IR_FunctionArgument("_tensorN_return", IR_ReferenceDatatype(tensor))
      func.datatype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp)) if (exp.datatype.isInstanceOf[IR_TensorDatatypeN]) =>
          List(
            IR_Assignment(IR_VariableAccess("_tensorN_return", tensor), exp),
            IR_Return())
        case _                                                                        => List(stmt)
      })
      func
  })

}

/** Implementation of tensor functions. Separate function for each kind. */
object IR_ResolveTensorFunctions extends DefaultStrategy("Resolve special tensor functions") {

  //################################################################################################################
  // Element access

  /** Element access for tensor 1,2,N
   *
   * @param exp : IR_Expression, input expression for access
   * @param row : Integer, Tensor1 only index, Tensor2 row index, TensorN direct index
   * @param col : Integer, Tensor2 col index
   * @param higherInd : List[Integer], TensorN multidim index
   * @return : IR_Expression, expression found on given index
   */
  def getElem(exp : IR_Expression, row : Integer, col : Integer, higherInd: List[Integer]) : IR_Expression = {
    exp match {
      case x : IR_TensorExpression1                                               => x.get(row)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorExpression1])   => IR_HighDimAccess(Duplicate(x),
        new IR_ConstIndex(Array(row)))
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorDatatype1])     => IR_HighDimAccess(Duplicate(x),
        new IR_ConstIndex(Array(row)))
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_VectorDatatype])     => IR_HighDimAccess(Duplicate(x),
        new IR_ConstIndex(Array(row)))
      case x : IR_TensorExpression2                                               => x.get(row, col)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorExpression2])   => IR_HighDimAccess(Duplicate(x),
        new IR_ConstIndex(Array(row + col*3)))
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorDatatype2])     => IR_HighDimAccess(Duplicate(x),
        new IR_ConstIndex(Array(row + col*3)))
      case x : IR_TensorExpressionN                                               => x.get(higherInd)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorExpressionN])   =>
        var index : Double = 0
        for (i <- higherInd.indices) {
          index += higherInd(i) + pow(3,i.toDouble)
        }
        IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(index.toInt)))
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorDatatypeN]) &&
        higherInd != Nil                                                          =>
        var index : Double = 0
        for (i <- higherInd.indices) {
          index += higherInd(i) + pow(3,i.toDouble)
        }
        IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(index.toInt)))
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorDatatypeN]) &&
        higherInd == Nil && row != 0                                              =>
        IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(row)))
      case _   => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
    }
  }

  def getSingleElem(exp : IR_Expression) : IR_Expression = {
    exp match {
      case x : IR_TensorExpression2                                               => x.get(0, 0)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorExpression2])   => x
      case _   => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
    }
  }

  //################################################################################################################
  // Hight order functions

  /** Calculates the determine of a second order tensor
   *
   * @param m : IR_Expression, represent tensor
   * @return : IR_Expression, expression of determinant
   */
  def determinant(m : IR_Expression) : IR_Expression = {
    m match {
      case m : IR_TensorExpression2                                               =>
      Duplicate(m.get(0, 0) * m.get(1, 1) * m.get(2, 2) +
        m.get(0, 1) * m.get(1, 2) * m.get(2, 0) +
        m.get(0, 2) * m.get(1, 0) * m.get(2, 1) -
        m.get(2, 0) * m.get(1, 1) * m.get(0, 2) -
        m.get(2, 1) * m.get(1, 2) * m.get(0, 0) -
        m.get(2, 2) * m.get(1, 0) * m.get(0, 1))
      case m : IR_VariableAccess if (m.datatype.isInstanceOf[IR_TensorExpression2]) =>
        val acc = IR_HighDimAccess
        val ind = IR_ExpressionIndex
        IR_Addition(
          IR_Multiplication(acc(m, ind(0 + 0* 3)), acc(m, ind(1 + 1*3)), acc(m, ind(2 + 2*3))),
          IR_Multiplication(acc(m, ind(0 + 1*3)), acc(m, ind(1 + 2*3)), acc(m, ind(2 + 0*3))),
          IR_Multiplication(acc(m, ind(0 + 2*3)), acc(m, ind(1 + 0*3)), acc(m, ind(2 + 1*3))),
          IR_Subtraction(0, IR_Multiplication(acc(m, ind(2 + 0*3)), acc(m, ind(1 + 1*3)), acc(m, ind(0 + 2*3)))),
          IR_Subtraction(0, IR_Multiplication(acc(m, ind(2 + 1*3)), acc(m, ind(1 + 2*3)), acc(m, ind(0 + 0*3)))),
          IR_Subtraction(0, IR_Multiplication(acc(m, ind(2 + 2*3)), acc(m, ind(1 + 0*3)), acc(m, ind(0 + 1*3))))
        )
      case _   => Logger.error("Determine got the wrong type")
    }
  }

  /** Calculates the trace of a tensor
   *
   * @param m : IR_Expression, represent tensor
   * @return : IR_Expression, expression of trace
   */
  def trace(m : IR_Expression) : IR_Expression = {
    m match {
      case m : IR_TensorExpression2 => IR_Addition(m.get(0, 0), m.get(1, 1), m.get(2, 2))
      case m : IR_VariableAccess if (m.datatype.isInstanceOf[IR_TensorDatatype2])   =>
        IR_Addition(IR_HighDimAccess(m, IR_ExpressionIndex(0)), IR_HighDimAccess(m, IR_ExpressionIndex(1 + 1 * 3)),
          IR_HighDimAccess(m, IR_ExpressionIndex(2 + 2 * 3)))
      case _  => Logger.error("Trace got the wrong type")
    }
  }


  /*
  //################################################################################################################
  // Print tensor

  /** print an overgiven Tensor
   *
   * @param m : IR_Expression, represents the tensor
   * @return
   */
  def printTensor(m : IR_Expression) : ListBuffer[IR_Statement] = {
    m match {
        /*
      case m : IR_VariableAccess if (m.datatype.isInstanceOf[IR_TensorDatatype1])    =>
        var newStmts = ListBuffer[IR_Statement]()
        newStmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf,
          IR_StringConstant("Tensor 1" + m.name + "(\n")))
        for (x <- 0 until 3) {
          newStmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf,
            ListBuffer[IR_Expression](IR_StringConstant("\t[" + x + "]\t => "),
              IR_HighDimAccess(m, IR_ConstIndex(x)), IR_StringConstant(",\n"))))
        }
        newStmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf,
          IR_StringConstant(")\n")))
        newStmts*/
      case m : IR_VariableAccess if (m.datatype.isInstanceOf[IR_TensorDatatype2])    =>
        var newStmts = ListBuffer[IR_Statement]()
        val acc = IR_VariableAccess("std::cout", IR_UnknownDatatype)
        //newStmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf,
        //  IR_StringConstant("Tensor 2" + m.name + "(\n")))
        for (x <- 0 until 3) {
          for (y <- 0 until 3) {
            newStmts += IR_Print(acc, IR_FunctionCall(IR_ExternalFunctionReference.printf,
              ListBuffer[IR_Expression](IR_StringConstant("\t[" + x.toString + ", " + y.toString + "]\t => "),
                IR_HighDimAccess(m, IR_ConstIndex(x, y)), IR_StringConstant(",\n"))))
          }
        }
        //newStmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf,
        //  IR_StringConstant(")\n")))
        newStmts
      case _                        => Logger.error("Tensor print: printing for this datatype is not yet implemented")
    }
  }*/

  //################################################################################################################
  // Compare tensors

  private def innerCompare(i : Int, m : IR_VariableAccess, n : IR_VariableAccess) : IR_Expression = {
    i match {
      case i : Int if (i > 0)  => {
        IR_OrOr(IR_Greater(IR_HighDimAccess(m, IR_ConstIndex(i - 1)), IR_HighDimAccess(n, IR_ConstIndex(i - 1))),
          IR_OrOr(IR_Greater(IR_HighDimAccess(n, IR_ConstIndex(i - 1)), IR_HighDimAccess(m, IR_ConstIndex(i - 1))),
            innerCompare(i - 1, m, n)))
      }
      case i : Int if (i == 0) => IR_BooleanConstant(false)
    }
  }

  private def compareTwoTensor1(m : IR_VariableAccess, n : IR_VariableAccess) : IR_Expression = {
    IR_Negation(innerCompare(3, m, n))
  }

  private def compareTwoTensor2(m : IR_VariableAccess, n : IR_VariableAccess) : IR_Expression = {
    IR_Negation(innerCompare(9, m, n))
  }

  private def compareTwoTensorN(m : IR_VariableAccess, n : IR_VariableAccess) : IR_Expression = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatypeN]
    if (tens1.order != tens2.order) {
      IR_BooleanConstant(false)
    } else {
      IR_Negation(innerCompare(pow(3, tens1.order.toDouble).toInt, m, n))
    }
  }

  private def compareTensor1TensorN(m : IR_VariableAccess, n : IR_VariableAccess) : IR_Expression = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    if (tens1.order != 1) {
      IR_BooleanConstant(false)
    } else {
      IR_Negation(innerCompare(3, m, n))
    }
  }

  private def compareTensor2TensorN(m : IR_VariableAccess, n : IR_VariableAccess) : IR_Expression = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    if (tens1.order != 2) {
      IR_BooleanConstant(false)
    } else {
      IR_Negation(innerCompare(9, m, n))
    }
  }

  /** Compares two tensors
   *
   * @param m : IR_Expression, respresents the first tensor
   * @param n : IR_Expression, respresents the second tensor
   * @return : ListBuffer[IR_Statement], Generated coparason
   */
  def compare(m: IR_Expression, n : IR_Expression) : IR_Expression = {
    (m, n) match {
      case (m: IR_VariableAccess, n : IR_VariableAccess) if (m.datatype.isInstanceOf[IR_TensorDatatype1]) &&
        (n.datatype.isInstanceOf[IR_TensorDatatype1])     => compareTwoTensor1(m, n)
      case (m: IR_VariableAccess, n : IR_VariableAccess) if (m.datatype.isInstanceOf[IR_TensorDatatype2]) &&
        (n.datatype.isInstanceOf[IR_TensorDatatype2])     =>compareTwoTensor2(m, n)
      case (m: IR_VariableAccess, n : IR_VariableAccess) if (m.datatype.isInstanceOf[IR_TensorDatatypeN]) &&
        (n.datatype.isInstanceOf[IR_TensorDatatypeN])     => compareTwoTensorN(m, n)
      case (m: IR_VariableAccess, n : IR_VariableAccess) if (m.datatype.isInstanceOf[IR_TensorDatatypeN]) &&
        (n.datatype.isInstanceOf[IR_TensorDatatype1])     => compareTensor1TensorN(m, n)
      case (m: IR_VariableAccess, n : IR_VariableAccess) if (m.datatype.isInstanceOf[IR_TensorDatatype1]) &&
        (n.datatype.isInstanceOf[IR_TensorDatatypeN])     => compareTensor1TensorN(n, m)
      case (m: IR_VariableAccess, n : IR_VariableAccess) if (m.datatype.isInstanceOf[IR_TensorDatatypeN]) &&
        (n.datatype.isInstanceOf[IR_TensorDatatype2])     => compareTensor2TensorN(m, n)
      case (m: IR_VariableAccess, n : IR_VariableAccess) if (m.datatype.isInstanceOf[IR_TensorDatatype2]) &&
        (n.datatype.isInstanceOf[IR_TensorDatatypeN])     => compareTensor2TensorN(n, m)
      case (_, _)   => Logger.error("Compare tensors: got wrong type")
    }
  }


  //################################################################################################################
  // Dyadic product

  private def dyadicProductTwoArray(m : IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val arr1 = m.datatype.asInstanceOf[Array[IR_Expression]]
    val arr2 = n.datatype.asInstanceOf[Array[IR_Expression]]
    if ((arr1.length != 3) || (arr2.length != 3)) {
      Logger.error("Dyadic productl: both input arrays must have size 3")
    } else {
      val tmp = IR_TensorExpression2(IR_ResultingDatatype(arr1.head.datatype, arr2.head.datatype))
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(x, y, IR_Multiplication(getElem(m, x, 0, Nil), getElem(n, y, 0, Nil)))
        }
      }
      tmp
    }
  }

  private def dyadicProductTwoTensor1(m : IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype1]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype1]
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens1.datatype, tens2.datatype))
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Multiplication(getElem(m, x, 0, Nil), getElem(n, y, 0, Nil)))
      }
    }
    tmp
  }

  private def dyadicProductTensor2Tensor1(m : IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpressionN = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype1]
    val tmp = IR_TensorExpressionN(IR_ResultingDatatype(tens1.datatype, tens2.datatype), 3)
    var index = 0
    for (z <- 0 until 3) {
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.setDirect(index, IR_Multiplication(getElem(m, z, y, Nil), getElem(n, x, 0, Nil)))
          index = index + 1
        }
      }
    }
    tmp
  }

  private def dyadicProductTwoTensor2(m : IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpressionN = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype1]
    val tmp = IR_TensorExpressionN(IR_ResultingDatatype(tens1.datatype, tens2.datatype), 4)
    var index = 0
    for (q <- 0 until 3) {
      for (z <- 0 until 3) {
        for (y <- 0 until 3) {
          for (x <- 0 until 3) {
            tmp.setDirect(index, IR_Multiplication(getElem(m, z, y, Nil), getElem(n, q, x, Nil)))
            index = index + 1
          }
        }
      }
    }

    tmp
  }

  /** Calculates the dyadic product and returns a expression of tensor with a higher order
   *
   * @param m : IR_Expression, represents vector1/tensor1
   * @param n : IR_Expression, represents vector2/tensor2
   * @return : IR_Expression, expression of calculated tensor
   */
  def dyadic(m : IR_Expression, n : IR_Expression) : IR_TensorExpression = {
    (m, n) match {
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype1] &&
        n.datatype.isInstanceOf[IR_TensorDatatype1] => dyadicProductTwoArray(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype1] &&
        n.datatype.isInstanceOf[IR_TensorDatatype1] => dyadicProductTwoTensor1(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype2] &&
        n.datatype.isInstanceOf[IR_TensorDatatype1] => dyadicProductTensor2Tensor1(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype2] &&
        n.datatype.isInstanceOf[IR_TensorDatatype2] => dyadicProductTwoTensor2(m, n)
      case (_, _)                                   => Logger.error("Dyadic product got the a wrong type")
    }
  }

  //################################################################################################################
  // Addition

  private def addTensors1Vector(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val vec = n.datatype.asInstanceOf[IR_VectorDatatype]
    if (vec.size != 3) {
      Logger.error("Add Tensor1 with Vector: vector has wrong size: " + vec.size.toString + " != 3")
    } else {
      val tmp = IR_TensorExpression1(IR_ResultingDatatype(tens.datatype, vec.datatype))
      for (x <- 0 until 3) {
        tmp.set(x, IR_Addition(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
      }
      tmp
    }
  }

  private def addTwoTensors1(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype1]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype1]
    val tmp = IR_TensorExpression1(IR_ResultingDatatype(tens1.datatype, tens2.datatype))
    for (x <- 0 until 3) {
      tmp.set(x, IR_Addition(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
    }
    tmp
  }

  private def addTwoTensors2(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype2]
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens1.datatype, tens2.datatype))
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Addition(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
      }
    }
    tmp
  }

  private def addTwoTensorsN(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpressionN = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatypeN]
    if (tens1.order != tens2.order) {
      Logger.error("Add of to tensor N: has different orders, " + tens1.order.toString + " != " + tens2.order.toString)
    } else {
      val tmp = IR_TensorExpressionN(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.order)
      for (x <- 0 until pow(3,tens1.order.toDouble).toInt) {
        tmp.setDirect(x, IR_Addition(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
      }
      tmp
    }
  }

  private def addTensor2Matrix(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val mat = n.datatype.asInstanceOf[IR_MatrixDatatype]
    if (mat.sizeM != 3 || mat.sizeN != 3) {
      Logger.error("matrix has the wrong dimension")
    } else {
      val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens.datatype, mat.datatype))
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(x, y, IR_Addition(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
        }
      }
      tmp
    }
  }

  /** Calculates the addiction of two tensors or tensor with matrix/vector
   *
   * @param m : IR_Expression, represents vector1/matrix1/tensor1
   * @param n : IR_Expression, represents vector2/matrix1/tensor2
   * @return : IR_Expression, expression of calculated tensor
   */
  def add(m : IR_Expression, n : IR_Expression) : IR_TensorExpression = {
    (m, n) match {
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype1] &&
        n.datatype.isInstanceOf[IR_TensorDatatype1] => addTwoTensors1(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype1] &&
        n.datatype.isInstanceOf[IR_VectorDatatype]  => addTensors1Vector(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_VectorDatatype] &&
        n.datatype.isInstanceOf[IR_TensorDatatype1] => addTensors1Vector(n, m)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype2] &&
        n.datatype.isInstanceOf[IR_TensorDatatype2] => addTwoTensors2(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype2] &&
        n.datatype.isInstanceOf[IR_MatrixDatatype]  => addTensor2Matrix(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_MatrixDatatype] &&
        n.datatype.isInstanceOf[IR_TensorDatatype2] => addTensor2Matrix(n, m)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatypeN] &&
        n.datatype.isInstanceOf[IR_TensorDatatypeN] => addTwoTensorsN(m, n)
      case (_, _)                                   => Logger.error("Add tensor got the a wrong type")
    }
  }

  //################################################################################################################
  // Dotproduct

  private def dotProductTwoTensors1(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatypeN]
    val tmp = IR_TensorExpression1(IR_ResultingDatatype(tens1.datatype, tens2.datatype))
    for (x <- 0 until 3) {
      tmp.set(x, IR_Multiplication(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
    }
    tmp
  }
  private def dotProductTensors1Vector(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype1]
    val vec = n.datatype.asInstanceOf[IR_VectorDatatype]
    if (vec.size != 3) {
      Logger.error("Add Tensor1 with Vector: vector has wrong size: " + vec.size.toString + " != 3")
    } else {
      val tmp = IR_TensorExpression1(IR_ResultingDatatype(tens.datatype, vec.datatype))
      for (x <- 0 until 3) {
        tmp.set(x, IR_Multiplication(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
      }
      tmp
    }
  }

  private def dotProductTwoTensors2(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype2]
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens1.datatype, tens2.datatype))
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Multiplication(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
      }
    }
    tmp
  }

  private def dotProductTwoTensorsN(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpressionN = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatypeN]
    if (tens1.order != tens2.order) {
      Logger.error("Dotproduct of to tensor N: has different orders " + tens1.order.toString +
        " != " + tens2.order.toString)
    } else {
      val tmp = IR_TensorExpressionN(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.order)
      for (x <- 0 until pow(3,tens1.order.toDouble).toInt) {
        tmp.setDirect(x, IR_Multiplication(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
      }
      tmp
    }
  }

  private def dotProductTensor2Matrix(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val mat = n.datatype.asInstanceOf[IR_MatrixDatatype]
    if (mat.sizeM != 3 || mat.sizeN != 3) {
      Logger.error("matrix has the wrong dimension")
    } else {
      val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens.datatype, mat.datatype))
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(x, y, IR_Multiplication(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
        }
      }
      tmp
    }
  }

  /** Calculates the dot product of two tensors or tensor with matrix/vectors
   *
   * @param m : IR_Expression, represents vector1/matrix1/tensor1
   * @param n : IR_Expression, represents vector2/matrix1/tensor2
   * @return : IR_Expression, expression of calculated tensor
   */
  def dot(m : IR_Expression, n : IR_Expression) : IR_TensorExpression = {
    (m, n) match {
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype2]
        && n.datatype.isInstanceOf[IR_TensorDatatype2]   => dotProductTwoTensors2(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype1] &&
        n.datatype.isInstanceOf[IR_VectorDatatype]       => dotProductTensors1Vector(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_VectorDatatype] &&
        n.datatype.isInstanceOf[IR_TensorDatatype1]      => dotProductTensors1Vector(n, m)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype1]
        && n.datatype.isInstanceOf[IR_TensorDatatype1]   => dotProductTwoTensors1(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatypeN]
        && n.datatype.isInstanceOf[IR_TensorDatatypeN]   => dotProductTwoTensorsN(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorExpression2]
        && n.datatype.isInstanceOf[IR_MatrixExpression]  => dotProductTensor2Matrix(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_MatrixExpression]
        && n.datatype.isInstanceOf[IR_TensorExpression2] => dotProductTensor2Matrix(n, m)
      case (_, _)                                        => Logger.error("Dot product tensor got the a wrong type")
    }
  }

  //################################################################################################################
  // Scalaproduct

  private def scalarMulTensor1(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype1]
    val num = n.datatype.asInstanceOf[IR_Number]
    val tmp = IR_TensorExpression1(IR_ResultingDatatype(tens.datatype, num.datatype))
    for (x <- 0 until 3) {
        tmp.set(x, IR_Multiplication(getElem(m, x, 0, Nil), IR_VariableAccess(n.name, n.datatype)))
    }
    tmp
  }

  private def scalarMulTensor2(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val num = n.datatype.asInstanceOf[IR_Number]
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens.datatype, num.datatype))
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Multiplication(getElem(m, x, y, Nil), IR_VariableAccess(n.name, n.datatype)))
      }
    }
    tmp
  }

  private def scalarMulTensorN(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpressionN = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val num = n.datatype.asInstanceOf[IR_Number]
    val tmp = IR_TensorExpressionN(IR_ResultingDatatype(tens.datatype, num.datatype), tens.order)
    for (x <- 0 until pow(3, tens.order.toDouble).toInt) {
      tmp.setDirect(x, IR_Multiplication(getElem(m, x, 0, Nil), IR_VariableAccess(n.name, n.datatype)))
    }
    tmp
  }

  /** Calculates the scalar product of a tensor with a scalar
   *
   * @param m : IR_Expression, represents tensor or scalar
   * @param n : IR_Expression, represents tensor or scalar
   * @return : IR_Expression, expression of calculated tensor
   */
  def scalar(m: IR_Expression, n : IR_Expression) : IR_TensorExpression = {
    (m, n) match {
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype1] &&
        n.datatype.isInstanceOf[IR_Number]                => scalarMulTensor1(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_Number] &&
        n.datatype.isInstanceOf[IR_TensorDatatype1]       => scalarMulTensor1(n, m)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype2] &&
        n.datatype.isInstanceOf[IR_Number]                => scalarMulTensor2(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_Number] &&
        n.datatype.isInstanceOf[IR_TensorDatatype2]       => scalarMulTensor2(n, m)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatypeN] &&
        n.datatype.isInstanceOf[IR_Number]                => scalarMulTensorN(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_Number] &&
        n.datatype.isInstanceOf[IR_TensorDatatypeN]       => scalarMulTensorN(n, m)
      case (_,  _)                                    => Logger.error("Multiplication of tensor with scalar got the a wrong type")
    }
  }

  //################################################################################################################
  // Functioncalls

  this += new Transformation("resolution of built-in functions 2/2", {

    case call : IR_FunctionCall if (call.name == "deter")    => // TODO : instanz prüfen
      if (call.arguments.length != 1) {
        Logger.error("det() must have one argument")
      }
      determinant(call.arguments.head)  // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "trace")    => // TODO : instanz prüfen
      if (call.arguments.length != 1) {
        Logger.error("trace() must have one argument")
      }
      trace(call.arguments.head)  // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "dyadic")    => // TODO : instanz prüfen
      if (call.arguments.length != 2) {
        Logger.error("trace() must have one argument")
      }
      dyadic(call.arguments(0), call.arguments(1))  // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "dotp")                                          =>
      if (call.arguments.length != 2) { // TODO: Zeus, warum kollidiert das mit IR_MatrixAccess? "dot"
        Logger.error("mul() must have two arguments")
      }
      dot(call.arguments(0), call.arguments(1))  // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "add")                                           =>
      if (call.arguments.length != 2) {
        Logger.error("add() must have two arguments")
      }
      add(call.arguments(0), call.arguments(1)) // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "scalar")                                        =>
      if (call.arguments.length != 2) {
        Logger.error("scalar() must have two arguments")
      }
      scalar(call.arguments(0), call.arguments(1)) // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "compare")                                        =>
      if (call.arguments.length != 2) {
        Logger.error("compare() must have two arguments")
      }
      compare(call.arguments(0), call.arguments(1)) // TODO: Zeus, zu testen

      /*
    case call : IR_FunctionCall if (call.name == "str")                                        =>
      if (call.arguments.length != 1) {
        Logger.error("print() must have two arguments")
      }
      printTensor(call.arguments.head) // TODO: Zeus, zu testen*/
  })
}