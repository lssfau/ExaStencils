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
import exastencils.config.Knowledge

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
      "printTensor",
      "compare",
      "asTensor1",
      "asTensor2",
      "eigen"
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
  // determinant

  /** Calculates the determine of a second order tensor to compiletime
   *
   * @param m : IR_Expression, represent tensor
   * @return : IR_Expression, expression of determinant
   */
  def determinant_compiletime(m : IR_Expression) : IR_Expression = {
    m match {
      case m : IR_VariableAccess if (m.datatype.isInstanceOf[IR_TensorDatatype2]) =>
        val tmp = m.datatype.asInstanceOf[IR_TensorDatatype2]
        val acc = IR_HighDimAccess
        val ind = IR_ExpressionIndex
        if (tmp.dims == 3) {
          IR_Addition(
            IR_Multiplication(acc(m, ind(0 + 0 * 3)), acc(m, ind(1 + 1 * 3)), acc(m, ind(2 + 2 * 3))),
            IR_Multiplication(acc(m, ind(0 + 1 * 3)), acc(m, ind(1 + 2 * 3)), acc(m, ind(2 + 0 * 3))),
            IR_Multiplication(acc(m, ind(0 + 2 * 3)), acc(m, ind(1 + 0 * 3)), acc(m, ind(2 + 1 * 3))),
            IR_Subtraction(0, IR_Multiplication(acc(m, ind(2 + 0 * 3)), acc(m, ind(1 + 1 * 3)), acc(m, ind(0 + 2 * 3)))),
            IR_Subtraction(0, IR_Multiplication(acc(m, ind(2 + 1 * 3)), acc(m, ind(1 + 2 * 3)), acc(m, ind(0 + 0 * 3)))),
            IR_Subtraction(0, IR_Multiplication(acc(m, ind(2 + 2 * 3)), acc(m, ind(1 + 0 * 3)), acc(m, ind(0 + 1 * 3))))
          )
        } else {
          IR_Addition(
            IR_Multiplication(acc(m, ind(0)), acc(m, ind(3))),
            IR_Subtraction(0, IR_Multiplication(acc(m, ind(1)), acc(m, ind(2))))
          )
        }
      case _   => Logger.error("Determine got the wrong type")
    }
  }

  /** Calculates the determine of a second order tensor to compiletime
   *
   * @param m : IR_Expression, represent tensor
   * @return : IR_Expression, expression of determinant
   */
  def determinant_runtime(m: IR_Expression) : IR_Scope = {
    m match {
      case m : IR_VariableAccess if (m.datatype.isInstanceOf[IR_TensorDatatype2])  =>
        val tmp = m.datatype.asInstanceOf[IR_TensorDatatype2]
        val det = IR_Scope(Nil)

        det // TODO: Hier muss noch der Laplas sche Entwicklungssatz rein
      case _  => Logger.error("Determine got the wrong type")
    }

  }

  //################################################################################################################
  // Trace

  def inner_trace2(m : IR_VariableAccess, i : Int, dims : Int) : IR_Expression = {
      i match {
        case i if (i > 0) => IR_Addition(IR_HighDimAccess(m, IR_ExpressionIndex(i-1 + (i-1)*dims)), inner_trace2(m, i-1, dims))
        case _ => IR_RealConstant(0.0)
      }
  }

  def inner_traceN(m : IR_VariableAccess, i : Int, dims : Int, order : Int) : IR_Expression = {
    i match {
      case i if (i > 0) =>
        var index = 0
        for (k <- 0 until order){
          index = index + (i -1) * pow(dims.toDouble, k.toDouble).toInt
        }
        IR_Addition(IR_HighDimAccess(m, IR_ExpressionIndex(index)), inner_traceN(m, i - 1, dims, order))
      case _ => IR_RealConstant(0.0)
    }
  }

  /** Calculates the trace of a tensor to compiletime
   *
   * @param m : IR_Expression, represent tensor
   * @return : IR_Expression, expression of trace
   */
  def trace(m : IR_Expression) : IR_Expression = {
    m match {
      case m : IR_TensorExpression2 => IR_Addition(m.get(0, 0), m.get(1, 1), m.get(2, 2))
      case m : IR_VariableAccess if (m.datatype.isInstanceOf[IR_TensorDatatype2]) &&
        (m.datatype.asInstanceOf[IR_TensorDatatype2].dims == 2)       =>
        IR_Addition(IR_HighDimAccess(m, IR_ExpressionIndex(0)), IR_HighDimAccess(m, IR_ExpressionIndex(2)))
      case m : IR_VariableAccess if (m.datatype.isInstanceOf[IR_TensorDatatype2]) &&
        (m.datatype.asInstanceOf[IR_TensorDatatype2].dims == 3)       =>
        IR_Addition(IR_HighDimAccess(m, IR_ExpressionIndex(0)), IR_HighDimAccess(m, IR_ExpressionIndex(5)),
          IR_HighDimAccess(m, IR_ExpressionIndex(8)))
      case m : IR_VariableAccess if (m.datatype.isInstanceOf[IR_TensorDatatype2]) &&
        (m.datatype.asInstanceOf[IR_TensorDatatype2].dims > 3)       =>
        val dims = m.datatype.asInstanceOf[IR_TensorDatatype2].dims
        val i = dims
        inner_trace2(m, i, dims)
      case m : IR_VariableAccess if (m.datatype.isInstanceOf[IR_TensorDatatypeN]) &&
        (m.datatype.asInstanceOf[IR_TensorDatatypeN].dims > 3)       =>
        val dims = m.datatype.asInstanceOf[IR_TensorDatatypeN].dims
        val order = m.datatype.asInstanceOf[IR_TensorDatatypeN].order
        val i = dims
        inner_traceN(m, i, dims, order)
      case _  => Logger.error("Trace got the wrong type")
    }
  }

  //################################################################################################################
  // eigenvalues of tensor order 2

  // calculates the eigenvalues with the QR-algorithm
  // for orthogonalisation householder-mirroring is used

  def householder_step(I : IR_VariableAccess, Q : IR_VariableAccess, R : IR_VariableAccess, sign : IR_VariableAccess, alpha : IR_VariableAccess, v : IR_VariableAccess, i : IR_VariableAccess, q : IR_TensorDatatype2, dims : Int) : IR_Scope = {
    // TODO: Hier beginnt mein Householder step mit alpha (immer sign(1,1) )

    /* Index direction
          i ->
        , --------------
      j |
        |   (i, j)
        |

     */
    // short names for better lookalike
    val decl = IR_VariableDeclaration
    val acc = IR_VariableAccess
    val ass = IR_Assignment
    val hdacc = IR_HighDimAccess
    val eind = IR_ExpressionIndex
    val realc = IR_RealConstant
    val intc = IR_IntegerConstant

    val step = IR_Scope(Nil)
    step.body += ass(sign, IR_TernaryCondition(IR_Lower(hdacc(Q, eind(0, 0)), realc(0.0)), realc(-1.0), realc(1.0)))

    // some iteration variables
    val j = acc("j", IR_IntegerDatatype)
    val k = acc("k", IR_IntegerDatatype)
    val l = acc("l", IR_IntegerDatatype)

    val Qn = acc("Qn", IR_TensorDatatype2(q.resolveDeclType, dims))
    step.body += decl(Qn)

    // alpha = sign(A_n1) * || A_n ||_2
    val A_norm = acc("A_2", IR_RealDatatype)
    step.body += decl(A_norm)
    step.body += ass(A_norm, realc(0.0))
    step.body += IR_ForLoop(decl(j, intc(0)), IR_Lower(j, intc(dims)), IR_PostIncrement(j),
      ass(A_norm, IR_Addition(A_norm, IR_Power(hdacc(R, eind(i ,j)), realc(2.0))))
    )
    step.body += ass(A_norm, IR_Power(A_norm, realc(1/2)))
    step.body += ass(alpha, IR_Multiplication(sign, A_norm))
    // end alpha

    // v_n = A_n + alpha_n * e_n
    step.body += IR_ForLoop(decl(j, i), IR_Lower(j, intc(dims)), IR_PostIncrement(j),
      ass(hdacc(v, eind(j)), hdacc(R, eind(i, j)))
    )
    step.body += ass(hdacc(v, eind(i)), IR_Addition(hdacc(v, eind(i)), alpha))
    // end v_n

    // over := 2* v * v^T
    val vv = acc("vv", IR_TensorDatatype2(q.resolveDeclType, dims))
    step.body += decl(vv, dyadic(v, v))
    step.body += IR_ForLoop(decl(k, intc(0)), IR_Lower(k, intc(dims)), IR_PostIncrement(k),
      IR_ForLoop(decl(l, intc(0)), IR_Lower(l, intc(dims)), IR_PostIncrement(l),
        IR_IfCondition(IR_OrOr(IR_Lower(k, i), IR_Lower(l, i)), ass(hdacc(vv, eind(k, l)), realc(0.0))) // sets the unused values of vv to 0
      )
    )
    val two = acc("two", IR_RealDatatype)
    step.body += decl(two, realc(2.0))
    val over = acc("over", IR_TensorDatatype2(q.resolveDeclType, dims))
    step.body += decl(over, scalar(two, vv))
    // end over

    // under := // v^T * v
    val under = acc("under", IR_RealDatatype)
    step.body += decl(under, realc(0.0))
    step.body += IR_ForLoop(decl(k, i), IR_Lower(k, intc(dims)), IR_PostIncrement(k),
      IR_Addition(IR_Multiplication(hdacc(v, eind(k)), hdacc(v, eind(k))))
    ) // end under

    val subt = acc("subt", IR_TensorDatatype2(q.resolveDeclType, dims))
    step.body += decl(subt, IR_Division(over, under)) // subt = (2* v * v^T)/(v^T * v)
    step.body += IR_Assignment(Qn, sub(I, subt)) // Qn=I-(2* v * v^T)/(v^T * v)
    step.body += IR_Assignment(R, mul(Qn, R)) // R_n = Qn*A
    step.body += IR_Assignment(Q, mul(transpose(Qn), Q)) // R_n = Qn*A

    step
  }

  // Algorithm
  // res : A = QR
  // 1. calc Q_n with householderStep
  // 2. Q_n+1 = Q_n * A_n

  def qrDecompHouseholder(A : IR_VariableAccess, res : IR_VariableAccess, Q : IR_VariableAccess, R : IR_VariableAccess) : IR_Scope = {
    // 1. alpha = sign(a_n1) * || a_n ||_2
    // 2. v_n = a_n + alpha * e
    // 3. Q = I - (2 v * v^T )/(v^T * v)

    // short names for better lookalike
    val decl = IR_VariableDeclaration
    val acc = IR_VariableAccess
    val ass = IR_Assignment
    val hdacc = IR_HighDimAccess
    val realc = IR_RealConstant


    val house = IR_Scope(Nil)
    val q = A.datatype.asInstanceOf[IR_TensorDatatype2]
    val dims = q.dims
    val R = acc("R", IR_TensorDatatype2(q.resolveDeclType, dims))
    val Q = acc("Q", IR_TensorDatatype2(q.resolveDeclType, dims))
    val I = acc("I", IR_TensorDatatype2(q.resolveDeclType, dims))
    val v = acc("v", IR_TensorDatatype1(q.resolveDeclType, dims))
    val alpha = acc("alpha", IR_RealDatatype)
    val sign = acc("sign", IR_RealDatatype)
    house.body += decl(v)
    house.body += decl(alpha)
    house.body += decl(sign)
    for (x <- 0 until 3) {
      for (y <- 0 until 3) {
        if (x equals y) {
          house.body += ass(hdacc(Q, IR_ConstIndex(x, y)), realc(1.0))
        } else {
          house.body += ass(hdacc(Q, IR_ConstIndex(x, y)), realc(0.0))
        }
      }
    }
    house.body += decl(I)
    for (x <- 0 until 3) {
      for (y <- 0 until 3) {
        if (x equals y) {
          house.body += ass(hdacc(I, IR_ConstIndex(x, y)), realc(1.0))
        } else {
          house.body += ass(hdacc(I, IR_ConstIndex(x, y)), realc(0.0))
        }
      }
    }
    val i = acc("i", IR_IntegerDatatype)
    house.body += decl(i)
    house.body += IR_ForLoop(ass(i, IR_IntegerConstant(1)), IR_Lower(i, IR_IntegerConstant(dims)), IR_PostIncrement(i),
      householder_step(I, Q, R, sign, alpha, v, i, q, dims)
    )

    house
  }

  /** Calculates the eigenvalue of a tensor
   *
   * @param A : IR_Expression, represent tensor order 2
   * @return : IR_Expression, expression of trace
   */
  def eigenvalue(A: IR_Expression, res : IR_Expression) : IR_Scope = {
    (A, res) match {
      case (a : IR_VariableAccess, res : IR_VariableAccess) if (A.datatype.isInstanceOf[IR_TensorDatatype2]) &&
        (res.datatype.isInstanceOf[IR_MatrixDatatype]) =>
        val _m = a.datatype.asInstanceOf[IR_TensorDatatype2]
        val _n = res.datatype.asInstanceOf[IR_MatrixDatatype]
        if (_m.datatype.isInstanceOf[IR_HigherDimensionalDatatype]) {
          Logger.error("Eigenvalue Tensor: Inner Datatype can not be an High dimension datatype")
        }
        if (_n.sizeM != _m.dims || _n.sizeN != 1) {
          Logger.error("Eigenvalue Tensor: Return array n need to has shape=(dimension,1)")
        }
        // short names for better lookalike
        val decl = IR_VariableDeclaration
        val acc = IR_VariableAccess
        val ass = IR_Assignment
        val hdacc = IR_HighDimAccess
        val realc = IR_RealConstant

        val eigen = IR_Scope(Nil)
        val q = a.datatype.asInstanceOf[IR_TensorDatatype2]
        val dims = q.dims
        val R = acc("R", IR_TensorDatatype2(q.resolveDeclType, dims))
        val Q = acc("Q", IR_TensorDatatype2(q.resolveDeclType, dims))
        eigen.body += decl(Q)
        eigen.body += decl(R, A)
        // TODO Hessebergmatrix?

        val n = acc("n", IR_IntegerDatatype)
        eigen.body += decl(n)
        eigen.body += IR_ForLoop(ass(n, IR_IntegerConstant(1)), IR_Lower(n, IR_IntegerConstant(dims)), IR_PostIncrement(n),
          qrDecompHouseholder(a, res, Q, R)
        )


        eigen
      case _                                           => Logger.error("Eigenvalue Tensor: got wrong datatype")
    }
  }


/*
  //################################################################################################################
  // Print tensor

  private def innerPrint2(i : Int, m : IR_VariableAccess, buf : ListBuffer[IR_Expression]) : ListBuffer[IR_Expression] = {
    i match {
      case i : Int if (i < 9)  => {
        buf += IR_StringConstant("\t[" + (i / 3 % 1).toString + ", " + (i % 3).toString + "] => ")
        buf += IR_HighDimAccess(m, IR_ConstIndex(i))
        buf += IR_StringConstant("\n")
        innerPrint2(i+1, m, buf)
    }
      case i : Int if (i == 9) => ListBuffer[IR_Expression](IR_StringConstant(""))
    }
  }

  private def printTensor2(m : IR_VariableAccess) : IR_ExpressionStatement = {
    var buf = ListBuffer[IR_Expression]()
    buf += IR_StringConstant("Tensor 2 " + m.name + "( \n")
    innerPrint2(0, m, buf)
    buf += IR_StringConstant(")\n")
    IR_ExpressionStatement(IR_FunctionCall("printTensor", buf))
  }

  /** print an overgiven Tensor
   *
   * @param m : IR_Expression, represents the tensor
   * @return
   */
  def printTensor(m : IR_Expression) : IR_ExpressionStatement = {
    m match {
      case m : IR_VariableAccess if (m.datatype.isInstanceOf[IR_TensorDatatype2])    => printTensor2(m)
      case _                        => Logger.error("Tensor print: printing for this datatype is not yet implemented")
    }
  }*/

  //################################################################################################################
  // Compare tensors

  private def innerCompare(i : Int, m : IR_VariableAccess, n : IR_VariableAccess) : IR_Expression = {
    i match {
      case i : Int if (i > 0)  =>
        IR_OrOr(IR_Greater(IR_HighDimAccess(m, IR_ConstIndex(i - 1)), IR_HighDimAccess(n, IR_ConstIndex(i - 1))),
          IR_OrOr(IR_Greater(IR_HighDimAccess(n, IR_ConstIndex(i - 1)), IR_HighDimAccess(m, IR_ConstIndex(i - 1))),
            innerCompare(i - 1, m, n)))
      case i : Int if (i == 0) => IR_BooleanConstant(false)
    }
  }

  private def compareTwoTensor1(m : IR_VariableAccess, n : IR_VariableAccess) : IR_Expression = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype1]
    val tens2 = m.datatype.asInstanceOf[IR_TensorDatatype1]
    if (tens1.dims != tens2.dims) {
      IR_BooleanConstant(false)
    }
    IR_Negation(innerCompare(tens1.dims, m, n))
  }

  private def compareTwoTensor2(m : IR_VariableAccess, n : IR_VariableAccess) : IR_Expression = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val tens2 = m.datatype.asInstanceOf[IR_TensorDatatype2]
    if (tens1.dims != tens2.dims) {
      IR_BooleanConstant(false)
    }
    IR_Negation(innerCompare(tens1.dims*tens1.dims, m, n))
  }

  private def compareTwoTensorN(m : IR_VariableAccess, n : IR_VariableAccess) : IR_Expression = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatypeN]
    if ((tens1.order != tens2.order) || (tens1.dims != tens2.dims)) {
      IR_BooleanConstant(false)
    } else {
      IR_Negation(innerCompare(pow(tens1.dims, tens1.order.toDouble).toInt, m, n))
    }
  }

  private def compareTensor1TensorN(m : IR_VariableAccess, n : IR_VariableAccess) : IR_Expression = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val tens2 = m.datatype.asInstanceOf[IR_TensorDatatype1]
    if ((tens1.order != 1) || (tens1.dims != tens2.dims)) {
      IR_BooleanConstant(false)
    } else {
      IR_Negation(innerCompare(tens1.dims, m, n))
    }
  }

  private def compareTensor2TensorN(m : IR_VariableAccess, n : IR_VariableAccess) : IR_Expression = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype2]
    if ((tens1.order != 2) || (tens1.dims != tens2.dims)) {
      IR_BooleanConstant(false)
    } else {
      IR_Negation(innerCompare(tens1.dims *  tens1.dims, m, n))
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
  // Convert N to 2 or 1

  /** Converts a TensorN to a Tensor1
   *
   * @param m : IR_Expression, represents the input tensor
   * @return IR_TensorExpression1
   */
  def covertToTensor1(m: IR_Expression) : IR_TensorExpression1 = {
    m match {
      case m : IR_VariableAccess if m.datatype.isInstanceOf[IR_TensorDatatypeN]             =>
        val tensN = m.datatype.asInstanceOf[IR_TensorDatatypeN]
        if (tensN.order != 1) {
          Logger.error("Convert to Tensor1: input tensor has the wrong order")
        }
        val tens1 = IR_TensorExpression1(tensN.resolveDeclType, tensN.dims)
        for (x <- 0 until tensN.dims) {
          tens1.set(x, getElem(m, 0, 0, List(x)))
        }
        tens1
      case m : IR_VariableAccess if m.datatype.isInstanceOf[IR_MatrixDatatype]              =>
        val matrix = m.datatype.asInstanceOf[IR_MatrixDatatype]
        if (matrix.sizeM != 1 && matrix.sizeN == 1) {
          val tens1 = IR_TensorExpression1(matrix.resolveDeclType, matrix.sizeM)
          for (x <- 0 until matrix.sizeM) {
            tens1.set(x, IR_HighDimAccess(m, IR_ExpressionIndex(x, 0)))
          }
          tens1
        } else if (matrix.sizeM != 1 && matrix.sizeN == 1) {
          val tens1 = IR_TensorExpression1(matrix.resolveDeclType, matrix.sizeM)
          for (y <- 0 until matrix.sizeM) {
            tens1.set(y, IR_HighDimAccess(m, IR_ExpressionIndex(0, y)))
          }
          tens1
        } else {
          Logger.error("Convert to Tensor1: input matrix is not one dimensional")
        }
      case _      => Logger.error("Convert to Tensor2: got wrong input type")
    }
  }

  /** Converts a TensorN or Matrix to a Tensor2
   *
   * @param m : IR_Expression, represents the input tensor
   * @return IR_TensorExpression2
   */
  def covertToTensor2(m: IR_Expression) : IR_TensorExpression2 = {
    m match {
      case m : IR_VariableAccess if m.datatype.isInstanceOf[IR_TensorDatatypeN]             =>
        val tensN = m.datatype.asInstanceOf[IR_TensorDatatypeN]
        if (tensN.order != 2) {
          Logger.error("Convert to Tensor2: input tensor has the wrong order")
        }
        val tens2 = IR_TensorExpression2(tensN.resolveDeclType, tensN.dims)
        for (y <- 0 until tensN.dims) {
          for (x <- 0 until tensN.dims) {
            tens2.set(x, y, getElem(m, 0, 0, List(x,y)))
          }
        }
        tens2
      case m : IR_VariableAccess if m.datatype.isInstanceOf[IR_MatrixDatatype]              =>
        val matrix = m.datatype.asInstanceOf[IR_MatrixDatatype]
        if (matrix.sizeM != matrix.sizeN) {
          Logger.error("Convert to Tensor2: input matrix is not squared")
        }
        val tens2 = IR_TensorExpression2(matrix.resolveDeclType, matrix.sizeN)
        for (y <- 0 until matrix.sizeN) {
          for (x <- 0 until matrix.sizeN) {
            tens2.set(x, y, IR_HighDimAccess(m, IR_ExpressionIndex(x, y)))
          }
        }
        tens2
      case _      => Logger.error("Convert to Tensor2: got wrong input type")
    }
  }

  //################################################################################################################
  // Dyadic product

  private def dyadicProductTwoArray(m : IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val arr1 = m.datatype.asInstanceOf[IR_MatrixDatatype]
    val arr2 = n.datatype.asInstanceOf[IR_MatrixDatatype]
    if (arr1.sizeM != arr2.sizeM) {
      Logger.error("Dyadic productl: both input arrays must have size 3")
    } else if ((arr1.sizeN != 1) && (arr2.sizeN != 1)) {
      Logger.error("Dyadic product: Both input arrys must be flat array")
    } else {
      val tmp = IR_TensorExpression2(IR_ResultingDatatype(arr1.resolveDeclType, arr2.resolveDeclType), arr1.sizeM)
      for (y <- 0 until arr1.sizeM) {
        for (x <- 0 until arr1.sizeM) {
          tmp.set(x, y, IR_Multiplication(getElem(m, x, 0, Nil), getElem(n, y, 0, Nil)))
        }
      }
      tmp
    }
  }

  private def dyadicProductTwoTensor1(m : IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype1]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype1]
    if (tens1.dims != tens2.dims) {
      Logger.error("Dyadic product: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims)
    for (y <- 0 until tens1.dims) {
      for (x <- 0 until tens1.dims) {
        tmp.set(x, y, IR_Multiplication(getElem(m, x, 0, Nil), getElem(n, y, 0, Nil)))
      }
    }
    tmp
  }

  private def dyadicProductTensor2Tensor1(m : IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpressionN = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype1]
    if (tens1.dims != tens2.dims) {
      Logger.error("Dyadic product: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpressionN(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims, 3)
    var index = 0
    for (z <- 0 until tens1.dims) {
      for (y <- 0 until tens1.dims) {
        for (x <- 0 until tens1.dims) {
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
    if (tens1.dims != tens2.dims) {
      Logger.error("Dyadic product: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpressionN(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims, 4)
    var index = 0
    for (q <- 0 until tens1.dims) {
      for (z <- 0 until tens1.dims) {
        for (y <- 0 until tens1.dims) {
          for (x <- 0 until tens1.dims) {
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
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_MatrixDatatype] &&
        n.datatype.isInstanceOf[IR_MatrixDatatype] => dyadicProductTwoArray(m, n)
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
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype1]
    val vec = n.datatype.asInstanceOf[IR_VectorDatatype]
    if (vec.size != tens.dims) {
      Logger.error("Add Tensor1 with Vector: vector and tensor has different dimensionality")
    }
    val tmp = IR_TensorExpression1(IR_ResultingDatatype(tens.datatype, vec.datatype), tens.dims)
    for (x <- 0 until tens.dims) {
      tmp.set(x, IR_Addition(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
    }
    tmp
  }

  private def addTwoTensors1(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype1]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype1]
    if (tens1.dims != tens2.dims) {
      Logger.error("Add two Tensor1: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpression1(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims)
    for (x <- 0 until tens1.dims) {
      tmp.set(x, IR_Addition(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
    }
    tmp
  }

  private def addTwoTensors2(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype2]
    if (tens1.dims != tens2.dims) {
      Logger.error("Add two Tensor2: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims)
    for (y <- 0 until tens1.dims) {
      for (x <- 0 until tens1.dims) {
        tmp.set(x, y, IR_Addition(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
      }
    }
    tmp
  }

  private def addTwoTensorsN(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpressionN = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatypeN]
    if (tens1.order != tens2.order) {
      Logger.error("Add two TensorN: has different orders, " + tens1.order.toString + " != " + tens2.order.toString)
    }
    if (tens1.dims != tens2.dims) {
      Logger.error("Add two TensorN: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpressionN(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims, tens1.order)
    for (x <- 0 until pow(tens1.dims,tens1.order.toDouble).toInt) {
      tmp.setDirect(x, IR_Addition(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
    }
    tmp

  }

  private def addTensor2Matrix(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val mat = n.datatype.asInstanceOf[IR_MatrixDatatype]
    if ((mat.sizeM != tens.dims) || (mat.sizeN != tens.dims)) {
      Logger.error("matrix has the wrong dimension")
    }
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens.datatype, mat.datatype), tens.dims)
    for (y <- 0 until tens.dims) {
      for (x <- 0 until tens.dims) {
        tmp.set(x, y, IR_Addition(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
      }
    }
    tmp
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
  // Subtraction

  private def subTensors1Vector(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val vec = n.datatype.asInstanceOf[IR_VectorDatatype]
    if (vec.size != tens.dims) {
      Logger.error("Sub Tensor1 with Vector: vector and tensor has different dimensionality")
    }
    val tmp = IR_TensorExpression1(IR_ResultingDatatype(tens.datatype, vec.datatype), tens.dims)
    for (x <- 0 until tens.dims) {
      tmp.set(x, IR_Subtraction(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
    }
    tmp
  }

  private def subTwoTensors1(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype1]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype1]
    if (tens1.dims != tens2.dims) {
      Logger.error("Sub two Tensor1: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpression1(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims)
    for (x <- 0 until tens1.dims) {
      tmp.set(x, IR_Subtraction(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
    }
    tmp
  }

  private def subTwoTensors2(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype2]
    if (tens1.dims != tens2.dims) {
      Logger.error("Sub two Tensor2: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims)
    for (y <- 0 until tens1.dims) {
      for (x <- 0 until tens1.dims) {// TODO: N-Dimensional
        tmp.set(x, y, IR_Subtraction(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
      }
    }
    tmp
  }

  private def subTwoTensorsN(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpressionN = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatypeN]
    if (tens1.order != tens2.order) {
      Logger.error("Sub two TensorN: has different orders, " + tens1.order.toString + " != " + tens2.order.toString)
    }
    if (tens1.dims != tens2.dims) {
      Logger.error("Sub two TensorN: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpressionN(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims, tens1.order)
    for (x <- 0 until pow(tens1.dims,tens1.order.toDouble).toInt) {
      tmp.setDirect(x, IR_Subtraction(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
    }
    tmp
  }

  private def subTensor2Matrix(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val mat = n.datatype.asInstanceOf[IR_MatrixDatatype]
    if ((mat.sizeM != tens.dims) || (mat.sizeN != tens.dims)) {
      Logger.error("matrix has the wrong dimension")
    }
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens.datatype, mat.datatype), tens.dims)
    for (y <- 0 until tens.dims) {
      for (x <- 0 until tens.dims) {
        tmp.set(x, y, IR_Subtraction(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
      }
    }
    tmp
  }

  /** Calculates the addiction of two tensors or tensor with matrix/vector
   *
   * @param m : IR_Expression, represents vector1/matrix1/tensor1
   * @param n : IR_Expression, represents vector2/matrix1/tensor2
   * @return : IR_Expression, expression of calculated tensor
   */
  def sub(m : IR_Expression, n : IR_Expression) : IR_TensorExpression = {
    (m, n) match {
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype1] &&
        n.datatype.isInstanceOf[IR_TensorDatatype1] => subTwoTensors1(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype1] &&
        n.datatype.isInstanceOf[IR_VectorDatatype]  => subTensors1Vector(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_VectorDatatype] &&
        n.datatype.isInstanceOf[IR_TensorDatatype1] => subTensors1Vector(n, m)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype2] &&
        n.datatype.isInstanceOf[IR_TensorDatatype2] => subTwoTensors2(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype2] &&
        n.datatype.isInstanceOf[IR_MatrixDatatype]  => subTensor2Matrix(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_MatrixDatatype] &&
        n.datatype.isInstanceOf[IR_TensorDatatype2] => subTensor2Matrix(n, m)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatypeN] &&
        n.datatype.isInstanceOf[IR_TensorDatatypeN] => subTwoTensorsN(m, n)
      //case (_, _)                                   => Logger.error("Add tensor got the a wrong type")
      case (_,_) => Logger.error(m.toString + n.toString)
    }
  }

  //################################################################################################################
  // Dotproduct

  private def dotProductTwoTensors1(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype1]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype1]
    if (tens1.dims != tens2.dims) {
      Logger.error("Dotproduct two Tensor1: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpression1(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims)
    for (x <- 0 until tens1.dims) {
      tmp.set(x, IR_Multiplication(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
    }
    tmp
  }

  private def dotProductTensors1Vector(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype1]
    val vec = n.datatype.asInstanceOf[IR_VectorDatatype]
    if (vec.size != tens.dims) {
      Logger.error("Dotproduct Tensor1 with Vector: vector and tensor has different dimensionality")
    } else {
      val tmp = IR_TensorExpression1(IR_ResultingDatatype(tens.datatype, vec.datatype), tens.dims)
      for (x <- 0 until tens.dims) {
        tmp.set(x, IR_Multiplication(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
      }
      tmp
    }
  }

  private def dotProductTwoTensors2(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype2]
    if (tens1.dims != tens2.dims) {
      Logger.error("Dotproduct two Tensor2: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims)
    for (y <- 0 until tens1.dims) {
      for (x <- 0 until tens1.dims) {
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
    }
    if (tens1.dims != tens2.dims) {
      Logger.error("Dotproduct two TensorN: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpressionN(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims, tens1.order)
    for (x <- 0 until pow(tens1.dims ,tens1.order.toDouble).toInt) {
      tmp.setDirect(x, IR_Multiplication(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
    }
    tmp
  }

  private def dotProductTensor2Matrix(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val mat = n.datatype.asInstanceOf[IR_MatrixDatatype]
    if ((mat.sizeM != tens.dims) || (mat.sizeN != tens.dims)) {
      Logger.error("matrix has the wrong dimension")
    }
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens.datatype, mat.datatype), tens.dims)
    for (y <- 0 until tens.dims) {
      for (x <- 0 until tens.dims) {
        tmp.set(x, y, IR_Multiplication(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
      }
    }
    tmp
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
  // Transpose of the tensor

  def transposeTens2(m: IR_VariableAccess) : IR_TensorExpression2 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val tmp = IR_TensorExpression2(tens.resolveDeclType, tens.dims)
    for (x <- 1 until tens.dims) {
      for (y <- 0 until x) {
        tmp.set(y, x, getElem(m, x, y, Nil))
        tmp.set(x, y, getElem(m, y, x, Nil))
      }
    }
    tmp
  }

  def transpose(m : IR_Expression) : IR_TensorExpression2 = {
    m match {
      case m : IR_VariableAccess if m.datatype.isInstanceOf[IR_TensorDatatype2] => transposeTens2(m)
      case _  => Logger.error("Tensor Transpose: Need tensor 2")
    }
  }

  //################################################################################################################
  // Multiplication as classical matrix multiplikation

  def matmul(m: IR_VariableAccess, n: IR_VariableAccess) : IR_TensorExpression2 = {
    val tens1 = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val tens2 = n.datatype.asInstanceOf[IR_TensorDatatype2]
    if (tens1.dims != tens2.dims) {
      Logger.error("Multiplication two Tensor2: has different dimensionality, " + tens1.dims.toString + " != " +
        tens2.dims.toString)
    }
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens1.datatype, tens2.datatype), tens1.dims)
    for (x <- 0 until tens1.dims) {
      for (y <- 0 until tens1.dims) {
        val mul : ListBuffer[IR_Expression] = ListBuffer()
        for (k <- 0 until tens1.dims) {
          mul += IR_Multiplication(getElem(m, k, y, Nil), getElem(n, x, k, Nil))
        }
        tmp.set(x, y, IR_Addition(mul))
      }
    }
    tmp
  }

  def mul(m : IR_Expression, n : IR_Expression) : IR_TensorExpression2 = {
    (m, n) match {
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype2] &&
        n.datatype.isInstanceOf[IR_TensorDatatype2] => matmul(m, n)
      case (_, _)  => Logger.error("Tensor Multiplication: Need two tensor 2")
    }
  }

  //################################################################################################################
  // Scalaproduct

  private def scalarMulTensor1(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype1]
    val num = n.datatype.asInstanceOf[IR_ScalarDatatype]
    val tmp = IR_TensorExpression1(IR_ResultingDatatype(tens.datatype, num.resolveDeclType), tens.dims)
    for (x <- 0 until tens.dims) {
        tmp.set(x, IR_Multiplication(getElem(m, x, 0, Nil), IR_VariableAccess(n.name, n.datatype)))
    }
    tmp
  }

  private def scalarMulTensor2(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatype2]
    val num = n.datatype.asInstanceOf[IR_ScalarDatatype]
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens.datatype, num.resolveDeclType), tens.dims)
    for (y <- 0 until tens.dims) {
      for (x <- 0 until tens.dims) {
        tmp.set(x, y, IR_Multiplication(getElem(m, x, y, Nil), IR_VariableAccess(n.name, n.datatype)))
      }
    }
    tmp
  }

  private def scalarMulTensorN(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpressionN = {
    val tens = m.datatype.asInstanceOf[IR_TensorDatatypeN]
    val num = n.datatype.asInstanceOf[IR_ScalarDatatype]
    val tmp = IR_TensorExpressionN(IR_ResultingDatatype(tens.datatype, num.resolveDeclType), tens.dims, tens.order)
    for (x <- 0 until pow(tens.dims, tens.order.toDouble).toInt) {
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
        n.datatype.isInstanceOf[IR_ScalarDatatype]                => scalarMulTensor1(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_ScalarDatatype] &&
        n.datatype.isInstanceOf[IR_TensorDatatype1]       => scalarMulTensor1(n, m)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype2] &&
        n.datatype.isInstanceOf[IR_ScalarDatatype]                => scalarMulTensor2(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_ScalarDatatype] &&
        n.datatype.isInstanceOf[IR_TensorDatatype2]       => scalarMulTensor2(n, m)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatypeN] &&
        n.datatype.isInstanceOf[IR_ScalarDatatype]                => scalarMulTensorN(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_ScalarDatatype] &&
        n.datatype.isInstanceOf[IR_TensorDatatypeN]       => scalarMulTensorN(n, m)
      //case (_,  _)                      => Logger.error("Multiplication of tensor with scalar got the a wrong type")
      case (_, _) => Logger.error(m.toString +  n.toString)
    }
  }

  //################################################################################################################
  // Functioncalls

  this += new Transformation("resolution of built-in functions 2/2", {

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

    case call : IR_FunctionCall if (call.name == "asTensor1")                                     =>
      if (call.arguments.length != 1) {
        Logger.error("asTensor1() must have two arguments")
      }
      covertToTensor1(call.arguments.head) // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "asTensor2")                                     =>
      if (call.arguments.length != 1) {
        Logger.error("asTensor2() must have two arguments")
      }
      covertToTensor2(call.arguments.head) // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "compare")                                       =>
      if (call.arguments.length != 2) {
        Logger.error("compare() must have two arguments")
      }
      compare(call.arguments(0), call.arguments(1)) // TODO: Zeus, zu testen

    case IR_ExpressionStatement(call) if (call.isInstanceOf[IR_FunctionCall]) &&
      (call.asInstanceOf[IR_FunctionCall].name == "deter") &&
      (call.asInstanceOf[IR_FunctionCall].arguments.head.isInstanceOf[IR_TensorDatatype2]) &&
      (List(2,3).contains(call.asInstanceOf[IR_FunctionCall].arguments.head.asInstanceOf[IR_TensorDatatype2].dims)) => // TODO : instanz prüfen
      val call_res = call.asInstanceOf[IR_FunctionCall]
      if (call_res.arguments.length != 1) {
        Logger.error("det() must have one argument")
      }
      determinant_runtime(call_res.arguments.head)  // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "deter")    => // TODO : instanz prüfen
      if (call.arguments.length != 1) {
        Logger.error("trace() must have one argument")
      }
      determinant_compiletime(call.arguments.head)  // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "trace")    => // TODO : instanz prüfen
      if (call.arguments.length != 1) {
        Logger.error("trace() must have one argument")
      }
      trace(call.arguments.head)  // TODO: Zeus, zu testen
/*
    case IR_ExpressionStatement(call) if (call.isInstanceOf[IR_FunctionCall]) && (call.asInstanceOf[IR_FunctionCall].name == "eigen")            =>
      val call_res = call.asInstanceOf[IR_FunctionCall]
      if (call_res.arguments.length != 2) {
        Logger.error("eigen() must have two arguments")
      }
      eigenvalue(call_res.arguments(0), call_res.arguments(1)) // TODO: Zeus, zu testen
*/
/*
    case call : IR_FunctionCall if (call.name == "printTensor")                                   =>
      if (call.arguments.length != 1) {
        Logger.error("printTensor() must have two arguments")
      }
      printTensor(call.arguments.head) // TODO: Zeus, zu testen*/
  })
}