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

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatNodes.IR_GetElement
import exastencils.baseExt.ir.IR_MatNodes.IR_GetSlice
import exastencils.baseExt.ir.IR_MatNodes.IR_SetElement
import exastencils.baseExt.ir.IR_MatNodes.IR_SetSlice
import exastencils.datastructures.Transformation.OutputType
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.prettyprinting._
import exastencils.util.ir._

/// IR_HackMatComponentAccess
// FIXME: update with actual accessors
case class IR_HackMatComponentAccess(var mat : IR_VariableAccess, var i : IR_Expression, var j : IR_Expression) extends IR_Expression {
  override def datatype = mat.datatype
  override def prettyprint(out : PpStream) : Unit = out << mat << "(" << i << ", " << j << ")"
}

object IR_MatrixAccess {
  def apply(acc : IR_Expression, idxy : IR_Index, idxx : Option[IR_Index]) = {
    new IR_MatrixAccess(acc.asInstanceOf[IR_Access], idxy, idxx)
  }
}

case class IR_MatrixAccess(acc : IR_Access, idxy : IR_Index, idxx : Option[IR_Index]) extends IR_Access {
  override def datatype : IR_Datatype = acc.datatype
  // override def prettyprint(out : PpStream) : Unit = Logger.error("internal node not resolved!")
  override def prettyprint(out : PpStream) : Unit = out << "MatAcc(" << acc << ")"
  def expand(lval : Boolean, rhsExpr : Option[IR_Expression]) : OutputType = {
    val indices_y : Array[IR_Expression] = idxy match {
      case _ @ IR_ExpressionIndex(idxs) => idxs
      case _ @ IR_ConstIndex(idxs)      => idxs.map(i => IR_IntegerConstant(i))
      case _ @ IR_RangeIndex(range)     => Array[IR_Expression](range(0).begin.getOrElse(IR_IntegerConstant(0)), range(0).end.getOrElse(IR_IntegerConstant(acc.datatype.asInstanceOf[IR_MatrixDatatype].sizeM)))
    }
    val indices_x : Array[IR_Expression] =
      idxx.get match {
        case _ @ IR_ExpressionIndex(idxs) => idxs
        case _ @ IR_ConstIndex(idxs)      => idxs.map(i => IR_IntegerConstant(i))
        case _ @ IR_RangeIndex(range)                    => Array[IR_Expression](range(0).begin.getOrElse(IR_IntegerConstant(0)), range(0).end.getOrElse(IR_IntegerConstant(acc.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)))
      }

    (indices_y.length, indices_x.length) match {
      case (1, 0) if (acc.datatype.asInstanceOf[IR_MatrixDatatype].sizeN == 1) =>
        if (!lval)
          IR_GetElement(ListBuffer[IR_Expression](acc, indices_y(0), IR_IntegerConstant(0)))
        else IR_SetElement(ListBuffer[IR_Expression](acc, indices_y(0), IR_IntegerConstant(0), rhsExpr.get))
      case (1, 0) =>
        if (!lval)
          IR_GetSlice(ListBuffer[IR_Expression](acc, indices_y(0), IR_IntegerConstant(0), IR_IntegerConstant(1), IR_IntegerConstant(acc.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)))
        else IR_SetSlice(ListBuffer[IR_Expression](acc, indices_y(0), IR_IntegerConstant(0), IR_IntegerConstant(1), IR_IntegerConstant(acc.datatype.asInstanceOf[IR_MatrixDatatype].sizeN), rhsExpr.get))
      case (2, 0) =>
        if (!lval) IR_GetSlice(ListBuffer[IR_Expression](acc, indices_y(0), IR_IntegerConstant(0), indices_y(1) - indices_y(0), IR_IntegerConstant(acc.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)))
        else IR_SetSlice(ListBuffer[IR_Expression](acc, indices_y(0), IR_IntegerConstant(0), indices_y(1) - indices_y(0), IR_IntegerConstant(acc.datatype.asInstanceOf[IR_MatrixDatatype].sizeN), rhsExpr.get))
      case (1, 1) =>
        if (!lval) IR_GetElement(ListBuffer[IR_Expression](acc, indices_y(0), indices_x(0)))
        else IR_SetElement(ListBuffer[IR_Expression](acc, indices_y(0), indices_x(0), rhsExpr.get))
      case (2, 1) =>
        if (!lval) IR_GetSlice(ListBuffer[IR_Expression](acc, indices_y(0), indices_x(0), indices_y(1) - indices_y(0), IR_IntegerConstant(1)))
        else IR_SetSlice(ListBuffer[IR_Expression](acc, indices_y(0), indices_x(0), indices_y(1) - indices_y(0), IR_IntegerConstant(1), rhsExpr.get))
      case (1, 2) =>
        if (!lval) IR_GetSlice(ListBuffer[IR_Expression](acc, indices_y(0), indices_x(0), IR_IntegerConstant(1), indices_x(1) - indices_x(0)))
        else IR_SetSlice(ListBuffer[IR_Expression](acc, indices_y(0), indices_x(0), IR_IntegerConstant(1), indices_x(1) - indices_x(0), rhsExpr.get))
      case (2, 2) =>
        if (!lval) IR_GetSlice(ListBuffer[IR_Expression](acc, indices_y(0), indices_x(0), indices_y(1) - indices_y(0), indices_x(1) - indices_x(0)))
        else IR_SetSlice(ListBuffer[IR_Expression](acc, indices_y(0), indices_x(0), indices_y(1) - indices_y(0), indices_x(1) - indices_x(0), rhsExpr.get))
      case _ => Logger.error(s"unexpected index combination: ${ indices_x.length }, ${ indices_y.length }")
    }

  }

      val me = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          me.set(row, col, IR_Power(getElem(left, row, col), right))
        }
      }
      me

    case call : IR_FunctionCall if call.name == "transpose" =>
      if (call.arguments.length != 1) {
        Logger.error(s"Transpose operation must have one arguments; has ${ call.arguments.length }")
      }

      val left = call.arguments(0)
      var transform = true

      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case _                                                   => transform = false; (0, 0)
      }
      if (transform) {
        val me = IR_MatrixExpression(left.datatype, lsize._2, lsize._1)
        for (row <- 0 until lsize._1) {
          for (col <- 0 until lsize._2) {
            me.set(col, row, getElem(left, row, col))
          }
        }
        me
      } else {
        call
      }

    case call : IR_FunctionCall if call.name == "getRow" =>
      if (call.arguments.length != 2) {
        Logger.error(s"getRow() must have 2 arguments for matrices; has ${ call.arguments.length }")
      }
      if (!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("getRow() may only be used for matrix datatypes")
      }
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      if (m.sizeM == 1 || m.sizeN == 1) {
        Logger.error("getRow() may not be used for vectors")
      }

      var expressions = ListBuffer[IR_Expression]()
      for (r <- 0 until m.sizeN) {
        expressions += getElem(call.arguments(0), call.arguments(1).asInstanceOf[IR_IntegerConstant].v.intValue(), r)
      }
      IR_MatrixExpression(Some(m.resolveBaseDatatype), 1, m.sizeN, expressions.toArray)

    case call : IR_FunctionCall if call.name == "getColumn" =>
      if (call.arguments.length != 2) {
        Logger.error(s"getColumn() must have 2 arguments for matrices; has ${ call.arguments.length }")
      }
      if (!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("getColumn() may only be used for matrix datatypes")
      }
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      if (m.sizeM == 1 || m.sizeN == 1) {
        Logger.error("getColumn() may not be used for vectors")
      }

      var expressions = ListBuffer[IR_Expression]()
      for (r <- 0 until m.sizeM) {
        expressions += getElem(call.arguments(0), r, call.arguments(1).asInstanceOf[IR_IntegerConstant].v.intValue())
      }
      IR_MatrixExpression(Some(m.resolveBaseDatatype), m.sizeM, 1, expressions.toArray)

    case call : IR_FunctionCall if call.name == "getElement" =>
      if (!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("getElement() may only be used for matrix datatypes")
      }
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      val itm = Array(IR_IntegerConstant(0), IR_IntegerConstant(0))

      if (m.sizeM > 1 && m.sizeN > 1) {
        if (call.arguments.length != 3) {
          Logger.error(s"getElement() must have 3 arguments for matrices; has ${ call.arguments.length }")
        }
        itm(0) = call.arguments(1).asInstanceOf[IR_IntegerConstant]
        itm(1) = call.arguments(2).asInstanceOf[IR_IntegerConstant]
      } else if (m.sizeM == 1 && m.sizeN > 1) {
        if (call.arguments.length != 2) {
          Logger.error(s"getElement() must have 2 arguments for vectors; has ${ call.arguments.length }")
        }
        itm(0) = call.arguments(1).asInstanceOf[IR_IntegerConstant]
      } else if (m.sizeM > 1 && m.sizeN == 1) {
        if (call.arguments.length != 2) {
          Logger.error(s"getElement() must have 2 arguments for vectors; has ${ call.arguments.length }")
        }
        itm(1) = call.arguments(1).asInstanceOf[IR_IntegerConstant]
      }

      var ret : IR_Expression = call

      call.arguments(0) match {
        case x : IR_MatrixExpression                                                                        => ret = getElem(x, itm(0).asInstanceOf[IR_IntegerConstant].v.intValue(), itm(1).asInstanceOf[IR_IntegerConstant].v.intValue())
        case x @ (_ : IR_FieldAccess | _ : IR_VariableAccess) if x.datatype.isInstanceOf[IR_MatrixDatatype] => ret = IR_HighDimAccess(call.arguments(0), IR_ExpressionIndex(itm(0), itm(1)))
        case _                                                                                              => Logger.error(s"Unhandled argument ${ call.arguments(0) } for getElement()")
      }
      ret

    case IR_ExpressionStatement(call : IR_FunctionCall) if call.name == "setRow" =>
      if (call.arguments.length != 3) {
        Logger.error(s"setRow() must have 3 arguments for matrices; has ${ call.arguments.length }")
      }
      if (!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("setRow() may only be used for matrix datatypes")
      }
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      if (m.sizeM == 1 || m.sizeN == 1) {
        Logger.error("setRow() may not be used for vectors")
      }
      val v = call.arguments(2)
      if (!v.datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("Argument 3 of setRow() must be vector")
      }

      var stmts = ListBuffer[IR_Statement]()
      for (r <- 0 until m.sizeN) {
        stmts += IR_Assignment(IR_HighDimAccess(call.arguments(0), IR_ExpressionIndex(call.arguments(1), r)), IR_HighDimAccess(v, IR_ExpressionIndex(r)))
      }
      stmts

    case IR_ExpressionStatement(call : IR_FunctionCall) if call.name == "setColumn" =>
      if (call.arguments.length != 3) {
        Logger.error(s"setColumn() must have 3 arguments for matrices; has ${ call.arguments.length }")
      }
      if (!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("setColumn() may only be used for matrix datatypes")
      }
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      if (m.sizeM == 1 || m.sizeN == 1) {
        Logger.error("setColumn() may not be used for vectors")
      }
      val v = call.arguments(2)
      if (!v.datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("Argument 3 of setColumn() must be vector")
      }

      var stmts = ListBuffer[IR_Statement]()
      for (r <- 0 until m.sizeN) {
        stmts += IR_Assignment(IR_HighDimAccess(call.arguments(0), IR_ExpressionIndex(r, call.arguments(1))), IR_HighDimAccess(v, IR_ExpressionIndex(r)))
      }
      stmts

    case IR_ExpressionStatement(call : IR_FunctionCall) if call.name == "setElement" =>
      if (!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("setElement() may only be used for matrix datatypes")
      }
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      val itm = Array(IR_IntegerConstant(0), IR_IntegerConstant(0))
      var obj : IR_Expression = null
      if (m.sizeM > 1 && m.sizeN > 1) {
        if (call.arguments.length != 4) {
          Logger.error(s"setElement() must have 4 arguments for matrices; has ${ call.arguments.length }")
        }
        itm(0) = call.arguments(1).asInstanceOf[IR_IntegerConstant]
        itm(1) = call.arguments(2).asInstanceOf[IR_IntegerConstant]
        obj = call.arguments(3)
      } else if (m.sizeM == 1 && m.sizeN > 1) {
        if (call.arguments.length != 3) {
          Logger.error(s"setElement() must have 3 arguments for vectors; has ${ call.arguments.length }")
        }
        itm(0) = call.arguments(1).asInstanceOf[IR_IntegerConstant]
        obj = call.arguments(2)
      } else if (m.sizeM > 1 && m.sizeN == 1) {
        if (call.arguments.length != 3) {
          Logger.error(s"setElement() must have 3 arguments for vectors; has ${ call.arguments.length }")
        }
        itm(1) = call.arguments(1).asInstanceOf[IR_IntegerConstant]
        obj = call.arguments(2)
      }
      IR_Assignment(IR_HighDimAccess(call.arguments(0), IR_ExpressionIndex(itm(0), itm(1))), obj)

  })

  if (Knowledge.experimental_resolveInverseFunctionCall == "Runtime") {
    def runtimeInverse(in : IR_VariableAccess, out : IR_VariableAccess) = {
      val debug = false

      def printMatrix(matrix : IR_VariableAccess) = {
        val stmts = ListBuffer[IR_Statement]()
        matrix.datatype match {
          case dt : IR_MatrixDatatype =>
            for (i <- 0 until dt.sizeM) {
              for (j <- 0 until dt.sizeN) {
                stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, ListBuffer[IR_Expression](IR_StringConstant("%e "), IR_HighDimAccess(matrix, IR_ConstIndex(i, j)))))
              }
              stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("\\n")))
            }
        }
        stmts
      }
      def mkConstant(dt : IR_Datatype, v : Double) = dt match {
        case IR_RealDatatype    => IR_RealConstant(v)
        case IR_IntegerDatatype => IR_IntegerConstant(v.toInt)
        case _                  => exastencils.logger.Logger.error("mkConstant not implemented for " + dt.toString)
      }

      val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]

      val N = inDt.sizeM
      val myType = inDt.resolveBaseDatatype
      val func = IR_Scope(Nil)
      val myL = IR_VariableAccess("_L", IR_MatrixDatatype(myType, N, N))
      val myU = IR_VariableAccess("_U", IR_MatrixDatatype(myType, N, N))
      val myQ = IR_VariableAccess("_q", IR_ArrayDatatype(IR_IntegerDatatype, N))
      val myI = IR_VariableAccess("_i", IR_IntegerDatatype)
      val myJ = IR_VariableAccess("_j", IR_IntegerDatatype)
      val myK = IR_VariableAccess("_k", IR_IntegerDatatype)
      func.body += IR_VariableDeclaration(myL)
      func.body += IR_VariableDeclaration(myU)
      func.body += IR_Assignment(myL, mkConstant(myType, 0))
      func.body += IR_Assignment(myU, in)
      func.body += IR_VariableDeclaration(myQ)
      func.body += IR_Assignment(out, mkConstant(myType, 0))
      for (i <- 0 until N) {
        func.body += IR_Assignment(IR_HighDimAccess(myL, IR_ConstIndex(i, i)), mkConstant(myType, 1))
      }
      for (i <- 0 until N) {
        func.body += IR_Assignment(IR_ArrayAccess(myQ, IR_IntegerConstant(i)), IR_IntegerConstant(i))
      }
      val myColmax = IR_VariableAccess("_colmax", myType)
      val myMaxCol = IR_VariableAccess("_maxCol", IR_IntegerDatatype)
      func.body += IR_ForLoop(IR_VariableDeclaration(myK, IR_IntegerConstant(0)), IR_Lower(myK, IR_IntegerConstant(N - 1)), IR_ExpressionStatement(IR_PreIncrement(myK)), ListBuffer[IR_Statement](
        IR_VariableDeclaration(myColmax, IR_RealConstant(0)),
        IR_VariableDeclaration(myMaxCol, myK),
        IR_ForLoop(IR_VariableDeclaration(myJ, myK), IR_Lower(myJ, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myJ)), ListBuffer[IR_Statement](
          IR_IfCondition(IR_Greater(IR_FunctionCall(IR_MathFunctionReference.fabs, IR_HighDimAccess(myU, IR_ExpressionIndex(myK, myJ))), myColmax), ListBuffer[IR_Statement](
            IR_Assignment(myColmax, IR_FunctionCall(IR_MathFunctionReference.fabs, IR_HighDimAccess(myU, IR_ExpressionIndex(myK, myJ)))),
            IR_Assignment(myMaxCol, myJ)
          ))
        )),
        if (debug) IR_IfCondition(IR_Lower(myColmax, mkConstant(myType, 1e-15)),
          IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[WARNING] Inverting potentially singular matrix\\n"))) +:
            printMatrix(myU)
        ) else IR_NullStatement,
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::swap"), IR_ArrayAccess(myQ, myK), IR_ArrayAccess(myQ, myMaxCol)))
      ))
      for (i <- 0 until N) {
        func.body.last.asInstanceOf[IR_ForLoop].body += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::swap"), IR_HighDimAccess(myL, IR_ExpressionIndex(IR_IntegerConstant(i), myK)), IR_HighDimAccess(myL, IR_ExpressionIndex(IR_IntegerConstant(i), myMaxCol))))
      }
      for (i <- 0 until N) {
        func.body.last.asInstanceOf[IR_ForLoop].body += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::swap"), IR_HighDimAccess(myU, IR_ExpressionIndex(IR_IntegerConstant(i), myK)), IR_HighDimAccess(myU, IR_ExpressionIndex(IR_IntegerConstant(i), myMaxCol))))
      }
      func.body.last.asInstanceOf[IR_ForLoop].body += IR_ForLoop(IR_VariableDeclaration(myI, myK + IR_IntegerConstant(1)), IR_Lower(myI, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myI)), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myK)), IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myK)) / IR_HighDimAccess(myU, IR_ExpressionIndex(myK, myK))),
        IR_ForLoop(IR_VariableDeclaration(myJ, myK), IR_Lower(myJ, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myJ)), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myJ)), IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myJ)) - IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myK)) * IR_HighDimAccess(myU, IR_ExpressionIndex(myK, myJ)))
        ))
      ))
      val myY = IR_VariableAccess("_y", IR_MatrixDatatype(myType, N, N))
      val mySum = IR_VariableAccess("_sum", myType)
      func.body += IR_VariableDeclaration(myY)
      func.body += IR_ForLoop(IR_VariableDeclaration(myJ, IR_IntegerConstant(0)), IR_Lower(myJ, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myJ)), ListBuffer[IR_Statement](
        IR_ForLoop(IR_VariableDeclaration(myI, IR_IntegerConstant(0)), IR_Lower(myI, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myI)), ListBuffer[IR_Statement](
          IR_VariableDeclaration(mySum, IR_RealConstant(0)),
          IR_ForLoop(IR_VariableDeclaration(myK, IR_IntegerConstant(0)), IR_Lower(myK, myI), IR_ExpressionStatement(IR_PreIncrement(myK)), ListBuffer[IR_Statement](
            IR_Assignment(mySum, mySum + IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myK)) * IR_HighDimAccess(myY, IR_ExpressionIndex(myK, myJ)))
          )),
          IR_IfCondition(IR_EqEq(myI, myJ),
            IR_Assignment(IR_HighDimAccess(myY, IR_ExpressionIndex(myI, myJ)), (mkConstant(myType, 1) - mySum) / IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myI))),
            IR_Assignment(IR_HighDimAccess(myY, IR_ExpressionIndex(myI, myJ)), (mkConstant(myType, 0) - mySum) / IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myI)))
          )
        )),
        IR_ForLoop(IR_VariableDeclaration(myI, IR_IntegerConstant(N) - IR_IntegerConstant(1)), IR_GreaterEqual(myI, IR_IntegerConstant(0)), IR_ExpressionStatement(IR_PreDecrement(myI)), ListBuffer[IR_Statement](
          IR_VariableDeclaration(mySum, IR_RealConstant(0)),
          IR_ForLoop(IR_VariableDeclaration(myK, IR_IntegerConstant(N) - IR_IntegerConstant(1)), IR_Greater(myK, myI), IR_ExpressionStatement(IR_PreDecrement(myK)), ListBuffer[IR_Statement](
            IR_Assignment(mySum, mySum + IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myK)) * IR_HighDimAccess(out, IR_ExpressionIndex(myK, myJ)))
          )),
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(myI, myJ)), (IR_HighDimAccess(myY, IR_ExpressionIndex(myI, myJ)) - mySum) / IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myI)))
        ))
      ))

      func
    }
    this += new Transformation("resolve runtime inversion", {
      case IR_ExpressionStatement(call @ IR_FunctionCall(_, ListBuffer(in : IR_VariableAccess, out : IR_VariableAccess))) if (call.name == "_runtimeInverseMatrix") =>
        runtimeInverse(in, out)
    })
  }
}

object IR_ResolveMatrixAssignments extends DefaultStrategy("Resolve assignments to matrices") {
  val annotationMatrixRow = "IR_ResolveMatrices.matrixRow"
  val annotationMatrixCol = "IR_ResolveMatrices.matrixCol"

  this += new Transformation("scalarize 1/2", {
    case stmt : IR_VariableDeclaration => stmt

    case IR_Assignment(dest, num : IR_Number, "=") if dest.datatype.isInstanceOf[IR_MatrixDatatype] && !dest.isInstanceOf[IR_MatrixExpression] =>
      val dt = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      IR_FunctionCall("std::fill", ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + dt.resolveFlattendSize, num)) : IR_Statement

    case IR_Assignment(dest, src : IR_VariableAccess, "=") if dest.datatype.isInstanceOf[IR_MatrixDatatype] && !dest.isInstanceOf[IR_MatrixExpression] && src.datatype.isInstanceOf[IR_MatrixDatatype] =>
      val dt = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      IR_FunctionCall("std::copy", ListBuffer[IR_Expression](Duplicate(src), Duplicate(src) + dt.resolveFlattendSize, dest)) : IR_Statement

    case stmt @ IR_Assignment(dest, _, _) if (dest.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      val matrix = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      var newStmts = ListBuffer[IR_Statement]()
      for (row <- 0 until matrix.sizeM) {
        for (col <- 0 until matrix.sizeN) {
          var cloned = Duplicate(stmt)
          StateManager.findAll[IR_Expression](cloned).foreach {
            case _ : IR_FunctionArgument                                                                                                            => // do not mark function arguments to be resolved into individual accesses
            case x @ (_ : IR_VariableAccess | _ : IR_MatrixExpression | _ : IR_MultiDimFieldAccess) if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => {
              x.annotate(annotationMatrixRow, row)
              x.annotate(annotationMatrixCol, col)
            }
            case exp                                                                                                                                =>
          }
          newStmts += cloned
        }
      }
      newStmts
  })

  this += new Transformation("expressions 2/2", {
    case exp : IR_MatrixExpression if (exp.hasAnnotation(annotationMatrixRow)) =>
      exp.get(exp.popAnnotationAs[Int](annotationMatrixRow), exp.popAnnotationAs[Int](annotationMatrixCol))

    case exp : IR_Expression if (exp.hasAnnotation(annotationMatrixRow)) =>
      IR_HighDimAccess(Duplicate(exp), IR_ConstIndex(Array(exp.popAnnotationAs[Int](annotationMatrixRow), exp.popAnnotationAs[Int](annotationMatrixCol))))
  }, false)
}

object IR_SetupMatrixExpressions extends DefaultStrategy("Convert accesses to matrices and vectors to MatrixExpressions") {
  def duplicateExpressions(access : IR_Expression, dt : IR_MatrixDatatype) = {
    var expressions = ListBuffer[IR_Expression]()
    for (row <- 0 until dt.sizeM)
      for (col <- 0 until dt.sizeN)
        expressions += IR_HighDimAccess(Duplicate(access), IR_ConstIndex(row, col))
    expressions.toArray
  }

  this += Transformation("Wrap", {
    case m @ IR_MatrixExpression(_, 1, 1)             => m.get(0, 0)
    case IR_MatrixDatatype(dt, 1, 1)                  => dt
    case m : IR_MatrixExpression                      => m // no need to process further
    case hda : IR_HighDimAccess                       => hda // no need to process further
    case x : IR_FunctionCall if (x.name != "inverse") => x

    case access @ IR_VariableAccess(_, m : IR_MatrixDatatype) if (m.sizeM > 1 || m.sizeN > 1) => IR_MatrixExpression(Some(m.datatype), m.sizeM, m.sizeN, duplicateExpressions(access, m))

    case access : IR_MultiDimFieldAccess if access.datatype.isInstanceOf[IR_MatrixDatatype] =>
      val m = access.datatype.asInstanceOf[IR_MatrixDatatype]
      if (m.sizeM > 1 || m.sizeN > 1)
        IR_MatrixExpression(Some(m.datatype), m.sizeM, m.sizeN, duplicateExpressions(access, m))
      else
        access

    // FIXME: add support for stencil fields
  }, false)
}

object IR_LinearizeMatrices extends DefaultStrategy("Linearize matrices") {
  this += Transformation("Linearize", {
    case IR_HighDimAccess(base, _) if (!base.datatype.isInstanceOf[IR_MatrixDatatype]) => base

    case IR_HighDimAccess(base : IR_MultiDimFieldAccess, idx : IR_Index) =>
      val hoIdx = idx.toExpressionIndex
      val fieldLayout = base.field.layout
      for (dim <- fieldLayout.numDimsGrid until fieldLayout.numDimsData) {
        if (base.index.indices.length <= dim)
          base.index.indices :+= hoIdx(dim - fieldLayout.numDimsGrid)
        else
          base.index.indices(dim) += hoIdx(dim - fieldLayout.numDimsGrid)
      }
      base

    case IR_HighDimAccess(base, idx : IR_ConstIndex) if idx.indices.length == 2 =>
      val matrix = base.datatype.asInstanceOf[IR_MatrixDatatype]
      if (matrix.sizeM > 1 || matrix.sizeN > 1 || idx(0) > 0 || idx(1) > 0)
        IR_ArrayAccess(base, matrix.sizeN * idx.indices(0) + idx.indices(1))
      else
        base

    case IR_HighDimAccess(base, idx : IR_ExpressionIndex) if idx.indices.length == 2 =>
      val matrix = base.datatype.asInstanceOf[IR_MatrixDatatype]
      if (matrix.sizeM > 1 || matrix.sizeN > 1)
        IR_ArrayAccess(base, matrix.sizeN * idx.indices(0) + idx.indices(1))
      else
        base
  }, false)
}
