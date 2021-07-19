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

package exastencils.baseExt.ir.IR_MatOperations

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_ExpressionStatement
import exastencils.base.ir.IR_FloatDatatype
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_ScalarFree
import exastencils.base.ir.IR_SizeOf
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_CompiletimeMatOps
import exastencils.baseExt.ir.IR_ClassifyMatShape
import exastencils.baseExt.ir.IR_MatShape
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.baseExt.ir.IR_MatNodeUtils._
import exastencils.config._
import exastencils.core._
import exastencils.logger.Logger
import exastencils.util.ir._

// generate simple operations with matrices for execution at runtime for e.g. an inversion at runtime
object IR_GenerateBasicMatrixOperations {
  var tmpCounter = 0
  var pivotVarCounter = 0

  // generate code to copy a matrix per std::copy
  def copyMatrix(dest : IR_VariableAccess, src : IR_Expression) : ListBuffer[IR_Statement] = {
    val destSize = IR_CompiletimeMatOps.getSize(dest)
    val srcSize = IR_CompiletimeMatOps.getSize(src)
    if (destSize != srcSize)
      Logger.error("sizes do not match: " + destSize + " vs " + srcSize)
    var srcDt = src.datatype.resolveBaseDatatype
    var destDt = dest.datatype.resolveBaseDatatype
    if (destDt != srcDt && !((destDt == IR_RealDatatype || destDt == IR_DoubleDatatype) && (srcDt == IR_RealDatatype || srcDt == IR_DoubleDatatype)))
      Logger.error("Datatypes do not match: destination datatype=" + destDt + " vs source datatype=" + srcDt)
    var stmts = ListBuffer[IR_Statement]()
    src match {
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        stmts += IR_FunctionCall(IR_ExternalFunctionReference("std::copy", IR_UnitDatatype), ListBuffer[IR_Expression](va,(va + destSize._1 * destSize._2), (dest)))
      case x @ IR_MatrixExpression(_, _,_, _)                      =>
        var tmpAccess = IR_VariableAccess("copyTmp_" + tmpCounter, src.datatype)
        tmpCounter += 1
        stmts += IR_VariableDeclaration(tmpAccess, src)
        stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::copy", IR_UnitDatatype), ListBuffer[IR_Expression]((tmpAccess),(tmpAccess +  destSize._1 * destSize._2), dest)))
      case _                                                     => Logger.error("unexpected datatype: " + src + ", expected matrix expression or access to matrix variable")
    }
  }

  // generate code to print a matrix
  def printMatrix(matrix : IR_Expression) = {
    val stmts = ListBuffer[IR_Statement]()
    matrix match {
      case IR_VariableAccess(_, dt @ IR_MatrixDatatype(_, _, _)) =>
        for (i <- 0 until dt.sizeM) {
          for (j <- 0 until dt.sizeN) {
            stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, ListBuffer[IR_Expression](IR_StringConstant("%e "), IR_HighDimAccess(matrix, IR_ConstIndex(i, j)))))
          }
          stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("\\n")))
        }
      case va @ IR_VariableAccess(_,  _) =>
        stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, ListBuffer[IR_Expression](IR_StringConstant("%e "), va)))
  }
    stmts
  }

  // print a matrix from a pointer and the size
  def printMatrixPointer(matrix : IR_Expression, mat_rows : IR_Expression, mat_cols : IR_Expression) : IR_Scope = {
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    var stmts = ListBuffer[IR_Statement]()
    matrix match {
      case IR_VariableAccess(_, IR_PointerDatatype(_)) =>
        stmts += IR_ForLoop(IR_VariableDeclaration(i, IR_IntegerConstant(0)), IR_Lower(i, mat_rows), IR_PreIncrement(i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(j, IR_IntegerConstant(0)), IR_Lower(j, mat_cols), IR_PreIncrement(j), ListBuffer[IR_Statement](
            IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, ListBuffer[IR_Expression](IR_StringConstant("%f "), IR_ArrayAccess(matrix, i * mat_cols + j))))
          )),
          IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("\\n")))
        ))
    }
    IR_Scope(stmts)
  }

  // generate a compare function for two matrices
  def compare(left : IR_Expression, right : IR_Expression, precision : IR_Expression, returnStmt : Boolean) : IR_Scope = {
    var func = IR_Scope(Nil)
    var outstream = IR_VariableAccess("std::cout", IR_StringDatatype)
    (left.datatype, right.datatype) match {
      case matrices @ (IR_MatrixDatatype(leftDt, sizeMLeft, sizeNLeft), IR_MatrixDatatype(rightDt, sizeMRight, sizeNRight))                                                     =>
        if ((sizeMLeft, sizeNLeft) != (sizeMRight, sizeNRight))
          Logger.error("sizes do not match: " + (sizeMLeft, sizeNLeft) + " vs " + (sizeMRight, sizeNRight))
        var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
        var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, IR_IntegerConstant(0)), IR_Lower(_i, sizeMLeft), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, sizeNLeft), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_IfCondition(IR_Greater(IR_FunctionCall(IR_ExternalFunctionReference.fabs, IR_Subtraction(IR_FunctionCall(IR_ExternalFunctionReference.fabs, IR_HighDimAccess(left, IR_ExpressionIndex(_i, _j))), IR_FunctionCall(IR_ExternalFunctionReference.fabs, IR_HighDimAccess(right, IR_ExpressionIndex(_i, _j))))), precision), ListBuffer[IR_Statement](
              IR_Print(outstream, ListBuffer[IR_Expression](IR_StringConstant("[Test] comparison failed at "), _i, IR_StringConstant(" "), _j, IR_StringConstant("\\n"), IR_HighDimAccess(left, IR_ExpressionIndex(_i, _j)), IR_StringConstant(" vs "), IR_HighDimAccess(right, IR_ExpressionIndex(_i, _j)), IR_StringConstant("\\n"))),
              if(returnStmt) IR_Return(IR_IntegerConstant(-1)) else IR_NullStatement
            ), ListBuffer[IR_Statement]())
          ))
        ))
      case scalars @ (IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype | IR_FloatDatatype, IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype | IR_FloatDatatype) =>
        func.body += IR_IfCondition(IR_Greater(IR_FunctionCall(IR_ExternalFunctionReference.fabs, IR_Subtraction(left, right)), precision), ListBuffer[IR_Statement](
          IR_Print(outstream, ListBuffer[IR_Expression](IR_StringConstant("[Test] comparison failed: "), left, IR_StringConstant(" vs "), right, IR_StringConstant("\\n")))
        ))
      case _ => Logger.error(s"unexpected combination: ${left.datatype}, ${right.datatype}")
    }
    func
  }

  def mkConstant(dt : IR_Datatype, v : Double) = dt match {
    case IR_RealDatatype    => IR_RealConstant(v)
    case IR_IntegerDatatype => IR_IntegerConstant(v.toInt)
    case _                  => exastencils.logger.Logger.error("mkConstant not implemented for " + dt.toString)
  }

  // generate code to transpose a matrix
  def transpose(in : IR_VariableAccess, out : IR_VariableAccess) : IR_Scope = {
    var insize = IR_CompiletimeMatOps.getSize(in)
    var outsize = IR_CompiletimeMatOps.getSize(out)
    if (insize._1 != outsize._2 || insize._2 != outsize._1)
      Logger.error("sizes do not match")
    var N = insize._1
    var M = insize._2
    var func = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    func.body += IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, N), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, M), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(j, i)), IR_HighDimAccess(in, IR_ExpressionIndex(i, j)))
      ))
    ))
    func
  }

  // generate code to elementwise multiply a scalar with a matrix
  def elementwiseMultiplication(scalar : IR_Expression, in : IR_VariableAccess, out : IR_VariableAccess) : IR_Scope = {
    var insize = IR_CompiletimeMatOps.getSize(in)
    var outsize = IR_CompiletimeMatOps.getSize(out)
    if (insize._1 != outsize._1 || insize._2 != outsize._2)
      Logger.error("sizes do not match: " + insize + " vs " + outsize)
    var N = insize._1
    var M = insize._2
    var func = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    func.body += IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, N), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, M), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i, j)), IR_Multiplication(scalar, IR_HighDimAccess(in, IR_ExpressionIndex(i, j))))
      ))
    ))
    func
  }

  // multiply 'left' and 'right' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def multAtSubmatrix(left : IR_VariableAccess, right : IR_VariableAccess, out : IR_VariableAccess, M : IR_Expression, N : IR_Expression, K : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression) : IR_Scope = {
    var func = IR_Scope(Nil)
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    var _k = IR_VariableAccess("_k", IR_IntegerDatatype)
    left match {
      case m @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), 0),
            IR_ForLoop(IR_VariableDeclaration(_k, 0), IR_Lower(_k, K), IR_PreIncrement(_k), ListBuffer[IR_Statement](
              IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Addition(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Multiplication(IR_HighDimAccess(left, IR_ExpressionIndex(_i, _k)), IR_HighDimAccess(right, IR_ExpressionIndex(_k, _j)))))
            ))
          ))
        ))
      case p @ IR_VariableAccess(_, IR_PointerDatatype(_))      =>
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_Assignment(IR_ArrayAccess(out, (_i + offset_r) * N + _j + offset_c), 0),
            IR_ForLoop(IR_VariableDeclaration(_k, 0), IR_Lower(_k, K), IR_PreIncrement(_k), ListBuffer[IR_Statement](
              IR_Assignment(IR_ArrayAccess(out, (_i + offset_r) * N + _j + offset_c), IR_Addition(IR_ArrayAccess(out, (_i + offset_r) * N + _j + offset_c), IR_Multiplication(IR_ArrayAccess(left, _i * K + _k), IR_ArrayAccess(right, _k * N + _j))))
            ))
          ))
        ))
      case _ => Logger.error("unexprected combination")
    }
    func
  }

  // add 'left' and 'right' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def addAtSubmatrix(left : IR_Access, right : IR_Access, out : IR_Access, outsize : IR_Expression, oprows : IR_Expression, opcols : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression) : IR_Scope = {
    var func = IR_Scope(Nil)
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    left match {
      case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, oprows), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, opcols), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Addition(IR_HighDimAccess(left, IR_ExpressionIndex(_i, _j)), IR_HighDimAccess(right, IR_ExpressionIndex(_i, _j))))
          ))
        ))
      case IR_VariableAccess(_, IR_PointerDatatype(_))      =>
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, oprows), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, opcols), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_Assignment(IR_ArrayAccess(out, (_i + offset_r) * outsize + _j + offset_c), IR_Addition(IR_ArrayAccess(left, (_i * opcols + _j)), IR_ArrayAccess(right, (_i * opcols + _j))))
          ))
        ))
    }
    func
  }

  // subtract 'left' from 'right' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def subAtSubmatrix(left : IR_Access, right : IR_VariableAccess, out : IR_Access, opcols : IR_Expression, oprows : IR_Expression, destcols : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression) : IR_Scope = {
    var func = IR_Scope(Nil)
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    left match {
      case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, oprows), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, opcols), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Subtraction(IR_HighDimAccess(left, IR_ExpressionIndex(_i, _j)), IR_HighDimAccess(right, IR_ExpressionIndex(_i, _j))))
          ))
        ))
      case IR_VariableAccess(_, IR_PointerDatatype(_))      =>
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, oprows), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, opcols), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_Assignment(IR_ArrayAccess(out, (_i + offset_r) * destcols + _j + offset_c), IR_Subtraction(IR_ArrayAccess(left, _i * opcols + _j), IR_ArrayAccess(right, (_i * opcols + _j))))
          ))
        ))
    }
    func
  }

  // produce negative of 'that' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def negAtSubmatrix(that : IR_Access, out : IR_Access, outsize : IR_Expression, that_rows : IR_Expression, that_cols : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression) : IR_Scope = {
    var func = IR_Scope(Nil)
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    that match {
      case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, that_rows), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, that_cols), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Negative(IR_HighDimAccess(that, IR_ExpressionIndex(_i, _j))))
          ))
        ))
      case IR_VariableAccess(_, IR_PointerDatatype(_))      =>
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, that_rows), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, that_cols), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_Assignment(IR_ArrayAccess(out, (_i + offset_r) * outsize + _j + offset_c), IR_Negative(IR_ArrayAccess(that, _i * that_cols + _j)))
          ))
        ))
    }
    func
  }

  // copy a submatrix of n_rows x n_cols to 'copy' from position 'offset_r', 'offset_c' in 'source'
  def loopCopySubmatrix(source : IR_Expression, dest : IR_Access, offset_r : IR_Expression, offset_c : IR_Expression, n_rows : IR_Expression, n_cols : IR_Expression) : IR_Scope = {
    var stmts = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    source match {
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        stmts.body += IR_ForLoop(IR_VariableDeclaration(i, offset_r), IR_Lower(i, n_rows + offset_r), IR_PreIncrement(i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(j, offset_c), IR_Lower(j, offset_c + n_cols), IR_PreIncrement(j), ListBuffer[IR_Statement](
            IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(i - offset_r, j - offset_c)), IR_HighDimAccess(source, IR_ExpressionIndex(i, j)))
          ))
        ))
      case x @ IR_MatrixExpression(_, _, _,_)                      =>
        var decl = IR_VariableDeclaration(x.datatype, "copyTmp_" + tmpCounter, x)
        tmpCounter += 1
        stmts.body += decl
        stmts.body += IR_ForLoop(IR_VariableDeclaration(i, offset_r), IR_Lower(i, n_rows + offset_r), IR_PreIncrement(i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(j, offset_c), IR_Lower(j, offset_c + n_cols), IR_PreIncrement(j), ListBuffer[IR_Statement](
            IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(i - offset_r, j - offset_c)), IR_HighDimAccess(IR_VariableAccess(decl), IR_ExpressionIndex(i, j)))
          ))
        ))
    }
    stmts
  }

  // copy a submatrix of n_rows x n_cols to 'copy' from position 'offset_r', 'offset_c' in 'source' with size 'sourcesize'
  def loopCopySubmatrixPointer(source : IR_Expression, sourcesize : IR_Expression, dest : IR_VariableAccess, offset_r : IR_Expression, offset_c : IR_Expression, n_rows : IR_Expression, n_cols : IR_Expression) : IR_Scope = {
    var stmts = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    stmts.body += IR_ForLoop(IR_VariableDeclaration(i, offset_r), IR_Lower(i, n_rows + offset_r), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(j, offset_c), IR_Lower(j, offset_c + n_cols), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_ArrayAccess(dest, (i - offset_r) * n_cols + j - offset_c), IR_ArrayAccess(source, i * sourcesize + j))
      ))
    ))
    stmts
  }

  // write a submatrix 'source' of n_rows x n_cols to 'destination' at position 'offset_r', 'offset_c'
  def loopSetSubmatrixMat(source : IR_Expression, destination : IR_Expression, rows_source : IR_Expression, cols_source : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression) : IR_Scope = {
    if (!isScalar(offset_r) || !isScalar(offset_c))
      Logger.error("offsets of wrong type: " + offset_c + offset_r + ", expected scalar variable or constant!")
    var stmts = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    stmts.body += IR_ForLoop(IR_VariableDeclaration(i, offset_r), IR_Lower(i, rows_source + offset_r), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(j, offset_c), IR_Lower(j, offset_c + cols_source), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(destination, IR_ExpressionIndex(i, j)), IR_HighDimAccess(source, IR_ExpressionIndex(i - offset_r, j - offset_c)))
      ))
    ))
    stmts
  }

  // write a submatrix 'source' of n_rows x n_cols to 'destination' at position 'offset_r', 'offset_c'
  def loopSetSubmatrixMatPointer(source : IR_VariableAccess, destination : IR_VariableAccess, destsize : IR_Expression, rows_source : IR_Expression, cols_source : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression) : IR_Scope = {
    if (!isScalar(offset_r) || !isScalar(offset_c))
      Logger.error("offsets of wrong type: " + offset_c + offset_r + ", expected scalar variable or constant!")
    var stmts = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    stmts.body += IR_ForLoop(IR_VariableDeclaration(i, offset_r), IR_Lower(i, rows_source + offset_r), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(j, offset_c), IR_Lower(j, offset_c + cols_source), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_ArrayAccess(destination, i * destsize + j), IR_ArrayAccess(source, (i - offset_r) * cols_source + j - offset_c))
      ))
    ))
    stmts
  }

  // write 'newVal' to all positions in 'n_rows' x 'n_cols' at position 'offset_r', 'offset_c' in 'matrix'
  def loopSetSubmatrixSc(matrix : IR_Expression, offsetRows : IR_Expression, offsetCols : IR_Expression, nRows : IR_Expression, nCols : IR_Expression, newValue : IR_Expression) : IR_Scope = {
    var stmts = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    stmts.body += IR_ForLoop(IR_VariableDeclaration(i, offsetRows), IR_Lower(i, nRows + offsetRows), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(j, offsetCols), IR_Lower(j, nCols + offsetCols), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(matrix, IR_ExpressionIndex(i, j)), newValue)
      ))
    ))
    stmts
  }

  // generate determinant calculation if 'in' is lu decomposed
  def determinantLargeMatrix(in : IR_Access, P : IR_VariableAccess, out : IR_Access) : IR_Scope = {
    var func = IR_Scope(Nil)
    var det = IR_VariableAccess("det", IR_DoubleDatatype)
    var N = IR_CompiletimeMatOps.getSize(in)._1
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(det, IR_HighDimAccess(in, IR_ExpressionIndex(0, 0)))
    func.body += IR_ForLoop(IR_VariableDeclaration(i, 1), IR_Lower(i, N), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_Assignment(det, IR_Multiplication(det, IR_HighDimAccess(in, IR_ExpressionIndex(i, i))))
    ))
    func.body += IR_IfCondition(IR_EqEq(IR_Modulo(IR_Subtraction(IR_ArrayAccess(P, N), N), 2), IR_IntegerConstant(0)), ListBuffer[IR_Statement](
      IR_Assignment(out, det)
    ), ListBuffer[IR_Statement](
      IR_Assignment(out, IR_Negative(det))
    ))

    func
  }

  // give a algorithm to calculate the determinant of 'in' by using lu decomposition
  def determinant(in : IR_Access, out : IR_Access) : IR_Scope = {
    var func = IR_Scope(Nil)
    var size = IR_CompiletimeMatOps.getSize(in)
    if (size._1 != size._2)
      Logger.error("determinants of nonquadratic matrices not supported")
    var N = size._1
    var P = IR_VariableAccess("P", IR_ArrayDatatype(IR_IntegerDatatype, N + 1))
    var zeroOffset = IR_VariableAccess("zeroOffset", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(P)
    func.body += IR_VariableDeclaration(zeroOffset, IR_IntegerConstant(0))
    var inplace = Knowledge.experimental_inplaceDeterminant
    if (!inplace) {
      var inCopy = IR_VariableAccess("inCopy", IR_MatrixDatatype(in.datatype.resolveBaseDatatype, N, N))
      func.body += IR_VariableDeclaration(inCopy)
      func.body += IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy", IR_UnitDatatype), ListBuffer[IR_Expression](IR_AddressOf(inCopy), IR_AddressOf(in), IR_SizeOf(in.datatype.resolveBaseDatatype) * N * N))
      func.body ++= IR_GenerateRuntimeInversion.localLUDecomp(inCopy, P, N, zeroOffset, zeroOffset)
      func.body += determinantLargeMatrix(inCopy, P, out)
    }
    else {
      func.body ++= IR_GenerateRuntimeInversion.localLUDecomp(in, P, N, zeroOffset, zeroOffset)
      func.body += determinantLargeMatrix(in, P, out)
    }
    func
  }

  def pivotCheck(pivots : IR_MatrixExpression) : ListBuffer[IR_Statement] = {
    var stmts = ListBuffer[IR_Statement]()
    var pivAcc = IR_VariableAccess("pivots_" + pivotVarCounter, pivots.datatype)
    pivotVarCounter += 1
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var cout = IR_VariableAccess("std::cout", IR_StringDatatype)
    stmts += IR_VariableDeclaration(pivAcc, pivots)
    stmts += IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, pivots.columns), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_IfCondition(IR_Lower(IR_HighDimAccess(pivAcc, IR_ExpressionIndex(0,i)), Knowledge.experimental_CTPivotTolerance),ListBuffer[IR_Statement](IR_Print(cout,ListBuffer[IR_Expression](IR_StringConstant("Compiletime inversion may have become unstable, switch to runtime inversion!"))), IR_Return(-1)))
    ))
   stmts
  }
}

object IR_GenerateRuntimeInversion {
  val pointerArithmetic = "pointerArithmetic"

  // generate code for direct inversion of small matrices
  def smallMatrixInversionAtSubMatrix(in : IR_Access, blocksize : Int, offsetRows : IR_Expression, offsetCols : IR_Expression, out : IR_Access) : IR_Scope = {
    var debug = false

    var stmts = ListBuffer[IR_Statement]()
    blocksize match {
      case 1 =>
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(0, 0)), IR_Division(IR_RealConstant(1), IR_CompiletimeMatOps.getElem(in, 0, 0)))

      case 2 =>
        val a = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 0, offsetCols + 0))
        val b = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 0, offsetCols + 1))
        val c = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 1, offsetCols + 0))
        val d = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 1, offsetCols + 1))
        val det = IR_VariableAccess("det", IR_DoubleDatatype)
        stmts += IR_VariableDeclaration(det, IR_Division(IR_RealConstant(1.0), (a * d) - (b * c)))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 0, offsetCols + 0)), det * Duplicate(d))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 0, offsetCols + 1)), det * Duplicate(b) * IR_IntegerConstant(-1))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 1, offsetCols + 0)), det * Duplicate(c) * IR_IntegerConstant(-1))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 1, offsetCols + 1)), det * Duplicate(a))

      case 3 =>
        val a = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 0, offsetCols + 0))
        val b = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 0, offsetCols + 1))
        val c = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 0, offsetCols + 2))
        val d = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 1, offsetCols + 0))
        val e = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 1, offsetCols + 1))
        val f = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 1, offsetCols + 2))
        val g = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 2, offsetCols + 0))
        val h = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 2, offsetCols + 1))
        val i = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 2, offsetCols + 2))
        val A = Duplicate(e) * Duplicate(i) - Duplicate(f) * Duplicate(h)
        val B = IR_IntegerConstant(-1) * (Duplicate(d) * Duplicate(i) - Duplicate(f) * Duplicate(g))
        val C = Duplicate(d) * Duplicate(h) - Duplicate(e) * Duplicate(g)
        val D = IR_IntegerConstant(-1) * (Duplicate(b) * Duplicate(i) - Duplicate(c) * Duplicate(h))
        val E = Duplicate(a) * Duplicate(i) - Duplicate(c) * Duplicate(g)
        val F = IR_IntegerConstant(-1) * (Duplicate(a) * Duplicate(h) - Duplicate(b) * Duplicate(g))
        val G = Duplicate(b) * Duplicate(f) - Duplicate(c) * Duplicate(e)
        val H = IR_IntegerConstant(-1) * (Duplicate(a) * Duplicate(f) - Duplicate(c) * Duplicate(d))
        val I = Duplicate(a) * Duplicate(e) - Duplicate(b) * Duplicate(d)
        val det = IR_VariableAccess("det", IR_DoubleDatatype)
        stmts += IR_VariableDeclaration(det, Duplicate(a) * Duplicate(A) + Duplicate(b) * Duplicate(B) + Duplicate(c) * Duplicate(C))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows, offsetCols)), Duplicate(A) / det)
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows, offsetCols + 1)), Duplicate(D) / det)
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows, offsetCols + 2)), Duplicate(G) / det)
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 1, offsetCols)), Duplicate(B) / det)
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 1, offsetCols + 1)), Duplicate(E) / det)
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 1, offsetCols + 2)), Duplicate(H) / det)
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 2, offsetCols)), Duplicate(C) / det)
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 2, offsetCols + 1)), Duplicate(F) / det)
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 2, offsetCols + 2)), Duplicate(I) / det)
    }
    IR_Scope(stmts)
  }

  // give a invert algorithm for diagonal matrices
  def diagonalInlined(in : IR_Access, out : IR_Access) : IR_Scope = {
    var debug = false
    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    val N = inDt.sizeM
    val inner = inDt.resolveBaseDatatype
    var func = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)

    // inversion loop
    func.body += IR_VariableDeclaration(i)
    func.body += IR_Assignment(i, IR_IntegerConstant(0))
    func.body += IR_WhileLoop(IR_Lower(i, IR_IntegerConstant(N)), ListBuffer[IR_Statement](
      IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(IR_Addition(IR_Multiplication(i, N), i))), IR_Division(IR_DoubleConstant(1), IR_HighDimAccess(in, IR_ExpressionIndex(i, i)))),
      IR_Assignment(i, IR_Addition(i, IR_IntegerConstant(1)))
    ))

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

    func
  }

  // generate a LU decomposition for a submatrix at 'offset_r','offset_c' of 'in' inplace
  def localLUDecomp(in : IR_Access, P : IR_VariableAccess, blocksize_asInt : Int, offset_r : IR_Expression, offset_c : IR_Expression) : ListBuffer[IR_Statement] = {

    val Tol = IR_RealConstant(0.000000000000001)
    val baseType = in.datatype.resolveBaseDatatype
    val func = ListBuffer[IR_Statement]()
    val i = IR_VariableAccess("i", IR_IntegerDatatype)
    val j = IR_VariableAccess("j", IR_IntegerDatatype)
    val k = IR_VariableAccess("k", IR_IntegerDatatype)
    val imax = IR_VariableAccess("imax", IR_IntegerDatatype)
    val maxA = IR_VariableAccess("maxA", baseType)
    val absA = IR_VariableAccess("absA", baseType)
    val tmp_row = IR_VariableAccess("tmp_row", IR_ArrayDatatype(baseType, blocksize_asInt))
    val tmp_idx = IR_VariableAccess("tmp_idx", IR_IntegerDatatype)
    var outstream = IR_VariableAccess("std::cout", IR_StringDatatype)

    func += IR_VariableDeclaration(tmp_row)
    func += IR_VariableDeclaration(imax)
    func += IR_VariableDeclaration(maxA)
    func += IR_VariableDeclaration(absA)
    func += IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, blocksize_asInt + 1), IR_ExpressionStatement(IR_PreIncrement(i)), ListBuffer[IR_Statement](
      IR_Assignment(IR_ArrayAccess(P, i), i)
    ))
    func += IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, blocksize_asInt), IR_ExpressionStatement(IR_PreIncrement(i)), ListBuffer[IR_Statement](
      IR_Assignment(maxA, IR_RealConstant(0)),
      IR_Assignment(imax, i),
      IR_ForLoop(IR_VariableDeclaration(k, i), IR_Lower(k, blocksize_asInt), IR_ExpressionStatement(IR_PreIncrement(k)), ListBuffer[IR_Statement](
        IR_Assignment(absA, IR_FunctionCall(IR_ExternalFunctionReference.fabs, ListBuffer[IR_Expression](IR_HighDimAccess(in, IR_ExpressionIndex(k + offset_r, i + offset_c))))),
        IR_IfCondition(IR_Greater(absA, maxA), ListBuffer[IR_Statement](IR_Assignment(maxA, absA), IR_Assignment(imax, k)), ListBuffer[IR_Statement]())
      )),
      //IR_IfCondition(IR_Lower(maxA, Tol), ListBuffer[IR_Statement](IR_Print(outstream, ListBuffer[IR_Expression](IR_StringConstant("[Warning] inverting potentially singular matrix\\n")))), ListBuffer[IR_Statement]()),
      IR_IfCondition(IR_Neq(imax, i), ListBuffer[IR_Statement](
        IR_VariableDeclaration(tmp_idx, IR_ArrayAccess(P, i)),
        IR_Assignment(IR_ArrayAccess(P, i), IR_ArrayAccess(P, imax)),
        IR_Assignment(IR_ArrayAccess(P, imax), tmp_idx),
        //IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_ArrayAccess(tmp_row, 0)), IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, 0 + offset_c))), blocksize_asInt * IR_SizeOf(baseType)))),
        IR_FunctionCall(IR_ExternalFunctionReference("std::copy", IR_UnitDatatype), ListBuffer[IR_Expression](IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, 0 + offset_c))),IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, 0 + offset_c))) + blocksize_asInt, tmp_row)),
        //IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, 0 + offset_c))), IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(imax + offset_r, 0 + offset_c))), blocksize_asInt * IR_SizeOf(baseType)))),
        IR_FunctionCall(IR_ExternalFunctionReference("std::copy", IR_UnitDatatype), ListBuffer[IR_Expression](IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(imax + offset_r, 0 + offset_c))),IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(imax + offset_r, 0 + offset_c))) + blocksize_asInt, IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, 0 + offset_c))))),
        //IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(imax + offset_r, 0 + offset_c))), IR_AddressOf(IR_ArrayAccess(tmp_row, 0)), blocksize_asInt * IR_SizeOf(baseType)))),
        IR_FunctionCall(IR_ExternalFunctionReference("std::copy", IR_UnitDatatype), ListBuffer[IR_Expression](IR_AddressOf(IR_ArrayAccess(tmp_row, 0)),IR_AddressOf(IR_ArrayAccess(tmp_row, 0)) + blocksize_asInt,IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(imax + offset_r, 0 + offset_c))))),

        IR_PostIncrement(IR_ArrayAccess(P, blocksize_asInt))
      )),
      IR_ForLoop(IR_VariableDeclaration(j, i + 1), IR_Lower(j, blocksize_asInt), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, i + offset_c)), IR_Division(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, i + offset_c)), IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, i + offset_c)))),
        IR_ForLoop(IR_VariableDeclaration(k, i + 1), IR_Lower(k, blocksize_asInt), IR_PreIncrement(k), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, k + offset_c)), IR_Subtraction(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, k + offset_c)), IR_Multiplication(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, i + offset_c)), IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, k + offset_c)))))
        ))
      ))
    ))
    func
  }

  // generate an inverted matrix for a submatrix at 'offset_r','offset_c' if submatrix('in') is LU decomposed
  def localLUDecomposedInversion(in : IR_Access, P : IR_VariableAccess, blocksize : Int, offset_r : IR_Expression, offset_c : IR_Expression, out : IR_Access) : ListBuffer[IR_Statement] = {
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    var k = IR_VariableAccess("k", IR_IntegerDatatype)
    var func = ListBuffer[IR_Statement]()
    func += IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, blocksize), IR_PreIncrement(j), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, blocksize), IR_PreIncrement(i), ListBuffer[IR_Statement](
        IR_IfCondition(IR_EqEq(IR_ArrayAccess(P, i), j), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), 1.0)
        ), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), 0.0)
        )),
        IR_ForLoop(IR_VariableDeclaration(k, 0), IR_Lower(k, i), IR_PreIncrement(k), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Subtraction(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Multiplication(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, k + offset_c)), IR_HighDimAccess(out, IR_ExpressionIndex(k + offset_r, j + offset_c)))))
        ))
      )),
      IR_ForLoop(IR_VariableDeclaration(i, blocksize - 1), IR_GreaterEqual(i, 0), IR_PostDecrement(i), ListBuffer[IR_Statement](
        IR_ForLoop(IR_VariableDeclaration(k, i + 1), IR_Lower(k, blocksize), IR_PreIncrement(k), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Subtraction(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Multiplication(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, k + offset_c)), IR_HighDimAccess(out, IR_ExpressionIndex(k + offset_r, j + offset_c)))))
        )),
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Division(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, i + offset_c))))
      ))
    ))
    func
  }

  // combines LU decomposition and inversion of submatrix of 'in' at 'offset_r', 'offset_c' of size 'blocksize'
  def localLUPINV(in : IR_Access, blocksize : Int, offset_r : IR_Expression, offset_c : IR_Expression, out : IR_Access) : IR_Scope = {
    var func = IR_Scope(Nil)
    var P = IR_VariableAccess("P", IR_ArrayDatatype(IR_IntegerDatatype, blocksize + 1))
    var block = IR_VariableAccess("block", IR_IntegerDatatype)
    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    val N = inDt.sizeM
    if (N % blocksize != 0) Logger.error(" Matrices with size not mutliple of blocksize not implemented yet")
    func.body += IR_VariableDeclaration(P)
    if (!Knowledge.experimental_inplaceInversion) {
      // copy input matrix and work on copy
      var inCopy = IR_VariableAccess("inCopy", IR_MatrixDatatype(inDt.resolveBaseDatatype, N, N))
      func.body += IR_VariableDeclaration(inCopy)

      // addition must not be resolved in compiler
      val in_end = IR_Addition(in, N*N)
      in_end.annotate(pointerArithmetic)
      func.body += IR_FunctionCall(IR_ExternalFunctionReference("std::copy", IR_UnitDatatype), ListBuffer[IR_Expression]((in), in_end, inCopy))

      // LU decompose
      func.body ++= localLUDecomp(inCopy, P, blocksize, offset_r, offset_c)
      // invert
      func.body ++= localLUDecomposedInversion(inCopy, P, blocksize, offset_r, offset_c, out)
    }
    else {
      // LU decompose
      func.body ++= localLUDecomp(in, P, blocksize, offset_r, offset_c)
      // invert
      func.body ++= localLUDecomposedInversion(in, P, blocksize, offset_r, offset_c, out)
    }
    func
  }

  // give an invert algorithm for blockdiagonal matrices
  def blockdiagonalInlined(in : IR_Access, blocksize : Int, out : IR_Access) : IR_Scope = {
    var debug = false
    var func = IR_Scope(Nil)
    var block = IR_VariableAccess("block", IR_IntegerDatatype)
    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    val N = inDt.sizeM
    if (N % blocksize != 0) Logger.error(s"Matrices with size not mutliple of blocksize not implemented yet, size: ${ N }, blocksize: ${ blocksize }")
    if (blocksize < 4) {
      func.body += IR_ForLoop(IR_VariableDeclaration(block, 0), IR_Lower(block, N), IR_Assignment(block, IR_Addition(block, blocksize)), ListBuffer[IR_Statement](
      ) += smallMatrixInversionAtSubMatrix(in, blocksize, block, block, out))
    }
    else {
      func.body += IR_ForLoop(IR_VariableDeclaration(block, 0), IR_Lower(block, N), IR_Assignment(block, IR_Addition(block, blocksize)), ListBuffer[IR_Statement](
      ) += localLUPINV(in, blocksize, block, block, out))
    }

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

    func
  }

  /* give an invert algorithm using the schur complement

    -1             -1
   M  =  (  A  B  )      =    ( A_inv + A_inv*B*S_inv*C*A_inv -A_inv*B*S_inv  )
         (  C  D  )           (           -S_inv*C*A_inv           S_inv      )

             with M of size (n + m) x (n + m) and S = D - C * A_inv * B
 */

  // schur complement inversion generated inlined in scope with stack memory for helper arrayss
  def schurInlined(in : IR_Access, blockSize : Int, structureA : String, blockSizeA : Int, out : IR_Access) : IR_Scope = {
    var debug = false
    var func = IR_Scope(Nil)
    var inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    var baseType = inDt.resolveBaseDatatype
    var n = IR_VariableAccess("n", IR_IntegerDatatype)
    var m = IR_VariableAccess("m", IR_IntegerDatatype)
    var n_asInt = blockSize
    var m_asInt = inDt.sizeM - blockSize
    var A = IR_VariableAccess("A", IR_MatrixDatatype(baseType, n_asInt, n_asInt))
    var A_inv = IR_VariableAccess("A_inv", IR_MatrixDatatype(baseType, n_asInt, n_asInt))
    var B = IR_VariableAccess("B", IR_MatrixDatatype(baseType, n_asInt, m_asInt))
    var C = IR_VariableAccess("C", IR_MatrixDatatype(baseType, m_asInt, n_asInt))
    var D = IR_VariableAccess("D", IR_MatrixDatatype(baseType, m_asInt, m_asInt))
    var S = IR_VariableAccess("S", IR_MatrixDatatype(baseType, m_asInt, m_asInt))
    var S_inv = IR_VariableAccess("S_inv", IR_MatrixDatatype(baseType, m_asInt, m_asInt))
    var CA_inv = IR_VariableAccess("CA_inv", IR_MatrixDatatype(baseType, m_asInt, n_asInt))
    var CA_invB = IR_VariableAccess("CA_invB", IR_MatrixDatatype(baseType, m_asInt, m_asInt))
    var A_invB = IR_VariableAccess("A_invB", IR_MatrixDatatype(baseType, n_asInt, m_asInt))
    var A_invBS_inv = IR_VariableAccess("A_invBS_inv", IR_MatrixDatatype(baseType, n_asInt, m_asInt))
    var S_invCA_inv = IR_VariableAccess("S_invCA_inv", IR_MatrixDatatype(baseType, m_asInt, n_asInt))
    var A_invBS_invCA_inv = IR_VariableAccess("A_invBS_invCA_inv", IR_MatrixDatatype(baseType, n_asInt, n_asInt))

    func.body += IR_VariableDeclaration(n, blockSize)
    func.body += IR_VariableDeclaration(m, inDt.sizeM - blockSize)

    // copy A and invert
    //TODO use algorithm that exploits structure -> receive matrix structure information from classifier -> e.g. blockdiagonal
    // blocksize of the diagonal blocks of A if A is a blockdiagonal matrix -> later this information comes from the classifyer?
    func.body += IR_VariableDeclaration(A)
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrix(in, A, 0, 0, n, n)
    func.body += IR_VariableDeclaration(A_inv)
    if (structureA == "blockdiagonal")
      func.body += IR_GenerateRuntimeInversion.blockdiagonalInlined(A, blockSizeA, A_inv)
    else if (structureA == "biagonal")
      func.body += IR_GenerateRuntimeInversion.diagonalInlined(A, A_inv)
    else
      func.body += IR_GenerateRuntimeInversion.localLUPINV(A, n_asInt, 0, 0, A_inv)

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(A_inv)

    // copy B
    func.body += IR_VariableDeclaration(B)
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrix(in, B, 0, n, n, m)

    // copy C
    func.body += IR_VariableDeclaration(C)
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrix(in, C, n, 0, m, n)

    // copy D
    func.body += IR_VariableDeclaration(D)
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrix(in, D, n, n, m, m)

    // calculate S
    func.body += IR_VariableDeclaration(S)
    func.body += IR_VariableDeclaration(CA_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(C, A_inv, CA_inv, m, n, n, 0, 0)
    func.body += IR_VariableDeclaration(CA_invB)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(CA_inv, B, CA_invB, m, m, n, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.subAtSubmatrix(D, CA_invB, S, m, m, m, 0, 0)

    // calculate S_inv
    func.body += IR_VariableDeclaration(S_inv)
    func.body += IR_GenerateRuntimeInversion.inverse(S, S_inv, IR_MatShape("filled"))

    // calculate upper right result block
    func.body += IR_VariableDeclaration(A_invB)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_inv, B, A_invB, n, m, n, 0, 0)
    func.body += IR_VariableDeclaration(A_invBS_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_invB, S_inv, A_invBS_inv, n, m, m, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(A_invBS_inv, out, n + m, n, m, 0, n_asInt)

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(A_invBS_inv)

    // insert lower right result block
    func.body += IR_GenerateBasicMatrixOperations.loopSetSubmatrixMat(S_inv, out, m, m, n, n)
    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(S_inv)

    // calculate lower left result block
    func.body += IR_VariableDeclaration(S_invCA_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(S_inv, CA_inv, S_invCA_inv, m, n, m, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(S_invCA_inv, out, m + n, m, n, n_asInt, 0)

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(S_invCA_inv)

    // calculate upper left result block
    func.body += IR_VariableDeclaration(A_invBS_invCA_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_invB, S_invCA_inv, A_invBS_invCA_inv, n, n, m, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.addAtSubmatrix(A_inv, A_invBS_invCA_inv, out, n + m, n, n, 0, 0)

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(A_invBS_invCA_inv)

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

    func
  }

  // old inverse runtime
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
    if(debug) {
      func.body ++= printMatrix(in)
    }
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
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[WARNING] Inverting potentially singular matrix, ref\\n"))) +:
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

  // head function that branches to specific inversions
  def inverse(in : IR_Access, out : IR_Access, msi : IR_MatShape) : IR_Scope = {
    val matrixShape = msi.shape
    val insize = IR_CompiletimeMatOps.getSize(in)
    val outsize = IR_CompiletimeMatOps.getSize(out)
    if (insize._1 != insize._2)
      Logger.error("inversion of matrices of size " + insize._1 + "," + insize._2 + " not supported")
    if (insize != outsize)
      Logger.error("matrix sizes of in and out do not match: " + insize + " vs " + outsize)

    if(Knowledge.experimental_matrixDebugConfig)
      Logger.warn("inverting at runtime with shape=" + matrixShape)

    matrixShape match {
      case "filled"        =>

        var stmts = ListBuffer[IR_Statement]()

        val N = insize._1
        if (insize._1 < 4) {
          stmts ++= smallMatrixInversionAtSubMatrix(in, N, 0, 0, out).body
        } else {


          stmts ++= localLUPINV(in, N, 0,0, out ).body
          //stmts += runtimeInverse(in,out)
        }
        IR_Scope(stmts)
      case "diagonal"      => diagonalInlined(in, out)
      case "blockdiagonal" =>
        val blocksize : Int = msi.size("block")
         blockdiagonalInlined(in, blocksize, out)
      case "schur"         =>
        val blocksize : Int = msi.size("block")
        val matrixStructureA : String = msi.shape("A")
        val blocksizeA : Int = if(matrixStructureA == "blockdiagonal") msi.size("Ablock") else -1
          schurInlined(in, blocksize, matrixStructureA, blocksizeA, out)
      case _               => Logger.error("runtime inversion: unknown runtimeInverse resolve: " + matrixShape)
    }
  }

  def inverseBranchAtRuntime(inMatrix : IR_Access, destname : String, dest : IR_Access) : IR_Scope = {
    var timing = false
    var newstmts = ListBuffer[IR_Statement]()
    var insize = IR_CompiletimeMatOps.getSize(inMatrix)
    var structure = IR_VariableAccess(s"${ destname }_structure", IR_StringDatatype)
    var structure_A = IR_VariableAccess(s"${ destname }_structure_A", IR_StringDatatype)
    var blocksize = IR_VariableAccess(s"${ destname }_blocksize", IR_IntegerDatatype)
    var blocksize_A = IR_VariableAccess(s"${ destname }_blocksize_A", IR_IntegerDatatype)
    var offsetIsZero = IR_VariableAccess("zero", IR_IntegerDatatype)
    var rows = IR_VariableAccess("rows", IR_IntegerDatatype)
    var pstream = IR_VariableAccess("std::cout", IR_StringDatatype)
    Logger.warn("determining structure at runtimes")
    newstmts += IR_VariableDeclaration(structure, IR_StringConstant("Filled"))
    newstmts += IR_VariableDeclaration(structure_A, IR_StringConstant("_"))
    newstmts += IR_VariableDeclaration(blocksize, IR_IntegerConstant(0))
    newstmts += IR_VariableDeclaration(blocksize_A, IR_IntegerConstant(0))
    newstmts += IR_FunctionCall(IR_PlainInternalFunctionReference("isOfStructure", IR_UnitDatatype), ListBuffer[IR_Expression](inMatrix, IR_IntegerConstant(insize._1), structure, blocksize, structure_A, blocksize_A))
    if (!timing)
      newstmts += IR_Print(pstream, IR_StringConstant("[INFO] inverting with the following configuration: Runtime, "), structure, IR_StringConstant(", "), blocksize, IR_StringConstant(", "), structure_A, IR_StringConstant(", "), blocksize_A, IR_StringConstant("\\n"))
    newstmts += IR_IfCondition(IR_EqEq(structure, IR_StringConstant("Filled")), ListBuffer[IR_Statement](
      IR_FunctionCall(IR_PlainInternalFunctionReference("inv_filled", IR_UnitDatatype), ListBuffer[IR_Expression](inMatrix, IR_IntegerConstant(insize._1), IR_IntegerConstant(insize._1), 0, 0, dest))
    ), ListBuffer[IR_Statement](
      IR_IfCondition(IR_EqEq(structure, IR_StringConstant("Diagonal")), ListBuffer[IR_Statement](
        IR_FunctionCall(IR_PlainInternalFunctionReference("inv_diagonal", IR_UnitDatatype), ListBuffer[IR_Expression](inMatrix, IR_IntegerConstant(insize._1), dest))
      )
        , ListBuffer[IR_Statement](
          IR_IfCondition(IR_EqEq(structure, IR_StringConstant("Blockdiagonal")), ListBuffer[IR_Statement](
            IR_FunctionCall(IR_PlainInternalFunctionReference("inv_blockdiagonal", IR_UnitDatatype), ListBuffer[IR_Expression](inMatrix, IR_IntegerConstant(insize._1), blocksize, dest))
          ), ListBuffer[IR_Statement](
            IR_IfCondition(IR_EqEq(structure, IR_StringConstant("Schur")), ListBuffer[IR_Statement](
              IR_FunctionCall(IR_PlainInternalFunctionReference("inv_schur", IR_UnitDatatype), ListBuffer[IR_Expression](inMatrix, IR_IntegerConstant(insize._1), blocksize, structure_A, blocksize_A, dest))
            ))
          ))
        ))
    ))
    if (!IR_UtilFunctions.get.functions.exists(f => f.name == "smallMatrixInversion")) {
      IR_UtilFunctions.get += IR_GenerateRuntimeInversion.smallMatrixInversionAsFunction()
    }
    if (!IR_UtilFunctions.get.functions.exists(f => f.name == "isOfStructure")) {
      IR_UtilFunctions.get += IR_ClassifyMatShape.isOfShapeRuntime()
    }
    if (!IR_UtilFunctions.get.functions.exists(f => f.name == "inv_schur")) {
      IR_UtilFunctions.get += IR_GenerateRuntimeInversion.schurAsFunction()
    }
    if (!IR_UtilFunctions.get.functions.exists(f => f.name == "inv_blockdiagonal")) {
      IR_UtilFunctions.get += IR_GenerateRuntimeInversion.blockdiagonalAsFunction()
    }
    if (!IR_UtilFunctions.get.functions.exists(f => f.name == "inv_diagonal")) {
      IR_UtilFunctions.get += IR_GenerateRuntimeInversion.diagonalAsFunction()
    }
    if (!IR_UtilFunctions.get.functions.exists(f => f.name == "inv_filled")) {
      IR_UtilFunctions.get += IR_GenerateRuntimeInversion.LUInversionAsFunction()
    }
    IR_Scope(newstmts)
  }



  // generate code for direct inversion of small matrices
  def smallMatrixInversionAsFunction() : IR_PlainFunction = {
    var in = IR_VariableAccess("in", IR_PointerDatatype(IR_DoubleDatatype))
    var insize = IR_VariableAccess("insize", IR_IntegerDatatype)
    var offset_r = IR_VariableAccess("offset_r", IR_IntegerDatatype)
    var offset_c = IR_VariableAccess("offset_c", IR_IntegerDatatype)
    var blocksize = IR_VariableAccess("blocksize", IR_IntegerDatatype)
    var out = IR_VariableAccess("out", IR_PointerDatatype(IR_DoubleDatatype))
    var det = IR_VariableAccess("det", IR_DoubleDatatype)
    var A = IR_VariableAccess("A", IR_DoubleDatatype)
    var B = IR_VariableAccess("B", IR_DoubleDatatype)
    var C = IR_VariableAccess("C", IR_DoubleDatatype)
    var D = IR_VariableAccess("D", IR_DoubleDatatype)
    var E = IR_VariableAccess("E", IR_DoubleDatatype)
    var F = IR_VariableAccess("F", IR_DoubleDatatype)
    var G = IR_VariableAccess("G", IR_DoubleDatatype)
    var H = IR_VariableAccess("H", IR_DoubleDatatype)
    var I = IR_VariableAccess("I", IR_DoubleDatatype)
    var a = IR_VariableAccess("a", IR_DoubleDatatype)
    var b = IR_VariableAccess("b", IR_DoubleDatatype)
    var c = IR_VariableAccess("c", IR_DoubleDatatype)
    var d = IR_VariableAccess("d", IR_DoubleDatatype)
    var e = IR_VariableAccess("e", IR_DoubleDatatype)
    var f = IR_VariableAccess("f", IR_DoubleDatatype)
    var g = IR_VariableAccess("g", IR_DoubleDatatype)
    var h = IR_VariableAccess("h", IR_DoubleDatatype)
    var i = IR_VariableAccess("i", IR_DoubleDatatype)
    var stmts = ListBuffer[IR_Statement]()
    stmts += IR_Switch(blocksize, ListBuffer[IR_Case](
      IR_Case(IR_IntegerConstant(1), ListBuffer[IR_Statement](
        IR_Assignment(IR_ArrayAccess(out, offset_r * insize + offset_c), IR_Division(IR_RealConstant(1), IR_ArrayAccess(in, offset_r * insize + offset_c)))
      )),
      IR_Case(IR_IntegerConstant(2), ListBuffer[IR_Statement](
        IR_VariableDeclaration(det, IR_Division(IR_RealConstant(1.0), (IR_ArrayAccess(in, offset_r * insize + offset_c) * IR_ArrayAccess(in, (offset_r + 1) * insize + 1 + offset_c)) - (IR_ArrayAccess(in, offset_r * insize + offset_c + 1) * IR_ArrayAccess(in, (offset_r + 1) * insize + offset_c)))),
        IR_Assignment(IR_ArrayAccess(out, offset_r * insize + offset_c), det * IR_ArrayAccess(in, (offset_r + 1) * insize + 1 + offset_c)),
        IR_Assignment(IR_ArrayAccess(out, offset_r * insize + offset_c + 1), det * IR_ArrayAccess(in, offset_r * insize + offset_c + 1) * IR_IntegerConstant(-1)),
        IR_Assignment(IR_ArrayAccess(out, (offset_r + 1) * insize + offset_c), det * IR_ArrayAccess(in, (offset_r + 1) * insize + offset_c) * IR_IntegerConstant(-1)),
        IR_Assignment(IR_ArrayAccess(out, (offset_r + 1) * insize + offset_c + 1), det * IR_ArrayAccess(in, offset_r * insize + offset_c))
      )),
      IR_Case(IR_IntegerConstant(3), ListBuffer[IR_Statement](
        IR_VariableDeclaration(a, IR_ArrayAccess(in, offset_r * insize + offset_c)),
        IR_VariableDeclaration(b, IR_ArrayAccess(in, offset_r * insize + offset_c + 1)),
        IR_VariableDeclaration(c, IR_ArrayAccess(in, offset_r * insize + offset_c + 2)),
        IR_VariableDeclaration(d, IR_ArrayAccess(in, (offset_r + 1) * insize + offset_c)),
        IR_VariableDeclaration(e, IR_ArrayAccess(in, (offset_r + 1) * insize + offset_c + 1)),
        IR_VariableDeclaration(f, IR_ArrayAccess(in, (offset_r + 1) * insize + offset_c + 2)),
        IR_VariableDeclaration(g, IR_ArrayAccess(in, (offset_r + 2) * insize + offset_c)),
        IR_VariableDeclaration(h, IR_ArrayAccess(in, (offset_r + 2) * insize + offset_c + 1)),
        IR_VariableDeclaration(i, IR_ArrayAccess(in, (offset_r + 2) * insize + offset_c + 2)),
        IR_VariableDeclaration(A, e * i - f * h),
        IR_VariableDeclaration(B, -1 * (d * i - f * g)),
        IR_VariableDeclaration(C, d * h - e * g),
        IR_VariableDeclaration(D, -1 * (b * i - c * h)),
        IR_VariableDeclaration(E, a * i - c * g),
        IR_VariableDeclaration(F, -1 * (a * h - b * g)),
        IR_VariableDeclaration(G, b * f - c * e),
        IR_VariableDeclaration(H, -1 * (a * f - c * d)),
        IR_VariableDeclaration(I, a * e - b * d),
        IR_VariableDeclaration(det, a * A + b * B + c * C),
        IR_Assignment(IR_ArrayAccess(out, offset_r * insize + offset_c), A / det),
        IR_Assignment(IR_ArrayAccess(out, offset_r * insize + offset_c + 1), D / det),
        IR_Assignment(IR_ArrayAccess(out, offset_r * insize + offset_c + 2), G / det),
        IR_Assignment(IR_ArrayAccess(out, (offset_r + 1) * insize + offset_c), B / det),
        IR_Assignment(IR_ArrayAccess(out, (offset_r + 1) * insize + offset_c + 1), E / det),
        IR_Assignment(IR_ArrayAccess(out, (offset_r + 1) * insize + offset_c + 2), H / det),
        IR_Assignment(IR_ArrayAccess(out, (offset_r + 2) * insize + offset_c), C / det),
        IR_Assignment(IR_ArrayAccess(out, (offset_r + 2) * insize + offset_c + 1), F / det),
        IR_Assignment(IR_ArrayAccess(out, (offset_r + 2) * insize + offset_c + 2), I / det)
      ))
    ))
    IR_PlainFunction("smallMatrixInversion", IR_UnitDatatype, ListBuffer[IR_FunctionArgument](
      IR_FunctionArgument("in", IR_PointerDatatype(IR_DoubleDatatype)),
      IR_FunctionArgument("insize", IR_IntegerDatatype),
      IR_FunctionArgument("blocksize", IR_IntegerDatatype),
      IR_FunctionArgument("offset_r", IR_IntegerDatatype),
      IR_FunctionArgument("offset_c", IR_IntegerDatatype),
      IR_FunctionArgument("out", IR_PointerDatatype(IR_DoubleDatatype))
    ), stmts)
  }
  // give a invert algorithm for diagonal matrices
  def diagonalAsFunction() : IR_PlainFunction = {
    var in = IR_VariableAccess("in", IR_PointerDatatype(IR_DoubleDatatype))
    var insize = IR_VariableAccess("insize", IR_IntegerDatatype)
    var blocksize = IR_VariableAccess("blocksize", IR_IntegerDatatype)
    var out = IR_VariableAccess("out", IR_PointerDatatype(IR_DoubleDatatype))
    var func = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(i)
    func.body += IR_Assignment(i, IR_IntegerConstant(0))
    func.body += IR_WhileLoop(IR_Lower(i, insize), ListBuffer[IR_Statement](
      IR_Assignment(IR_ArrayAccess(out, i * insize + i), IR_Division(IR_DoubleConstant(1), IR_ArrayAccess(in, i * insize + i))),
      IR_Assignment(i, IR_Addition(i, IR_IntegerConstant(1)))
    ))

    var pfunc = IR_PlainFunction("inv_diagonal", IR_UnitDatatype, ListBuffer[IR_FunctionArgument](
      IR_FunctionArgument("in", IR_PointerDatatype(IR_DoubleDatatype)),
      IR_FunctionArgument("insize", IR_IntegerDatatype),
      IR_FunctionArgument("out", IR_PointerDatatype(IR_DoubleDatatype))
    ),
      func.body
    )
    pfunc.allowInlining = false
    pfunc
  }
  // generate a LU decomposition for a submatrix at 'offset_r','offset_c' of 'in' inplace
  def localLUDecompDynMem(in : IR_VariableAccess, insize : IR_Expression, P : IR_VariableAccess, blocksize : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression) : ListBuffer[IR_Statement] = {

    val Tol = IR_RealConstant(0.0001)
    val baseType = in.datatype.resolveBaseDatatype
    val func = ListBuffer[IR_Statement]()
    val i = IR_VariableAccess("i", IR_IntegerDatatype)
    val j = IR_VariableAccess("j", IR_IntegerDatatype)
    val k = IR_VariableAccess("k", IR_IntegerDatatype)
    val imax = IR_VariableAccess("imax", IR_IntegerDatatype)
    val maxA = IR_VariableAccess("maxA", baseType)
    val absA = IR_VariableAccess("absA", baseType)
    val tmp_row = IR_VariableAccess("tmp_row", IR_PointerDatatype(baseType))
    val tmp_idx = IR_VariableAccess("tmp_idx", IR_IntegerDatatype)
    var outstream = IR_VariableAccess("std::cout", IR_StringDatatype)

    func += IR_VariableDeclaration(tmp_row)
    func += IR_ArrayAllocation(tmp_row, IR_DoubleDatatype, blocksize)
    func += IR_VariableDeclaration(imax)
    func += IR_VariableDeclaration(maxA)
    func += IR_VariableDeclaration(absA)
    func += IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, blocksize + 1), IR_ExpressionStatement(IR_PreIncrement(i)), ListBuffer[IR_Statement](
      IR_Assignment(IR_ArrayAccess(P, i), i)
    ))
    func += IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, blocksize), IR_ExpressionStatement(IR_PreIncrement(i)), ListBuffer[IR_Statement](
      IR_Assignment(maxA, IR_RealConstant(0)),
      IR_Assignment(imax, i),
      IR_ForLoop(IR_VariableDeclaration(k, i), IR_Lower(k, blocksize), IR_ExpressionStatement(IR_PreIncrement(k)), ListBuffer[IR_Statement](
        IR_Assignment(absA, IR_FunctionCall(IR_ExternalFunctionReference.fabs, ListBuffer[IR_Expression](IR_ArrayAccess(in, (k + offset_r) * insize + i + offset_c)))),
        IR_IfCondition(IR_Greater(absA, maxA), ListBuffer[IR_Statement](IR_Assignment(maxA, absA), IR_Assignment(imax, k)), ListBuffer[IR_Statement]())
      )),
      IR_IfCondition(IR_Lower(maxA, Tol), ListBuffer[IR_Statement](IR_Print(outstream, ListBuffer[IR_Expression](IR_StringConstant("[Warning] inverting potentially singular matrix\\n"))), IR_Return()), ListBuffer[IR_Statement]()),
      IR_IfCondition(IR_Neq(imax, i), ListBuffer[IR_Statement](
        IR_VariableDeclaration(tmp_idx, IR_ArrayAccess(P, i)),
        IR_Assignment(IR_ArrayAccess(P, i), IR_ArrayAccess(P, imax)),
        IR_Assignment(IR_ArrayAccess(P, imax), tmp_idx),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_ArrayAccess(tmp_row, 0)), IR_AddressOf(IR_ArrayAccess(in, (i + offset_r) * insize + 0 + offset_c)), blocksize * IR_SizeOf(baseType)))),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_ArrayAccess(in, (i + offset_r) * insize + 0 + offset_c)), IR_AddressOf(IR_ArrayAccess(in, (imax + offset_r) * insize + 0 + offset_c)), blocksize * IR_SizeOf(baseType)))),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_ArrayAccess(in, (imax + offset_r) * insize + 0 + offset_c)), IR_AddressOf(IR_ArrayAccess(tmp_row, 0)), blocksize * IR_SizeOf(baseType)))),
        IR_PostIncrement(IR_ArrayAccess(P, blocksize))
      )),
      IR_ForLoop(IR_VariableDeclaration(j, i + 1), IR_Lower(j, blocksize), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_ArrayAccess(in, (j + offset_r) * insize + i + offset_c), IR_Division(IR_ArrayAccess(in, (j + offset_r) * insize + i + offset_c), IR_ArrayAccess(in, (i + offset_r) * insize + i + offset_c))),
        IR_ForLoop(IR_VariableDeclaration(k, i + 1), IR_Lower(k, blocksize), IR_PreIncrement(k), ListBuffer[IR_Statement](
          IR_Assignment(IR_ArrayAccess(in, (j + offset_r) * insize + k + offset_c), IR_Subtraction(IR_ArrayAccess(in, (j + offset_r) * insize + k + offset_c), IR_Multiplication(IR_ArrayAccess(in, (j + offset_r) * insize + i + offset_c), IR_ArrayAccess(in, (i + offset_r) * insize + k + offset_c))))
        ))
      ))
    ))
    func += IR_ArrayFree(tmp_row)
    func
  }
  //TODO integrate in normal method
  // generate an inverted matrix for a submatrix at 'offset_r','offset_c' if submatrix('in') is LU decomposed
  def localLUDecomposedInversionDynMem(in : IR_VariableAccess, insize : IR_Expression, P : IR_VariableAccess, blocksize : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression, out : IR_VariableAccess) : ListBuffer[IR_Statement] = {
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    var k = IR_VariableAccess("k", IR_IntegerDatatype)
    var func = ListBuffer[IR_Statement]()
    func += IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, blocksize), IR_PreIncrement(j), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, blocksize), IR_PreIncrement(i), ListBuffer[IR_Statement](
        IR_IfCondition(IR_EqEq(IR_ArrayAccess(P, i), j), ListBuffer[IR_Statement](
          IR_Assignment(IR_ArrayAccess(out, (i + offset_r) * insize + j + offset_c), 1.0)
        ), ListBuffer[IR_Statement](
          IR_Assignment(IR_ArrayAccess(out, (i + offset_r) * insize + j + offset_c), 0.0)
        )),
        IR_ForLoop(IR_VariableDeclaration(k, 0), IR_Lower(k, i), IR_PreIncrement(k), ListBuffer[IR_Statement](
          IR_Assignment(IR_ArrayAccess(out, (i + offset_r) * insize + j + offset_c), IR_Subtraction(IR_ArrayAccess(out, (i + offset_r) * insize + j + offset_c), IR_Multiplication(IR_ArrayAccess(in, (i + offset_r) * insize + k + offset_c), IR_ArrayAccess(out, (k + offset_r) * insize + j + offset_c))))
        ))
      )),
      IR_ForLoop(IR_VariableDeclaration(i, blocksize - 1), IR_GreaterEqual(i, 0), IR_PostDecrement(i), ListBuffer[IR_Statement](
        IR_ForLoop(IR_VariableDeclaration(k, i + 1), IR_Lower(k, blocksize), IR_PreIncrement(k), ListBuffer[IR_Statement](
          IR_Assignment(IR_ArrayAccess(out, (i + offset_r) * insize + j + offset_c), IR_Subtraction(IR_ArrayAccess(out, (i + offset_r) * insize + j + offset_c), IR_Multiplication(IR_ArrayAccess(in, (i + offset_r) * insize + k + offset_c), IR_ArrayAccess(out, (k + offset_r) * insize + j + offset_c))))
        )),
        IR_Assignment(IR_ArrayAccess(out, (i + offset_r) * insize + j + offset_c), IR_Division(IR_ArrayAccess(out, (i + offset_r) * insize + j + offset_c), IR_ArrayAccess(in, (i + offset_r) * insize + i + offset_c)))
      ))
    ))
    func
  }
  // schur complement inversion as function call and with heap memory for helper arrays
  // -> for use in runtime classification: helper array sizes not known
  def schurAsFunction() : IR_PlainFunction = {
    var debug = false
    var func = IR_Scope(Nil)
    var in = IR_VariableAccess("in", IR_PointerDatatype(IR_DoubleDatatype))
    var insize = IR_VariableAccess("insize", IR_IntegerDatatype)
    var blockSize = IR_VariableAccess("blocksize", IR_IntegerDatatype)
    var structureA = IR_VariableAccess("structure_A", IR_StringDatatype)
    var blockSizeA = IR_VariableAccess("blocksize_A", IR_IntegerDatatype)
    var out = IR_VariableAccess("out", IR_PointerDatatype(IR_DoubleDatatype))
    var baseType = in.datatype.resolveBaseDatatype
    var offset_r = IR_VariableAccess("offset_r", IR_IntegerDatatype)
    var offset_c = IR_VariableAccess("offset_c", IR_IntegerDatatype)
    var n = IR_VariableAccess("n", IR_IntegerDatatype)
    var m = IR_VariableAccess("m", IR_IntegerDatatype)

    var A = IR_VariableAccess("A", IR_PointerDatatype(baseType))
    var A_inv = IR_VariableAccess("A_inv", IR_PointerDatatype(baseType))
    var B = IR_VariableAccess("B", IR_PointerDatatype(baseType))
    var C = IR_VariableAccess("C", IR_PointerDatatype(baseType))
    var D = IR_VariableAccess("D", IR_PointerDatatype(baseType))
    var S = IR_VariableAccess("S", IR_PointerDatatype(baseType))
    var S_inv = IR_VariableAccess("S_inv", IR_PointerDatatype(baseType))
    var CA_inv = IR_VariableAccess("CA_inv", IR_PointerDatatype(baseType))
    var CA_invB = IR_VariableAccess("CA_invB", IR_PointerDatatype(baseType))
    var A_invB = IR_VariableAccess("A_invB", IR_PointerDatatype(baseType))
    var A_invBS_inv = IR_VariableAccess("A_invBS_inv", IR_PointerDatatype(baseType))
    var S_invCA_inv = IR_VariableAccess("S_invCA_inv", IR_PointerDatatype(baseType))
    var A_invBS_invCA_inv = IR_VariableAccess("A_invBS_invCA_inv", IR_PointerDatatype(baseType))

    func.body += IR_VariableDeclaration(A)
    func.body += IR_VariableDeclaration(A_inv)
    func.body += IR_VariableDeclaration(B)
    func.body += IR_VariableDeclaration(C)
    func.body += IR_VariableDeclaration(D)
    func.body += IR_VariableDeclaration(S)
    func.body += IR_VariableDeclaration(S_inv)
    func.body += IR_VariableDeclaration(CA_inv)
    func.body += IR_VariableDeclaration(CA_invB)
    func.body += IR_VariableDeclaration(A_invB)
    func.body += IR_VariableDeclaration(A_invBS_inv)
    func.body += IR_VariableDeclaration(S_invCA_inv)
    func.body += IR_VariableDeclaration(A_invBS_invCA_inv)

    func.body += IR_VariableDeclaration(offset_r)
    func.body += IR_VariableDeclaration(offset_c)
    func.body += IR_Assignment(offset_r, 0)
    func.body += IR_Assignment(offset_c, 0)
    func.body += IR_VariableDeclaration(n)
    func.body += IR_Assignment(n, blockSize)
    func.body += IR_VariableDeclaration(m)
    func.body += IR_Assignment(m, insize - blockSize)

    func.body += IR_IfCondition(IR_EqEq(n, IR_IntegerConstant(1)), ListBuffer[IR_Statement](
      IR_ScalarAllocation(A, baseType),
      IR_ScalarAllocation(A_inv, baseType),
      IR_ScalarAllocation(A_invBS_invCA_inv, baseType)
    ), ListBuffer[IR_Statement](
      IR_ArrayAllocation(A, baseType, n * n),
      IR_ArrayAllocation(A_inv, baseType, n * n),
      IR_ArrayAllocation(A_invBS_invCA_inv, baseType, n * n)
    ))
    func.body += IR_ArrayAllocation(B, baseType, n * m)
    func.body += IR_ArrayAllocation(C, baseType, m * n)
    func.body += IR_IfCondition(IR_EqEq(m, IR_IntegerConstant(1)), ListBuffer[IR_Statement](
      IR_ScalarAllocation(D, baseType),
      IR_ScalarAllocation(S, baseType),
      IR_ScalarAllocation(S_inv, baseType),
      IR_ScalarAllocation(CA_invB, baseType)
    ), ListBuffer[IR_Statement](
      IR_ArrayAllocation(D, baseType, m * m),
      IR_ArrayAllocation(S, baseType, m * m),
      IR_ArrayAllocation(S_inv, baseType, m * m),
      IR_ArrayAllocation(CA_invB, baseType, m * m)
    ))
    func.body += IR_ArrayAllocation(CA_inv, baseType, m * n)
    func.body += IR_ArrayAllocation(A_invB, baseType, n * m)
    func.body += IR_ArrayAllocation(A_invBS_inv, baseType, n * m)
    func.body += IR_ArrayAllocation(S_invCA_inv, baseType, m * n)

    // copy A and invert
    //TODO use algorithm that exploits structure -> receive matrix structure information from classifier -> e.g. blockdiagonal
    // blocksize of the diagonal blocks of A if A is a blockdiagonal matrix -> later this information comes from the classifyer?
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrixPointer(in, insize, A, 0, 0, n, n)
    func.body += IR_IfCondition(IR_EqEq(structureA, IR_StringConstant("Blockdiagonal")), ListBuffer[IR_Statement](
      IR_FunctionCall(IR_PlainInternalFunctionReference("inv_blockdiagonal", IR_UnitDatatype), ListBuffer[IR_Expression](A, n, blockSizeA, A_inv))
    ), ListBuffer[IR_Statement](
      IR_IfCondition(IR_EqEq(structureA, IR_StringConstant("Diagonal")), ListBuffer[IR_Statement](
        IR_FunctionCall(IR_PlainInternalFunctionReference("inv_diagonal", IR_UnitDatatype), ListBuffer[IR_Expression](A, n, A_inv))
      ), ListBuffer[IR_Statement](
        IR_FunctionCall(IR_PlainInternalFunctionReference("inv_filled", IR_UnitDatatype), ListBuffer[IR_Expression](A, n, n, 0, 0, A_inv))

        /*
                IR_IfCondition(IR_Greater(n,3),ListBuffer[IR_Statement](

                IR_FunctionCall(IR_PlainInternalFunctionReference("inv_filled", IR_UnitDatatype), ListBuffer[IR_Expression](A, n, n, 0, 0, A_inv))
                ),ListBuffer[IR_Statement](
                  IR_FunctionCall(IR_PlainInternalFunctionReference("smallMatrixInversion",IR_UnitDatatype), ListBuffer[IR_Expression](A,n,n,0,0,A_inv))
                ))

         */
      ))
    ))

    if (debug)
      func.body += IR_GenerateBasicMatrixOperations.printMatrixPointer(A_inv, n, n)

    // copy B
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrixPointer(in, insize, B, 0, n, n, m)

    // copy C
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrixPointer(in, insize, C, n, 0, m, n)

    // copy D
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrixPointer(in, insize, D, n, n, m, m)

    // calculate S
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(C, A_inv, CA_inv, m, n, n, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(CA_inv, B, CA_invB, m, m, n, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.subAtSubmatrix(D, CA_invB, S, m, m, m, 0, 0)

    // calculate S_inv
    //func.body += IR_GenerateRuntimeInversion.localLUInversionAsFunction(S, n, n, IR_IntegerConstant(0), IR_IntegerConstant(0), S_inv)
    func.body += IR_IfCondition(IR_Lower(m, 4), ListBuffer[IR_Statement](
      IR_FunctionCall(IR_PlainInternalFunctionReference("smallMatrixInversion", IR_UnitDatatype), ListBuffer[IR_Expression](S, m, m, 0, 0, S_inv))
    ), ListBuffer[IR_Statement](
      IR_FunctionCall(IR_PlainInternalFunctionReference("inv_filled", IR_UnitDatatype), ListBuffer[IR_Expression](S, m, m, 0, 0, S_inv))
    ))

    // calculate upper right result block
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_inv, B, A_invB, n, m, n, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_invB, S_inv, A_invBS_inv, n, m, m, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(A_invBS_inv, out, insize, n, m, 0, n)

    if (debug)
      func.body += IR_GenerateBasicMatrixOperations.printMatrixPointer(A_invBS_inv, n, m)

    // insert lower right result block
    func.body += IR_GenerateBasicMatrixOperations.loopSetSubmatrixMatPointer(S_inv, out, insize, m, m, n, n)

    if (debug)
      func.body += IR_GenerateBasicMatrixOperations.printMatrixPointer(S_inv, m, m)

    // calculate lower left result block
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(S_inv, CA_inv, S_invCA_inv, m, n, m, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(S_invCA_inv, out, insize, m, n, n, 0)

    if (debug)
      func.body += IR_GenerateBasicMatrixOperations.printMatrixPointer(S_invCA_inv, m, n)

    // calculate upper left result block
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_invB, S_invCA_inv, A_invBS_invCA_inv, n, n, m, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.addAtSubmatrix(A_inv, A_invBS_invCA_inv, out, insize, n, n, 0, 0)

    if (debug)
      func.body += IR_GenerateBasicMatrixOperations.printMatrixPointer(A_invBS_invCA_inv, n, n)

    if (debug)
      func.body += IR_GenerateBasicMatrixOperations.printMatrixPointer(out, insize, insize)

    func.body += IR_IfCondition(IR_EqEq(n, IR_IntegerConstant(1)), ListBuffer[IR_Statement](
      IR_ScalarFree(A),
      IR_ScalarFree(A_inv),
      IR_ScalarFree(A_invBS_invCA_inv)
    ), ListBuffer[IR_Statement](
      IR_ArrayFree(A),
      IR_ArrayFree(A_inv),
      IR_ArrayFree(A_invBS_invCA_inv)
    ))
    func.body += IR_ArrayFree(B)
    func.body += IR_ArrayFree(C)
    func.body += IR_IfCondition(IR_EqEq(m, IR_IntegerConstant(1)), ListBuffer[IR_Statement](
      IR_ScalarFree(D),
      IR_ScalarFree(S),
      IR_ScalarFree(S_inv),
      IR_ScalarFree(CA_invB)
    ), ListBuffer[IR_Statement](
      IR_ArrayFree(D),
      IR_ArrayFree(S),
      IR_ArrayFree(S_inv),
      IR_ArrayFree(CA_invB)
    ))
    func.body += IR_ArrayFree(CA_inv)
    func.body += IR_ArrayFree(A_invB)
    func.body += IR_ArrayFree(A_invBS_inv)
    func.body += IR_ArrayFree(S_invCA_inv)

    var pfunc = IR_PlainFunction("inv_schur", IR_UnitDatatype, ListBuffer[IR_FunctionArgument](
      IR_FunctionArgument("in", IR_PointerDatatype(IR_DoubleDatatype)),
      IR_FunctionArgument("insize", IR_IntegerDatatype),
      IR_FunctionArgument("blocksize", IR_IntegerDatatype),
      IR_FunctionArgument("structure_A", IR_StringDatatype),
      IR_FunctionArgument("blocksize_A", IR_IntegerDatatype),
      IR_FunctionArgument("out", IR_PointerDatatype(IR_DoubleDatatype))
    ),
      func.body
    )
    pfunc.allowInlining = false
    pfunc
  }
  // combines LU decomposition and inversion of submatrix of 'in' at 'offset_r', 'offset_c' of size 'blocksize'
  def LUInversionAsFunction() : IR_PlainFunction = {
    var func = IR_Scope(Nil)
    var in = IR_VariableAccess("in", IR_PointerDatatype(IR_DoubleDatatype))
    var insize = IR_VariableAccess("insize", IR_IntegerDatatype)
    var out = IR_VariableAccess("out", IR_PointerDatatype(IR_DoubleDatatype))
    var P = IR_VariableAccess("P", IR_PointerDatatype(IR_IntegerDatatype))
    val baseType = in.datatype.resolveBaseDatatype
    func.body += IR_VariableDeclaration(P)
    func.body += IR_ArrayAllocation(P, IR_IntegerDatatype, insize)
    var inplace = Knowledge.experimental_inplaceInversion
    if (!inplace) {
      var inCopy = IR_VariableAccess("inCopy", IR_PointerDatatype(baseType))
      func.body += IR_VariableDeclaration(inCopy)
      func.body += IR_ArrayAllocation(inCopy, baseType, insize * insize)
      func.body += IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy", IR_UnitDatatype), ListBuffer[IR_Expression](IR_AddressOf(inCopy), IR_AddressOf(in), IR_SizeOf(baseType) * insize * insize))
      func.body ++= localLUDecompDynMem(inCopy, insize, P, insize, IR_IntegerConstant(0), IR_IntegerConstant(0))
      func.body ++= localLUDecomposedInversionDynMem(inCopy, insize, P, insize, IR_IntegerConstant(0), IR_IntegerConstant(0), out)
    }
    else {
      func.body ++= localLUDecompDynMem(in, insize, P, insize, IR_IntegerConstant(0), IR_IntegerConstant(0))
      func.body ++= localLUDecomposedInversionDynMem(in, insize, P, insize, IR_IntegerConstant(0), IR_IntegerConstant(0), out)
    }
    func.body += IR_ArrayFree(P)

    var pfunc = IR_PlainFunction("inv_filled", IR_UnitDatatype, ListBuffer[IR_FunctionArgument](
      IR_FunctionArgument("in", IR_PointerDatatype(IR_DoubleDatatype)),
      IR_FunctionArgument("insize", IR_IntegerDatatype),
      IR_FunctionArgument("out", IR_PointerDatatype(IR_DoubleDatatype))
    ),
      func.body
    )
    pfunc.allowInlining = false
    pfunc
  }
  // give an invert algorithm for blockdiagonal matrices
  def blockdiagonalAsFunction() : IR_PlainFunction = {
    var in = IR_VariableAccess("in", IR_PointerDatatype(IR_DoubleDatatype))
    var insize = IR_VariableAccess("insize", IR_IntegerDatatype)
    var blocksize = IR_VariableAccess("blocksize", IR_IntegerDatatype)
    var out = IR_VariableAccess("out", IR_PointerDatatype(IR_DoubleDatatype))
    var debug = false
    var func = IR_Scope(Nil)
    var block = IR_VariableAccess("block", IR_IntegerDatatype)
    func.body += IR_IfCondition(IR_Greater(blocksize, 3), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(block, 0), IR_Lower(block, insize), IR_Assignment(block, IR_Addition(block, blocksize)), ListBuffer[IR_Statement](
        IR_FunctionCall(IR_PlainInternalFunctionReference("inv_filled", IR_UnitDatatype), ListBuffer[IR_Expression](in, insize, blocksize, block, block, out))
      ))), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(block, 0), IR_Lower(block, insize), IR_Assignment(block, IR_Addition(block, blocksize)), ListBuffer[IR_Statement](
        IR_FunctionCall(IR_PlainInternalFunctionReference("smallMatrixInversion", IR_UnitDatatype), ListBuffer[IR_Expression](in, insize, blocksize, block, block, out))
      )))
    )

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

    var pfunc = IR_PlainFunction("inv_blockdiagonal", IR_UnitDatatype, ListBuffer[IR_FunctionArgument](
      IR_FunctionArgument("in", IR_PointerDatatype(IR_DoubleDatatype)),
      IR_FunctionArgument("insize", IR_IntegerDatatype),
      IR_FunctionArgument("blocksize", IR_IntegerDatatype),
      IR_FunctionArgument("out", IR_PointerDatatype(IR_DoubleDatatype))
    ),
      func
    )
    pfunc.allowInlining = false
    pfunc
  }

}
