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

import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_FloatDatatype
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir._
import exastencils.config._
import exastencils.core._
import exastencils.logger.Logger
import exastencils.util.ir._

// generate simple operations with matrices for execution at runtime for e.g. an inversion at runtime
object IR_GenerateBasicMatrixOperations {
  var tmpCounter = 0

  // generate code to copy a matrix per std::memcpy
  def memcpyMatrix(dest : IR_VariableAccess, src : IR_Expression) : ListBuffer[IR_Statement] = {
    val destSize = IR_BasicMatrixOperations.getSize(dest)
    val srcSize = IR_BasicMatrixOperations.getSize(src)
    if (destSize != srcSize)
      Logger.error("sizes do not match: " + destSize + " vs " + srcSize)
    var srcDt = src.datatype.resolveBaseDatatype
    var destDt = dest.datatype.resolveBaseDatatype
    if (destDt != srcDt && !((destDt == IR_RealDatatype || destDt == IR_DoubleDatatype) && (srcDt == IR_RealDatatype || srcDt == IR_DoubleDatatype)))
      Logger.error("Datatypes do not match: destination datatype=" + destDt + " vs source datatype=" + srcDt)
    var stmts = ListBuffer[IR_Statement]()
    src match {
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        stmts += IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy", IR_UnitDatatype), ListBuffer[IR_Expression](IR_AddressOf(dest), IR_AddressOf(src), IR_SizeOf(dest.datatype.resolveBaseDatatype) * destSize._1 * destSize._2))
      case x @ IR_MatrixExpression(_, _, _)                      =>
        var tmpAccess = IR_VariableAccess("copyTmp_" + tmpCounter, src.datatype)
        tmpCounter += 1
        stmts += IR_VariableDeclaration(tmpAccess, src)
        stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy", IR_UnitDatatype), ListBuffer[IR_Expression](IR_AddressOf(dest), IR_AddressOf(tmpAccess), IR_SizeOf(dest.datatype.resolveBaseDatatype) * destSize._1 * destSize._2)))
      case _                                                     => Logger.error("unexpected datatype: " + src + ", expected matrix expression or access to matrix variable")
    }
  }

  // generate code to print a matrix
  def printMatrix(matrix : IR_VariableAccess) = {
    val stmts = ListBuffer[IR_Statement]()
    matrix.datatype match {
      case dt : IR_MatrixDatatype =>
        for (i <- 0 until dt.sizeM) {
          for (j <- 0 until dt.sizeN) {
            stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, ListBuffer[IR_Expression](IR_StringConstant("%f "), IR_HighDimAccess(matrix, IR_ConstIndex(i, j)))))
          }
          stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("\\n")))
        }
    }
    stmts
  }

  // generate a compare function for two matrices
  def compare(left : IR_Expression, right : IR_Expression, precision : IR_Expression) : IR_Scope = {
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
            IR_IfCondition(IR_Greater(IR_FunctionCall(IR_ExternalFunctionReference.fabs, IR_Subtraction(IR_HighDimAccess(left, IR_ExpressionIndex(_i, _j)), IR_HighDimAccess(right, IR_ExpressionIndex(_i, _j)))), precision), ListBuffer[IR_Statement](
              IR_Print(outstream, ListBuffer[IR_Expression](IR_StringConstant("[Test] comparison failed at "), _i, IR_StringConstant(" "), _j, IR_StringConstant("\\n"), IR_HighDimAccess(left, IR_ExpressionIndex(_i, _j)), IR_StringConstant(" vs "), IR_HighDimAccess(right, IR_ExpressionIndex(_i, _j)), IR_StringConstant("\\n"))),
              IR_Return(IR_IntegerConstant(-1))
            ), ListBuffer[IR_Statement]())
          ))
        ))
      case scalars @ (IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype | IR_FloatDatatype, IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype | IR_FloatDatatype) =>
        func.body += IR_IfCondition(IR_Greater(IR_FunctionCall(IR_ExternalFunctionReference.fabs, IR_Subtraction(left, right)), precision), ListBuffer[IR_Statement](
          IR_Print(outstream, ListBuffer[IR_Expression](IR_StringConstant("[Test] comparison failed: "), left, IR_StringConstant(" vs "), right, IR_StringConstant("\\n")))
        ))
    }
    func
  }

  def mkConstant(dt : IR_Datatype, v : Double) = dt match {
    case IR_RealDatatype    => IR_RealConstant(v)
    case IR_IntegerDatatype => IR_IntegerConstant(v.toInt)
    case _                  => exastencils.logger.Logger.error("mkConstant not implemented for " + dt.toString)
  }

  // generate code to calculate the euclidian norm of a "vector"
  def norm(in : IR_VariableAccess, out : IR_VariableAccess, length : IR_Expression = IR_NullExpression) : IR_Scope = {
    var func = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var sum = IR_VariableAccess("sum", IR_DoubleDatatype)
    func.body += IR_VariableDeclaration(i)
    func.body += IR_VariableDeclaration(sum, IR_DoubleConstant(0))
    in.datatype match {
      case IR_MatrixDatatype(_, _, _) =>
        var size = IR_BasicMatrixOperations.getSize(in)
        var N : Int = 0
        var columnvector = size match {
          case (1, cols)    => false; N = cols
          case (rows, 1)    => true; N = rows
          case (rows, cols) => Logger.error("norming matrices without one dimension being of size 1 not supported")
        }
        func.body += IR_ForLoop(IR_Assignment(i, 0), IR_Lower(i, N), IR_PreIncrement(i), ListBuffer[IR_Statement](
          IR_Assignment(sum, IR_Addition(sum, IR_Multiplication(IR_ArrayAccess(in, i), IR_ArrayAccess(in, i))))
        ))
      case IR_PointerDatatype(_)      =>
        if (length == IR_NullExpression)
          Logger.error("no length specified")
        func.body += IR_ForLoop(IR_Assignment(i, 0), IR_Lower(i, length), IR_PreIncrement(i), ListBuffer[IR_Statement](
          IR_Assignment(sum, IR_Addition(sum, IR_Multiplication(IR_ArrayAccess(in, i), IR_ArrayAccess(in, i))))
        ))
    }
    func.body += IR_Assignment(out, IR_FunctionCall(IR_ExternalFunctionReference("std::sqrt", IR_DoubleDatatype), ListBuffer[IR_Expression](sum)))
    func
  }

  // generate code to transpose a matrix
  def transpose(in : IR_VariableAccess, out : IR_VariableAccess) : IR_Scope = {
    var insize = IR_BasicMatrixOperations.getSize(in)
    var outsize = IR_BasicMatrixOperations.getSize(out)
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
    var insize = IR_BasicMatrixOperations.getSize(in)
    var outsize = IR_BasicMatrixOperations.getSize(out)
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
    }
    func
  }

  // add 'left' and 'right' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def addAtSubmatrix(left : IR_VariableAccess, right : IR_VariableAccess, out : IR_VariableAccess, M : IR_Expression, N : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression) : IR_Scope = {
    var func = IR_Scope(Nil)
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    left match {
      case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Addition(IR_HighDimAccess(left, IR_ExpressionIndex(_i, _j)), IR_HighDimAccess(right, IR_ExpressionIndex(_i, _j))))
          ))
        ))
      case IR_VariableAccess(_, IR_PointerDatatype(_))      =>
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_Assignment(IR_ArrayAccess(out, (_i + offset_r) * N + _j + offset_c), IR_Addition(IR_ArrayAccess(left, (_i * N + _j)), IR_ArrayAccess(right, (_i * N + _j))))
          ))
        ))
    }
    func
  }

  // subtract 'left' from 'right' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def subAtSubmatrix(left : IR_VariableAccess, right : IR_VariableAccess, out : IR_VariableAccess, opcols : IR_Expression, oprows : IR_Expression, destcols : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression) : IR_Scope = {
    var func = IR_Scope(Nil)
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, oprows), IR_PreIncrement(_i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, opcols), IR_PreIncrement(_j), ListBuffer[IR_Statement](
        IR_Assignment(IR_ArrayAccess(out, (_i + offset_r) * destcols + _j + offset_c), IR_Subtraction(IR_ArrayAccess(left, _i * opcols + _j), IR_ArrayAccess(right, (_i * opcols + _j))))
      ))
    ))
    func
  }

  // produce negative of 'that' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def negAtSubmatrix(that : IR_VariableAccess, out : IR_VariableAccess, M : IR_Expression, N : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression) : IR_Scope = {
    var func = IR_Scope(Nil)
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    that match {
      case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Negative(IR_HighDimAccess(that, IR_ExpressionIndex(_i, _j))))
          ))
        ))
      case IR_VariableAccess(_, IR_PointerDatatype(_))      =>
        func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
            IR_Assignment(IR_ArrayAccess(out, (_i + offset_r) * N + _j + offset_c), IR_Negative(IR_ArrayAccess(that, _i * N + _j)))
          ))
        ))
    }
    func
  }

  // copy a submatrix of n_rows x n_cols to 'copy' from position 'offset_r', 'offset_c' in 'source'
  def loopCopySubmatrix(source : IR_Expression, dest : IR_VariableAccess, offset_r : IR_Expression, offset_c : IR_Expression, n_rows : IR_Expression, n_cols : IR_Expression) : IR_Scope = {
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
      case x @ IR_MatrixExpression(_, _, _)                      =>
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

  // copy a submatrix of n_rows x n_cols to 'copy' from position 'offset_r', 'offset_c' in 'source'
  def loopCopySubmatrixDynMem(source : IR_Expression, sourcesize : IR_Expression, dest : IR_VariableAccess, offset_r : IR_Expression, offset_c : IR_Expression, n_rows : IR_Expression, n_cols : IR_Expression) : IR_Scope = {
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
  def loopSetSubmatrixMat(source : IR_VariableAccess, destination : IR_VariableAccess, rows_source : IR_Expression, cols_source : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression) : IR_Scope = {
    if (!IR_ResolveMatrixOperations.isScalar(offset_r) || !IR_ResolveMatrixOperations.isScalar(offset_c))
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
  def loopSetSubmatrixMatDynMem(source : IR_VariableAccess, destination : IR_VariableAccess, destsize : IR_Expression, rows_source : IR_Expression, cols_source : IR_Expression, offset_r : IR_Expression, offset_c : IR_Expression) : IR_Scope = {
    if (!IR_ResolveMatrixOperations.isScalar(offset_r) || !IR_ResolveMatrixOperations.isScalar(offset_c))
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
  def loopSetSubmatrixSc(matrix : IR_VariableAccess, offsetRows : IR_Expression, offsetCols : IR_Expression, nRows : IR_Expression, nCols : IR_Expression, newValue : IR_Expression) : IR_Scope = {
    var stmts = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    var stream = IR_VariableAccess("std::cout", IR_StringDatatype)
    stmts.body += IR_ForLoop(IR_VariableDeclaration(i, offsetRows), IR_Lower(i, nRows + offsetRows), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(j, offsetCols), IR_Lower(j, nCols + offsetCols), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(matrix, IR_ExpressionIndex(i, j)), newValue)
      ))
    ))
    stmts
  }

  // code to generate a n-unitmatrix
  def unitmatrix(size : IR_Expression, out : IR_VariableAccess) : IR_Scope = {
    var outsize = IR_BasicMatrixOperations.getSize(out)
    if (outsize._1 != outsize._2)
      Logger.error("outmatrix not quadratic")
    var func = IR_Scope(Nil)
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)

    func.body += IR_ForLoop(IR_VariableDeclaration(_i, 0), IR_Lower(_i, outsize._1), IR_PreIncrement(_i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(_j, 0), IR_Lower(_j, outsize._2), IR_PreIncrement(_j), ListBuffer[IR_Statement](
        IR_IfCondition(IR_EqEq(_i, _j), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i, _j)), IR_DoubleConstant(1))
        ),
          ListBuffer[IR_Statement](
            IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i, _j)), IR_DoubleConstant(0))
          )
        )
      ))
    ))
    func
  }

  // generate determinant calculation if 'in' is lu decomposed
  def determinantLargeMatrix(in : IR_VariableAccess, P : IR_VariableAccess, out : IR_VariableAccess) : IR_Scope = {
    var func = IR_Scope(Nil)
    var det = IR_VariableAccess("det", IR_DoubleDatatype)
    var N = IR_BasicMatrixOperations.getSize(in)._1
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
  def determinant(in : IR_VariableAccess, out : IR_VariableAccess) : IR_Scope = {
    var func = IR_Scope(Nil)
    var size = IR_BasicMatrixOperations.getSize(in)
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
}

object IR_GenerateRuntimeInversion {

  // generate code for direct inversion of small matrices
  def smallMatrixInversion(in : IR_VariableAccess, out : IR_VariableAccess) : IR_Scope = {
    var debug = false

    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    //var stmts = IR_Scope(Nil)
    var stmts = ListBuffer[IR_Statement]()
    val M = inDt.sizeM
    M match {
      case 1 =>
        stmts += IR_Assignment(out, IR_Division(IR_RealConstant(1), IR_BasicMatrixOperations.getElem(in, 0, 0)))

      case 2 =>
        val a = IR_BasicMatrixOperations.getElem(in, 0, 0)
        val b = IR_BasicMatrixOperations.getElem(in, 0, 1)
        val c = IR_BasicMatrixOperations.getElem(in, 1, 0)
        val d = IR_BasicMatrixOperations.getElem(in, 1, 1)
        val det = IR_Division(IR_RealConstant(1.0), (a * d) - (b * c))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(0, 0)), Duplicate(det) * Duplicate(d))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(0, 1)), Duplicate(det) * Duplicate(b) * IR_IntegerConstant(-1))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(1, 0)), Duplicate(det) * Duplicate(c) * IR_IntegerConstant(-1))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(1, 1)), Duplicate(det) * Duplicate(a))

      case 3 =>
        val a = IR_BasicMatrixOperations.getElem(in, 0, 0)
        val b = IR_BasicMatrixOperations.getElem(in, 0, 1)
        val c = IR_BasicMatrixOperations.getElem(in, 0, 2)
        val d = IR_BasicMatrixOperations.getElem(in, 1, 0)
        val e = IR_BasicMatrixOperations.getElem(in, 1, 1)
        val f = IR_BasicMatrixOperations.getElem(in, 1, 2)
        val g = IR_BasicMatrixOperations.getElem(in, 2, 0)
        val h = IR_BasicMatrixOperations.getElem(in, 2, 1)
        val i = IR_BasicMatrixOperations.getElem(in, 2, 2)
        val A = Duplicate(e) * Duplicate(i) - Duplicate(f) * Duplicate(h)
        val B = IR_IntegerConstant(-1) * (Duplicate(d) * Duplicate(i) - Duplicate(f) * Duplicate(g))
        val C = Duplicate(d) * Duplicate(h) - Duplicate(e) * Duplicate(g)
        val D = IR_IntegerConstant(-1) * (Duplicate(b) * Duplicate(i) - Duplicate(c) * Duplicate(h))
        val E = Duplicate(a) * Duplicate(i) - Duplicate(c) * Duplicate(g)
        val F = IR_IntegerConstant(-1) * (Duplicate(a) * Duplicate(h) - Duplicate(b) * Duplicate(g))
        val G = Duplicate(b) * Duplicate(f) - Duplicate(c) * Duplicate(e)
        val H = IR_IntegerConstant(-1) * (Duplicate(a) * Duplicate(f) - Duplicate(c) * Duplicate(d))
        val I = Duplicate(a) * Duplicate(e) - Duplicate(b) * Duplicate(d)
        val det = Duplicate(a) * Duplicate(A) + Duplicate(b) * Duplicate(B) + Duplicate(c) * Duplicate(C)
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(0, 0)), Duplicate(A) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(0, 1)), Duplicate(D) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(0, 2)), Duplicate(G) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(1, 0)), Duplicate(B) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(1, 1)), Duplicate(E) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(1, 2)), Duplicate(H) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(2, 0)), Duplicate(C) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(2, 1)), Duplicate(F) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(2, 2)), Duplicate(I) / Duplicate(det))
    }
    IR_Scope(stmts)
  }

  // generate code for direct inversion of small matrices
  def smallMatrixInversion(in : IR_VariableAccess, blocksize : Int, offsetRows : IR_VariableAccess, offsetCols : IR_VariableAccess, out : IR_VariableAccess) : IR_Scope = {
    var debug = false

    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    //var stmts = IR_Scope(Nil)
    var stmts = ListBuffer[IR_Statement]()
    blocksize match {
      case 1 =>
        stmts += IR_Assignment(out, IR_Division(IR_RealConstant(1), IR_BasicMatrixOperations.getElem(in, 0, 0)))

      case 2 =>
        val a = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 0, offsetCols + 0))
        val b = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 0, offsetCols + 1))
        val c = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 1, offsetCols + 0))
        val d = IR_HighDimAccess(in, IR_ExpressionIndex(offsetRows + 1, offsetCols + 1))
        val det = IR_Division(IR_RealConstant(1.0), (a * d) - (b * c))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 0, offsetCols + 0)), Duplicate(det) * Duplicate(d))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 0, offsetCols + 1)), Duplicate(det) * Duplicate(b) * IR_IntegerConstant(-1))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 0, offsetCols + 0)), Duplicate(det) * Duplicate(c) * IR_IntegerConstant(-1))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 1, offsetCols + 1)), Duplicate(det) * Duplicate(a))

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
        val det = Duplicate(a) * Duplicate(A) + Duplicate(b) * Duplicate(B) + Duplicate(c) * Duplicate(C)
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows, offsetCols)), Duplicate(A) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows, offsetCols + 1)), Duplicate(D) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows, offsetCols + 2)), Duplicate(G) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 1, offsetCols)), Duplicate(B) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 1, offsetCols + 1)), Duplicate(E) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 1, offsetCols + 2)), Duplicate(H) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 2, offsetCols)), Duplicate(C) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 2, offsetCols + 1)), Duplicate(F) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(offsetRows + 2, offsetCols + 2)), Duplicate(I) / Duplicate(det))
    }
    IR_Scope(stmts)
  }

  // give a invert algorithm for diagonal matrices
  def diagonalInlined(in : IR_VariableAccess, out : IR_VariableAccess) : IR_Scope = {
    var debug = false
    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    val N = inDt.sizeM
    val inner = inDt.resolveBaseDatatype
    var func = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var tmp = IR_VariableAccess("tmp", inner)

    func.body += IR_VariableDeclaration(tmp)
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
  def localLUDecomp(in : IR_VariableAccess, P : IR_VariableAccess, blocksize_asInt : Int, offset_r : IR_VariableAccess, offset_c : IR_VariableAccess) : ListBuffer[IR_Statement] = {

    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    val Tol = IR_RealConstant(0.0001)
    val baseType = inDt.resolveBaseDatatype
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
      IR_IfCondition(IR_Lower(maxA, Tol), ListBuffer[IR_Statement](IR_Print(outstream, ListBuffer[IR_Expression](IR_StringConstant("[Warning] inverting potentially singular matrix\\n"))), IR_Return(IR_IntegerConstant(-1))), ListBuffer[IR_Statement]()),
      IR_IfCondition(IR_Neq(imax, i), ListBuffer[IR_Statement](
        IR_VariableDeclaration(tmp_idx, IR_ArrayAccess(P, i)),
        IR_Assignment(IR_ArrayAccess(P, i), IR_ArrayAccess(P, imax)),
        IR_Assignment(IR_ArrayAccess(P, imax), tmp_idx),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_ArrayAccess(tmp_row, 0)), IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, 0 + offset_c))), blocksize_asInt * IR_SizeOf(baseType)))),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, 0 + offset_c))), IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(imax + offset_r, 0 + offset_c))), blocksize_asInt * IR_SizeOf(baseType)))),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(imax + offset_r, 0 + offset_c))), IR_AddressOf(IR_ArrayAccess(tmp_row, 0)), blocksize_asInt * IR_SizeOf(baseType)))),
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
  //TODO integrate in normal method
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
    func
  }

  // generate an inverted matrix for a submatrix at 'offset_r','offset_c' if submatrix('in') is LU decomposed
  def localLUDecomposedInversion(in : IR_VariableAccess, P : IR_VariableAccess, blocksize : Int, offset_r : IR_VariableAccess, offset_c : IR_VariableAccess, out : IR_VariableAccess) : ListBuffer[IR_Statement] = {
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

  // combines LU decomposition and inversion of submatrix of 'in' at 'offset_r', 'offset_c' of size 'blocksize'
  def localLUInversionInlined(in : IR_VariableAccess, blocksize_asInt : Int, offset_r : IR_VariableAccess, offset_c : IR_VariableAccess, out : IR_VariableAccess) : IR_Scope = {
    var func = IR_Scope(Nil)
    var P = IR_VariableAccess("P", IR_ArrayDatatype(IR_IntegerDatatype, blocksize_asInt + 1))
    var block = IR_VariableAccess("block", IR_IntegerDatatype)
    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    val N = inDt.sizeM
    if (N % blocksize_asInt != 0) Logger.error("IR_ResolveMatrixFunctions::localLUInversion: Matrices with size not mutliple of blocksize not implemented yet")
    func.body += IR_VariableDeclaration(P)
    var inplace = Knowledge.experimental_inplaceInversion
    if (!inplace) {
      var inCopy = IR_VariableAccess("inCopy", IR_MatrixDatatype(inDt.resolveBaseDatatype, N, N))
      func.body += IR_VariableDeclaration(inCopy)
      func.body += IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy", IR_UnitDatatype), ListBuffer[IR_Expression](IR_AddressOf(inCopy), IR_AddressOf(in), IR_SizeOf(inDt.resolveBaseDatatype) * N * N))
      func.body ++= localLUDecomp(inCopy, P, blocksize_asInt, offset_r, offset_c)
      func.body ++= localLUDecomposedInversion(inCopy, P, blocksize_asInt, offset_r, offset_c, out)
    }
    else {
      func.body ++= localLUDecomp(in, P, blocksize_asInt, offset_r, offset_c)
      func.body ++= localLUDecomposedInversion(in, P, blocksize_asInt, offset_r, offset_c, out)
    }
    func
  }

  // combines LU decomposition and inversion of submatrix of 'in' at 'offset_r', 'offset_c' of size 'blocksize'
  def localLUInversionAsFunction() : IR_PlainFunction = {
    var func = IR_Scope(Nil)
    var offset_r = IR_VariableAccess("offset_r", IR_IntegerDatatype)
    var offset_c = IR_VariableAccess("offset_c", IR_IntegerDatatype)
    var in = IR_VariableAccess("in", IR_PointerDatatype(IR_DoubleDatatype))
    var insize = IR_VariableAccess("insize", IR_IntegerDatatype)
    var blocksize = IR_VariableAccess("blocksize", IR_IntegerDatatype)
    var out = IR_VariableAccess("out", IR_PointerDatatype(IR_DoubleDatatype))
    var P = IR_VariableAccess("P", IR_PointerDatatype(IR_IntegerDatatype))
    var block = IR_VariableAccess("block", IR_IntegerDatatype)
    val baseType = in.datatype.resolveBaseDatatype
    func.body += IR_VariableDeclaration(P)
    func.body += IR_ArrayAllocation(P, IR_IntegerDatatype, blocksize + IR_IntegerConstant(1))
    var inplace = Knowledge.experimental_inplaceInversion
    if (!inplace) {
      var inCopy = IR_VariableAccess("inCopy", IR_PointerDatatype(baseType))
      func.body += IR_VariableDeclaration(inCopy)
      func.body += IR_ArrayAllocation(inCopy, baseType, insize * insize)
      func.body += IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy", IR_UnitDatatype), ListBuffer[IR_Expression](IR_AddressOf(inCopy), IR_AddressOf(in), IR_SizeOf(baseType) * insize * insize))
      func.body ++= localLUDecompDynMem(inCopy, insize, P, blocksize, offset_r, offset_c)
      func.body ++= localLUDecomposedInversionDynMem(inCopy, insize, P, blocksize, offset_r, offset_c, out)
    }
    else {
      func.body ++= localLUDecompDynMem(in, insize, P, blocksize, offset_r, offset_c)
      func.body ++= localLUDecomposedInversionDynMem(in, insize, P, blocksize, offset_r, offset_c, out)
    }
    func.body += IR_ArrayFree(P)

    var pfunc = IR_PlainFunction("inv_filled", IR_UnitDatatype, ListBuffer[IR_FunctionArgument](
      IR_FunctionArgument("in", IR_PointerDatatype(IR_DoubleDatatype)),
      IR_FunctionArgument("insize", IR_IntegerDatatype),
      IR_FunctionArgument("blocksize", IR_IntegerDatatype),
      IR_FunctionArgument("offset_r", IR_IntegerDatatype),
      IR_FunctionArgument("offset_c", IR_IntegerDatatype),
      IR_FunctionArgument("out", IR_PointerDatatype(IR_DoubleDatatype))
    ),
      func.body
    )
    pfunc.allowInlining = false
    pfunc
  }

  // give an invert algorithm for blockdiagonal matrices
  def blockdiagonalInlined(in : IR_VariableAccess, blocksize : Int, out : IR_VariableAccess) : IR_Scope = {
    var debug = false
    var func = IR_Scope(Nil)
    var block = IR_VariableAccess("block", IR_IntegerDatatype)
    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    val N = inDt.sizeM
    if (N % blocksize != 0) Logger.error("Matrices with size not mutliple of blocksize not implemented yet")
    if (blocksize < 4) {
      func.body += IR_ForLoop(IR_VariableDeclaration(block, 0), IR_Lower(block, N), IR_Assignment(block, IR_Addition(block, blocksize)), ListBuffer[IR_Statement](
      ) += smallMatrixInversion(in, blocksize, block, block, out))
    }
    else {
      func.body += IR_ForLoop(IR_VariableDeclaration(block, 0), IR_Lower(block, N), IR_Assignment(block, IR_Addition(block, blocksize)), ListBuffer[IR_Statement](
      ) += localLUInversionInlined(in, blocksize, block, block, out))
    }

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

    func
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
    func.body += IR_ForLoop(IR_VariableDeclaration(block, 0), IR_Lower(block, insize), IR_Assignment(block, IR_Addition(block, blocksize)), ListBuffer[IR_Statement](
    ) += IR_FunctionCall(IR_PlainInternalFunctionReference("inv_filled", IR_UnitDatatype), ListBuffer[IR_Expression](in, insize, blocksize, block, block, out)))
    //localLUInversionAsFunction(in, insize, blocksize, block, block, out))
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

  /* give an invert algorithm using the schur complement

    -1             -1
   M  =  (  A  B  )      =    ( A_inv + A_inv*B*S_inv*C*A_inv -A_inv*B*S_inv  )
         (  C  D  )           (           -S_inv*C*A_inv           S_inv      )

             with M of size (n + m) x (n + m) and S = D - C * A_inv * B
 */

  def schurInlined(in : IR_VariableAccess, blockSize : Int, structureA : String, blockSizeA : Int, out : IR_VariableAccess) : IR_Scope = {
    var debug = false
    var func = IR_Scope(Nil)
    var inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    var baseType = inDt.resolveBaseDatatype
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var offset_r = IR_VariableAccess("offset_r", IR_IntegerDatatype)
    var offset_c = IR_VariableAccess("offset_c", IR_IntegerDatatype)
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

    func.body += IR_VariableDeclaration(offset_r)
    func.body += IR_VariableDeclaration(offset_c)
    func.body += IR_Assignment(offset_r, 0)
    func.body += IR_Assignment(offset_c, 0)
    func.body += IR_VariableDeclaration(n)
    func.body += IR_Assignment(n, blockSize)
    func.body += IR_VariableDeclaration(m)
    func.body += IR_Assignment(m, inDt.sizeM - blockSize)

    // copy A and invert
    //TODO use algorithm that exploits structure -> receive matrix structure information from classifier -> e.g. blockdiagonal
    // blocksize of the diagonal blocks of A if A is a blockdiagonal matrix -> later this information comes from the classifyer?
    func.body += IR_VariableDeclaration(A)
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrix(in, A, offset_r, offset_c, n, n)
    func.body += IR_VariableDeclaration(A_inv)
    if (structureA == "Blockdiagonal")
      func.body += IR_GenerateRuntimeInversion.blockdiagonalInlined(A, blockSizeA, A_inv)
    else if (structureA == "Diagonal")
      func.body += IR_GenerateRuntimeInversion.diagonalInlined(A, A_inv)
    else
      func.body += IR_GenerateRuntimeInversion.localLUInversionInlined(A, n_asInt, offset_r, offset_c, A_inv)

    // copy B
    func.body += IR_VariableDeclaration(B)
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrix(in, B, offset_r, n, n, m)

    // copy C
    func.body += IR_VariableDeclaration(C)
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrix(in, C, n, offset_c, m, n)

    // copy D
    func.body += IR_VariableDeclaration(D)
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrix(in, D, n, n, m, m)

    // calculate S
    func.body += IR_VariableDeclaration(S)
    func.body += IR_VariableDeclaration(CA_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(C, A_inv, CA_inv, m, n, n, 0, 0)
    func.body += IR_VariableDeclaration(CA_invB)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(CA_inv, B, CA_invB, m, m, n, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.subAtSubmatrix(D, CA_invB, S, n, n, n, 0, 0)

    // calculate S_inv
    func.body += IR_VariableDeclaration(S_inv)
    func.body += IR_GenerateRuntimeInversion.inverse(S, S_inv, "Filled")

    // calculate upper right result block
    func.body += IR_VariableDeclaration(A_invB)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_inv, B, A_invB, n, m, n, 0, 0)
    func.body += IR_VariableDeclaration(A_invBS_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_invB, S_inv, A_invBS_inv, n, m, m, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(A_invBS_inv, out, n, m, 0, n_asInt)

    // insert lower right result block
    func.body += IR_GenerateBasicMatrixOperations.loopSetSubmatrixMat(S_inv, out, m, m, n, n)

    // calculate lower left result block
    func.body += IR_VariableDeclaration(S_invCA_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(S_inv, CA_inv, S_invCA_inv, m, n, m, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(S_invCA_inv, out, m, n, n_asInt, 0)

    // calculate upper left result block
    func.body += IR_VariableDeclaration(A_invBS_invCA_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_invB, S_invCA_inv, A_invBS_invCA_inv, n, n, m, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.addAtSubmatrix(A_inv, A_invBS_invCA_inv, out, n, n, 0, 0)

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

    func
  }

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
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
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

    func.body += IR_ArrayAllocation(A, baseType, n * n)
    func.body += IR_ArrayAllocation(A_inv, baseType, n * n)
    func.body += IR_ArrayAllocation(B, baseType, n * m)
    func.body += IR_ArrayAllocation(C, baseType, m * n)
    func.body += IR_ArrayAllocation(D, baseType, m * m)
    func.body += IR_ArrayAllocation(S, baseType, m * m)
    func.body += IR_ArrayAllocation(S_inv, baseType, m * m)
    func.body += IR_ArrayAllocation(CA_inv, baseType, m * n)
    func.body += IR_ArrayAllocation(CA_invB, baseType, m * m)
    func.body += IR_ArrayAllocation(A_invB, baseType, n * m)
    func.body += IR_ArrayAllocation(A_invBS_inv, baseType, n * m)
    func.body += IR_ArrayAllocation(S_invCA_inv, baseType, m * n)
    func.body += IR_ArrayAllocation(A_invBS_invCA_inv, baseType, n * n)

    // copy A and invert
    //TODO use algorithm that exploits structure -> receive matrix structure information from classifier -> e.g. blockdiagonal
    // blocksize of the diagonal blocks of A if A is a blockdiagonal matrix -> later this information comes from the classifyer?
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrixDynMem(in, insize, A, offset_r, offset_c, n, n)
    func.body += IR_IfCondition(IR_EqEq(structureA, IR_StringConstant("Blockdiagonal")), ListBuffer[IR_Statement](
      IR_FunctionCall(IR_PlainInternalFunctionReference("inv_blockdiagonal", IR_UnitDatatype), ListBuffer[IR_Expression](A, insize, blockSize, A_inv))
    ), ListBuffer[IR_Statement](
      IR_IfCondition(IR_EqEq(structureA, IR_StringConstant("Diagonal")), ListBuffer[IR_Statement](
        IR_FunctionCall(IR_PlainInternalFunctionReference("inv_diagonal", IR_UnitDatatype), ListBuffer[IR_Expression](A, insize, A_inv))
      ), ListBuffer[IR_Statement](
        IR_FunctionCall(IR_PlainInternalFunctionReference("inv_filled", IR_UnitDatatype), ListBuffer[IR_Expression](A, insize, n, offset_r, offset_c, A_inv))
      ))
    ))
    //func.body += IR_GenerateRuntimeInversion.blockdiagonalAsFunction(A, insize, blockSizeA, A_inv)
    //    func.body += IR_GenerateRuntimeInversion.diagonalAsFunction(A, insize, A_inv)
    //   func.body += IR_GenerateRuntimeInversion.localLUInversionAsFunction(A, insize, n, offset_r, offset_c, A_inv)

    // copy B
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrixDynMem(in, insize, B, offset_r, n, n, m)

    // copy C
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrixDynMem(in, insize, C, n, offset_c, m, n)

    // copy D
    func.body += IR_GenerateBasicMatrixOperations.loopCopySubmatrixDynMem(in, insize, D, n, n, m, m)

    // calculate S
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(C, A_inv, CA_inv, m, n, n, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(CA_inv, B, CA_invB, m, m, n, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.subAtSubmatrix(D, CA_invB, S, n, n, n, 0, 0)

    // calculate S_inv
    //func.body += IR_GenerateRuntimeInversion.localLUInversionAsFunction(S, n, n, IR_IntegerConstant(0), IR_IntegerConstant(0), S_inv)
    func.body += IR_FunctionCall(IR_PlainInternalFunctionReference("inv_filled", IR_UnitDatatype), ListBuffer[IR_Expression](S, n, n, 0, 0, S_inv))

    // calculate upper right result block
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_inv, B, A_invB, n, m, n, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_invB, S_inv, A_invBS_inv, n, m, m, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(A_invBS_inv, out, n, m, 0, n)

    // insert lower right result block
    func.body += IR_GenerateBasicMatrixOperations.loopSetSubmatrixMatDynMem(S_inv, out, insize, m, m, n, n)

    // calculate lower left result block
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(S_inv, CA_inv, S_invCA_inv, m, n, m, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(S_invCA_inv, out, m, n, n, 0)

    // calculate upper left result block
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_invB, S_invCA_inv, A_invBS_invCA_inv, n, n, m, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.addAtSubmatrix(A_inv, A_invBS_invCA_inv, out, n, n, 0, 0)

    func.body += IR_ArrayFree(A)
    func.body += IR_ArrayFree(A_inv)
    func.body += IR_ArrayFree(B)
    func.body += IR_ArrayFree(C)
    func.body += IR_ArrayFree(D)
    func.body += IR_ArrayFree(S)
    func.body += IR_ArrayFree(S_inv)
    func.body += IR_ArrayFree(CA_inv)
    func.body += IR_ArrayFree(CA_invB)
    func.body += IR_ArrayFree(A_invB)
    func.body += IR_ArrayFree(A_invBS_inv)
    func.body += IR_ArrayFree(S_invCA_inv)
    func.body += IR_ArrayFree(A_invBS_invCA_inv)

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

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

  // head function that branches to specific inversions
  def inverse(in : IR_VariableAccess, out : IR_VariableAccess, matrixStructure : String) : IR_Scope = {
    var insize = IR_BasicMatrixOperations.getSize(in)
    var outsize = IR_BasicMatrixOperations.getSize(out)
    if (insize._1 != insize._2)
      Logger.error("inversion of matrices of size " + insize._1 + "," + insize._2 + " not supported")
    if (insize != outsize)
      Logger.error("matrix sizes of in and out do not match: " + insize + " vs " + outsize)

    matrixStructure match {
      case "Filled"        =>
        var debug = false
        //TODO maybe overload GenerateRuntimeInversion methods or 0-access-constant
        var stmts = ListBuffer[IR_Statement]()

        if (insize._1 < 4) {
          stmts += smallMatrixInversion(in, out)
        } else {
          var offsetIsZero = IR_VariableAccess("zero", IR_IntegerDatatype)
          stmts += IR_VariableDeclaration(offsetIsZero, 0)
          // use localLUInversion for the full matrix
          stmts += localLUInversionInlined(in, insize._1, offsetIsZero, offsetIsZero, out)
        }

        if (debug)
          stmts ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

        IR_Scope(stmts)
      case "Diagonal"      => diagonalInlined(in, out)
      case "Blockdiagonal" => blockdiagonalInlined(in, Knowledge.experimental_blocksize, out)
      case "Schur"         => schurInlined(in, Knowledge.experimental_blocksize, Knowledge.experimental_structure_A, Knowledge.experimental_blocksize_A, out)
      case _               => Logger.error("runtime inversion: unknown runtimeInverse resolve: " + Knowledge.experimental_matrixStructure)
    }
  }
}

/*
object IR_GenerateQRDecomposition {

    // generate code to calculate a householder matrix from a column vector
    def householderMatrix(a : IR_VariableAccess,  hhmatrix : IR_VariableAccess) : IR_Scope = {
      var debug = true

      var asize = IR_BasicMatrixOperations.getSize(a)
      var hhsize = IR_BasicMatrixOperations.getSize(hhmatrix)
      if(hhsize._1 != hhsize._2)
        Logger.error("householder for nonquadratic matrices not supported: " + hhsize)
      if(asize._1 != hhsize._1)
        Logger.error("sizes do not match: " + asize + " vs " + hhsize)
      var func = IR_Scope(Nil)
      var N = asize._1

      // auxilliary variables
      var i = IR_VariableAccess("i", IR_IntegerDatatype)
      var j = IR_VariableAccess("j", IR_IntegerDatatype)
      var alpha = IR_VariableAccess("alpha",IR_DoubleDatatype)
      var u = IR_VariableAccess("u", IR_MatrixDatatype(IR_DoubleDatatype,N,1))
      var e1 = IR_VariableAccess("e1", IR_MatrixDatatype(IR_DoubleDatatype,N,1))
      var tmp = IR_VariableAccess("tmp", IR_MatrixDatatype(IR_DoubleDatatype,N,1))
      var tmpMatrix = IR_VariableAccess("tmpMatrix", IR_MatrixDatatype(IR_DoubleDatatype,N,N))
      var I = IR_VariableAccess("I", IR_MatrixDatatype(IR_DoubleDatatype,N,N))
      var v = IR_VariableAccess("v", IR_MatrixDatatype(IR_DoubleDatatype,N,1))
      var v_transposed = IR_VariableAccess("v_transposed", IR_MatrixDatatype(IR_DoubleDatatype,1,N))
      var norm_u = IR_VariableAccess("norm_u", IR_DoubleDatatype)
      var printstream = IR_VariableAccess("std::cout",IR_StringDatatype)

      // declarations
      func.body += IR_VariableDeclaration(i)
      func.body += IR_VariableDeclaration(j)
      func.body += IR_VariableDeclaration(alpha)
      func.body += IR_VariableDeclaration(u)
      func.body += IR_VariableDeclaration(e1)
      func.body += IR_VariableDeclaration(norm_u)
      func.body += IR_VariableDeclaration(v)
      func.body += IR_VariableDeclaration(v_transposed)
      func.body += IR_VariableDeclaration(tmpMatrix)
      func.body += IR_VariableDeclaration(I)
      func.body += IR_VariableDeclaration(tmp)

      // fill unit vector
      func.body += IR_Assignment(IR_ArrayAccess(e1,0),IR_DoubleConstant(1))

      // calculate norm of column vector
      func.body += IR_GenerateBasicMatrixOperations.norm(a,alpha)
      if(debug)
        func.body += IR_Print(printstream,alpha,IR_StringConstant("\\n"))

      // calculate u
      func.body += IR_GenerateBasicMatrixOperations.elementwiseMultiplication(alpha,e1,tmp)
      func.body += IR_GenerateBasicMatrixOperations.subAtSubmatrix(a,tmp,u,0,0)
      if(debug)
        func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(u)

      // norm u
      func.body += IR_GenerateBasicMatrixOperations.norm(u,norm_u)

      // calculate v and transpose
      func.body += IR_GenerateBasicMatrixOperations.elementwiseMultiplication(IR_Division(1,norm_u),u,v)
      func.body += IR_GenerateBasicMatrixOperations.transpose(v,v_transposed)
      if(debug) {
        func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(v)
        func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(v_transposed)
      }


      // calculate householder matrix and write to hhmatrix(out)
      func.body += IR_GenerateBasicMatrixOperations.unitmatrix(N,I)
      func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(v,v_transposed,tmpMatrix,0,0)
      func.body += IR_GenerateBasicMatrixOperations.elementwiseMultiplication(2,tmpMatrix,tmpMatrix)
      if(debug)
        func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(tmpMatrix)
      func.body += IR_GenerateBasicMatrixOperations.subAtSubmatrix(I,tmpMatrix, hhmatrix,0,0)

      func
    }
  */
// generate a runtime inversion
