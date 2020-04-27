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
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.globals.ir.IR_GlobalCollection
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

/// IR_MatrixExpression
object IR_MatrixExpression {
  //def apply(innerDatatype : Option[IR_Datatype], rows : Integer, columns : Integer) : IR_MatrixExpression = new IR_MatrixExpression(innerDatatype, rows, columns)
  def apply(innerDatatype : IR_Datatype, rows : Integer, columns : Integer) : IR_MatrixExpression = new IR_MatrixExpression(Some(innerDatatype), rows, columns)

  def apply(innerDatatype : Option[IR_Datatype], rows : Integer, columns : Integer, expressions : Array[IR_Expression]) : IR_MatrixExpression = {
    val tmp = new IR_MatrixExpression(innerDatatype, rows, columns)
    tmp.expressions = expressions
    tmp
  }

  def apply(innerDatatype : Option[IR_Datatype], expressions : ListBuffer[ListBuffer[IR_Expression]]) : IR_MatrixExpression = {
    val rows = expressions.size
    val columns = expressions(0).size
    val tmp = new IR_MatrixExpression(innerDatatype, rows, columns)
    for (row <- 0 until rows) {
      for (col <- 0 until columns) {
        tmp.set(row, col, expressions(row)(col))
      }
    }
    tmp
  }

  def apply(datatype : IR_MatrixDatatype, expressions : ListBuffer[IR_Expression]) : IR_MatrixExpression = {
    val tmp = IR_MatrixExpression(datatype.datatype, datatype.sizeM, datatype.sizeN)
    tmp.expressions = expressions.toArray
    tmp
  }

  def fromSingleExpression(innerDatatype : IR_Datatype, rows : Integer, columns : Integer, expression : IR_Expression) : IR_MatrixExpression = {
    val tmp = new IR_MatrixExpression(Some(innerDatatype), rows, columns)
    for (i <- 0 until rows * columns)
      tmp.expressions(i) = Duplicate(expression)
    tmp
  }
}

case class IR_MatrixExpression(var innerDatatype : Option[IR_Datatype], var rows : Int, var columns : Int) extends IR_Expression {
  var expressions : Array[IR_Expression] = Array.ofDim[IR_Expression](rows * columns)

  override def datatype = {
    innerDatatype match {
      case None                         =>
        var ret = expressions(0).datatype
        expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
        innerDatatype = Some(ret)
      case Some(dt : IR_MatrixDatatype) => innerDatatype = Some(dt.resolveBaseDatatype)
      case _                            =>
    }
    IR_MatrixDatatype(innerDatatype.getOrElse(IR_RealDatatype), this.rows, this.columns)
  }

  def prettyprintInner(out : PpStream) : Unit = {
    out << '{' << expressions.map(_.prettyprint).mkString(", ") << '}'
  }

  override def prettyprint(out : PpStream) : Unit = {
    out << "__matrix_"
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out << '_' << rows << "_" << columns << "_t "
    prettyprintInner(out)
  }

  def isConstant = expressions.forall(e => e.isInstanceOf[IR_Number])

  def isInteger = expressions.forall(e => e.isInstanceOf[IR_IntegerConstant])

  def isReal = expressions.forall(e => e.isInstanceOf[IR_RealConstant])

  def get(row : Integer, column : Integer) = expressions(row * columns + column)

  def set(row : Integer, column : Integer, exp : IR_Expression) = expressions(row * columns + column) = exp

  def inverse : IR_MatrixExpression = {
    IR_CompiletimeInversion.inverse(this, Knowledge.experimental_resolveInverseFunctionCall, Knowledge.experimental_blocksize)
  }

  override def toString : String = {
    "IR_MatrixExpression(" + innerDatatype + ", " + rows + ", " + columns + "; Items: " + expressions.mkString(", ") + ")"
  }

}

// trying out new strategies to resolve matrices
// resolve "Var matrix : Matrix<Datatype, rows, columns> = initialization" or split to declaration and assignment if convenient
object IR_ResolveMatrixDeclarations extends DefaultStrategy("Resolve matrix decl + initialization") {
  var matrixDeclCounter = 0

  this += new Transformation("with constants", {
    // split to use std::fill later
    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _), _, Some(IR_FloatConstant(_) | IR_DoubleConstant(_) | IR_RealConstant(_) | IR_IntegerConstant(_)), _)           =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)

    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _), _, Some(IR_VariableAccess(_, IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype)), _) =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)
  })

  this += new Transformation("with matrices", {
    // do nothing
    case decl @ IR_VariableDeclaration(declDt @ IR_MatrixDatatype(_, _, _), _, Some(srcDt @ IR_MatrixExpression(_, _, _)), _) =>
      if(declDt.sizeM != srcDt.rows || declDt.sizeN != srcDt.columns)
        Logger.error("Declaration of variable of type: " + declDt + " with expression of type: " + srcDt + ", sizes must match!")
      decl

    // split to use std::memcpy or std::copy later
    case decl @ IR_VariableDeclaration(declDt @ IR_MatrixDatatype(_, _, _), _, Some(IR_VariableAccess(_, srcDt @ IR_MatrixDatatype(_, _, _))), _) =>
        if(declDt.sizeM != srcDt.sizeM || declDt.sizeN != srcDt.sizeN)
          Logger.error("Declaration of variable of type: " + declDt + " with expression of type: " + srcDt + ", sizes must match!")
      IR_MatrixNodeUtilities.splitDeclaration(decl)
  })

  // resolve declarations with operators directly
  this += new Transformation("with operators", {
    // initialize variable with product of matrices
    case IR_VariableDeclaration(datatype @ IR_MatrixDatatype(_, _, _), name : String, Some(init : IR_Multiplication), _) =>
      init.factors.foreach(s => if (!s.datatype.isInstanceOf[IR_MatrixDatatype]) Logger.error("factor " + s + " is not a matrix"))
      if (init.factors.length < 2)
        Logger.error("multiplication of at least 2 matrices expected")
      IR_VariableDeclaration(datatype, name, Some(IR_BasicMatrixOperations.mult(init)))

    // elementwise operators
    // initialize variable with sum of multiple matrices
    case IR_VariableDeclaration(datatype @ IR_MatrixDatatype(_, _, _), name : String, Some(init @ (IR_Addition(_) | IR_ElementwiseAddition(_, _))), _) =>
      IR_VariableDeclaration(datatype, name, Some(IR_BasicMatrixOperations.add(init)))

    // initialize variable with subtraction of two matrices
    case IR_VariableDeclaration(datatype @ IR_MatrixDatatype(_, _, _), name : String, Some(init @ (IR_Subtraction(_, _) | IR_ElementwiseSubtraction(_, _))), _) =>
      IR_VariableDeclaration(datatype, name, Some(IR_BasicMatrixOperations.sub(init)))

    // initialize variable with elementwise multiplication of two matrices
    case IR_VariableDeclaration(datatype @ IR_MatrixDatatype(_, _, _), name : String, Some(init @ IR_ElementwiseMultiplication(_, _)), _) if (init.left.datatype.isInstanceOf[IR_MatrixDatatype] | init.right.datatype.isInstanceOf[IR_MatrixDatatype] | init.left.isInstanceOf[IR_MatrixExpression] | init.right.isInstanceOf[IR_MatrixExpression]) =>
      IR_VariableDeclaration(datatype, name, Some(IR_BasicMatrixOperations.elementwiseMultiplication(init.left, init.right)))

    // initialize variable with elementwise division of two matrices
    case IR_VariableDeclaration(datatype @ IR_MatrixDatatype(_, _, _), name : String, Some(init @ IR_ElementwiseDivision(_, _)), _) if (init.left.datatype.isInstanceOf[IR_MatrixDatatype] | init.right.datatype.isInstanceOf[IR_MatrixDatatype] | init.left.isInstanceOf[IR_MatrixExpression] | init.right.isInstanceOf[IR_MatrixExpression]) =>
      IR_VariableDeclaration(datatype, name, Some(IR_BasicMatrixOperations.elementwiseDivision(init.left, init.right)))
  })

  this += new Transformation("with built-in functions", {
    // resolve transpose call directly
    case decl @ IR_VariableDeclaration(datatype @ IR_MatrixDatatype(_, _, _), name : String, Some(init : IR_FunctionCall), _) if (init.name == "transpose") =>
      if (init.arguments.length != 1)
        Logger.error("wrong number of arguments: " + init.arguments.length + ", expected 1 arument for transpose")
      init.arguments(0) match {
        case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
          IR_VariableDeclaration(datatype, name, Some(IR_BasicMatrixOperations.transpose(va)))
        case _                                                     =>
          Logger.error("wrong type of argument: " + init.arguments(0).datatype + ", expected variable access to matrix variable")
      }

    // split inverse call to handle as assignment
    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _), _, Some(init : IR_FunctionCall), _) if (init.name == "inverse") =>
      if (init.arguments.length != 1)
        Logger.error("wrong number of arguments: " + init.arguments.length + ", expected 1 argument of type matrix")
      if (!init.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype])
        Logger.error("wrong type of argument: " + init.arguments(0).datatype)
      IR_MatrixNodeUtilities.splitDeclaration(decl)

    // split (symbolic) getSlice call so it can be resolved at compiletime and runtime
    case decl @ IR_VariableDeclaration(datatype @ IR_MatrixDatatype(_, _, _), name, Some(init @ IR_FunctionCall(_, _)), _) if (init.name == "getSlice") =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)

    // resolve dot product
    case IR_VariableDeclaration(datatype @ (IR_MatrixDatatype(_, _, _) | IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype | IR_FloatDatatype), name : String, Some(init @ IR_FunctionCall(_, _)), _) if (init.name == "dot" || init.name == "dotProduct") =>
      if (init.arguments.length != 2)
        Logger.error("wrong number of arguments: " + init.arguments.length + ", expected 2 arguments of type matrix for dotProduct")
      IR_VariableDeclaration(datatype, name, IR_BasicMatrixOperations.dotProduct(init.arguments(0), init.arguments(1)))

    // split determinant call
    case decl @ IR_VariableDeclaration(IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype, _, Some(call @ IR_FunctionCall(_, _)), _) if (call.name == "determinant") =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)

    // split getElement call
    case decl @ IR_VariableDeclaration(IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype, _, Some(call @ IR_FunctionCall(_, _)), _) if (call.name == "getElement")                               =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)

    // resolve cross product
    case IR_VariableDeclaration(datatype @ IR_MatrixDatatype(_, _, _), name, Some(call @ IR_FunctionCall(_, _)), _) if (call.name == "crossProduct" || call.name == "cross")                                               =>
      if (call.arguments.length != 2)
        Logger.error("wrong number of arguments: " + call.arguments.length)
      IR_VariableDeclaration(datatype, name, IR_BasicMatrixOperations.crossProduct(call.arguments(0), call.arguments(1)))

      // resolve trace call
    case IR_VariableDeclaration(datatype @ (IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype), name, Some(call @ IR_FunctionCall(_, _)), _) if (call.name == "trace")                           =>
      if (call.arguments.length != 1)
        Logger.error("wrong number of arguments: " + call.arguments.length + ", expected 1 argument for function call 'trace'")
      IR_VariableDeclaration(datatype, name, IR_BasicMatrixOperations.trace(call.arguments(0)))

      // resolve matrix multiplication
    case IR_VariableDeclaration(datatype @ IR_MatrixDatatype(_,_,_), name, Some(call @ IR_FunctionCall(_, _)), _) if (call.name == "matmult")                           =>
      if(call.arguments.length < 2)
        Logger.error("expected at least two matrices to multiply, got: " + call.arguments.length)
      IR_VariableDeclaration(datatype, name, IR_BasicMatrixOperations.mult(IR_Multiplication(call.arguments)))

      //TODO experimental: eigenvalues
    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _) | IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype, _, Some(call @ IR_FunctionCall(_, _)), _) if (call.name == "eigenvalues") =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)
  })
}

// resolve "matrix = expression"
object IR_ResolveMatrixAssignmentsNew extends DefaultStrategy("Resolve matrix assignments") {
  var debug = false

  // use std::fill for assignments of matrices with constants
  this += new Transformation("with constants", {
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src @ (IR_RealConstant(_) | IR_DoubleConstant(_) | IR_IntegerConstant(_) | IR_FloatConstant(_)), "=")         =>
      IR_FunctionCall(IR_ExternalFunctionReference("std::fill", IR_UnitDatatype), ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + dest.datatype.asInstanceOf[IR_MatrixDatatype].resolveFlattendSize, src)) : IR_Statement
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src @ IR_VariableAccess(_, IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype | IR_FloatDatatype), "=") =>
      IR_FunctionCall(IR_ExternalFunctionReference("std::fill", IR_UnitDatatype), ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + dest.datatype.asInstanceOf[IR_MatrixDatatype].resolveFlattendSize, src)) : IR_Statement
  })

  // assignment of a matrix with another matrix : copy other matrix
  this += new Transformation("with matrices", {
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src @ IR_MatrixExpression(_, _, _), "=")                     =>
      val destSize = IR_BasicMatrixOperations.getSize(dest)
      val srcSize = IR_BasicMatrixOperations.getSize(src)
      if (destSize != srcSize)
        Logger.error("sizes do not match: " + destSize + " vs " + srcSize)
      var stmts = ListBuffer[IR_Statement]()
      for (i <- 0 until destSize._1) {
        for (j <- 0 until destSize._2) {
          stmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(i, j)), src.get(i, j))
        }
      }
      stmts
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), "=") =>
      if (dest.datatype.resolveBaseDatatype != src.datatype.resolveBaseDatatype)
        Logger.error("datatypes do not match: " + dest.datatype.resolveBaseDatatype + " vs " + src.datatype.resolveBaseDatatype)
      val destSize = IR_BasicMatrixOperations.getSize(dest)
      val srcSize = IR_BasicMatrixOperations.getSize(src)
      if (destSize != srcSize)
        Logger.error("sizes do not match: " + destSize + " vs " + srcSize)
      IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy", IR_UnitDatatype), ListBuffer[IR_Expression](IR_AddressOf(dest), IR_AddressOf(src), IR_SizeOf(dest.datatype.resolveBaseDatatype) * destSize._1 * destSize._2)) : IR_Statement
  })

  // resolve assignments of matrices with operators
  //TODO elementwise power and modulo: does not parse
  this += new Transformation("with operators", {
    // (pointwise) addition of matrices
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), addition @ (IR_Addition(_) | IR_ElementwiseAddition(_, _)), _) =>
      IR_MatrixNodeUtilities.expressionToAssignments(dest, IR_BasicMatrixOperations.add(addition))

    // (pointwise) subtraction of two matrices
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), subtraction @ (IR_Subtraction(_, _) | IR_ElementwiseSubtraction(_, _)), _) =>
      IR_MatrixNodeUtilities.expressionToAssignments(dest, IR_BasicMatrixOperations.sub(subtraction))

    // multiplication of matrices
    //TODO multiplication of multiple matrices with different sizes -> error (out of bounds)
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), mult : IR_Multiplication, _) =>
      IR_MatrixNodeUtilities.expressionToAssignments(dest, IR_BasicMatrixOperations.mult(mult))

    // pointwise multiplication of two matrices
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), mult : IR_ElementwiseMultiplication, _) if (mult.left.datatype.isInstanceOf[IR_MatrixExpression] | mult.right.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      IR_MatrixNodeUtilities.expressionToAssignments(dest, IR_BasicMatrixOperations.elementwiseMultiplication(mult.left, mult.right))

    // pointwise division of two matrices
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), div : IR_ElementwiseDivision, _) if (div.left.datatype.isInstanceOf[IR_MatrixExpression] | div.right.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      IR_MatrixNodeUtilities.expressionToAssignments(dest, IR_BasicMatrixOperations.elementwiseMultiplication(div.left, div.right))
  })

  this += new Transformation("with build-in functions", {
    // transpose a matrix
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), call @ IR_FunctionCall(_, ListBuffer(matrix @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)))), _) if (call.name == "transpose") =>
      if (call.arguments.length != 1)
        Logger.error("wrong number of arguments")
      IR_MatrixNodeUtilities.expressionToAssignments(dest, IR_BasicMatrixOperations.transpose(matrix))

    // slice a matrix: test for compiletime constants and call slicing at compile/runtime
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), call @ IR_FunctionCall(_, ListBuffer(matrix @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), offsetRows : IR_Expression, offsetCols : IR_Expression, nRows : IR_Expression, nCols : IR_Expression)), _) if (call.name == "getSlice") =>
      if (call.arguments.length != 5)
        Logger.error("wrong number of arguments")
      var offsetRows_asInt : Int = 0
      var offsetCols_asInt : Int = 0
      var nRows_asInt : Int = 0
      var nCols_asInt : Int = 0
      var allCompiletimeConstant = true
      try {
        offsetRows_asInt = IR_SimplifyExpression.evalIntegral(offsetRows).toInt
        offsetCols_asInt = IR_SimplifyExpression.evalIntegral(offsetCols).toInt
        nRows_asInt = IR_SimplifyExpression.evalIntegral(nRows).toInt
        nCols_asInt = IR_SimplifyExpression.evalIntegral(nCols).toInt
      } catch {
        case e : EvaluationException => allCompiletimeConstant = false
        case _ : Throwable           => Logger.error("unexpected exception")
      }
      var out = allCompiletimeConstant match {
        case true  =>
          IR_MatrixNodeUtilities.expressionToAssignments(dest, IR_BasicMatrixOperations.copySubMatrix(matrix, offsetRows_asInt, offsetCols_asInt, nRows_asInt, nCols_asInt))
        case false =>
          IR_GenerateBasicMatrixOperations.copySubmatrix(matrix, dest, offsetRows, offsetCols, nRows, nCols)
      }
      out

    // dot product of two matrices
    case IR_Assignment(dest @ IR_VariableAccess(_, _), call @ IR_FunctionCall(_, _), _) if (call.name == "dotProduct" || call.name == "dot")     =>
      if (call.arguments.length != 2)
        Logger.error("wrong number of arguments")
      IR_MatrixNodeUtilities.expressionToAssignments(dest, IR_BasicMatrixOperations.dotProduct(call.arguments(0), call.arguments(1)))

      // cross product
    case IR_Assignment(dest @ IR_VariableAccess(_, _), call @ IR_FunctionCall(_, _), _) if (call.name == "crossProduct" || call.name == "cross") =>
      if (call.arguments.length != 2)
        Logger.error("wrong number of arguments: " + call.arguments.length)
      IR_MatrixNodeUtilities.expressionToAssignments(dest, IR_BasicMatrixOperations.crossProduct(call.arguments(0), call.arguments(1)))

      // trace
    case IR_Assignment(dest @ IR_VariableAccess(_, _), call @ IR_FunctionCall(_, _), _) if (call.name == "trace")                                =>
      if (call.arguments.length != 1)
        Logger.error("wrong number of arguments: " + call.arguments.length + ", expected 1 argument for trace call")
      IR_Assignment(dest, IR_BasicMatrixOperations.trace(call.arguments(0)))

      // matrix multiplication
    case IR_Assignment(dest @ IR_VariableAccess(_, _), call @ IR_FunctionCall(_, _), _) if (call.name == "matmult")                                =>
      if (call.arguments.length < 2)
        Logger.error("expected at least 2 arguments for matrix multiplication, got: " + call.arguments.length)
      IR_MatrixNodeUtilities.expressionToAssignments(dest,IR_BasicMatrixOperations.mult(IR_Multiplication(call.arguments)))

    //TODO TEST
    /*
    case IR_Assignment(dest @ IR_VariableAccess(_, _), call @ IR_FunctionCall(_, _), _) if (call.name == "eigenvalues") =>
      IR_GenerateQRDecomposition.householderQRDecomposition(call.arguments(0).asInstanceOf[IR_VariableAccess], dest)
    */
  })

  // resolve inversion call depending on Knowledge.experimentalResolveInverseFunctionCall and Knowledge.experimental_matrixStructure
  this += new Transformation("with inversion call", {
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), call : IR_FunctionCall, _) if (call.name == "inverse") =>
      if (call.arguments.length != 1)
        Logger.error("resolve inversion: inverse call with " + call.arguments.length + " arguments not supported")
      var inMatrix = call.arguments(0)
      Knowledge.experimental_resolveInverseFunctionCall match {
        case "Runtime"     =>
          inMatrix match {
            case x : IR_MatrixExpression                               =>
              var tmp = IR_VariableDeclaration(IR_MatrixDatatype(x.datatype.resolveBaseDatatype, x.rows, x.columns), "tmp", x)
              IR_Scope(
                tmp,
                IR_GenerateRuntimeInversion.inverse(IR_VariableAccess(tmp), Knowledge.experimental_matrixStructure, Knowledge.experimental_blocksize, dest)
              )
            case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_GenerateRuntimeInversion.inverse(va, Knowledge.experimental_matrixStructure, Knowledge.experimental_blocksize, dest)
            case _                                                     => Logger.error("argument of unexpected type: " + inMatrix.datatype)
          }
        case "Compiletime" =>
          inMatrix match {
            case x : IR_MatrixExpression                               => IR_MatrixNodeUtilities.expressionToAssignments(dest, IR_CompiletimeInversion.inverse(x, Knowledge.experimental_matrixStructure, Knowledge.experimental_blocksize))
            case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_MatrixNodeUtilities.expressionToAssignments(dest, IR_CompiletimeInversion.inverse(IR_MatrixNodeUtilities.accessToExpression(va), Knowledge.experimental_matrixStructure, Knowledge.experimental_blocksize))
            case _                                                     => Logger.error("argument of unexpected type: " + inMatrix.datatype)
          }
        case _             =>
          Logger.error("resolve inversion: type of inverse resolve " + Knowledge.experimental_resolveInverseFunctionCall + " not supported")
      }
  })
}

// Resolve standalone(no assign or decl to a matrix variable) matrix functions
// e.g. "function(matrix, ...)" or "Var v : Double = Function(matrix, ...)"
//TODO error checks
object IR_ResolveNoMatrixReturnOperations extends DefaultStrategy("Resolve standalone matrix functions") {
  this += new Transformation("no return value", {
    // compare two matrices or scalars
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, ListBuffer(left : IR_Expression, right : IR_Expression, precision : IR_Expression))) if (call.name == "compare") =>
      IR_GenerateBasicMatrixOperations.compare(left, right, precision)

    // set a slice to 'newValue'
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, _)) if (call.name == "setSlice") =>
    call.arguments match {
      case ListBuffer(matrix @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), offsetRows @ (IR_VariableAccess(_,IR_IntegerDatatype) | IR_IntegerConstant(_)), offsetCols @ (IR_VariableAccess(_,IR_IntegerDatatype) | IR_IntegerConstant(_)), nRows @ (IR_VariableAccess(_,IR_IntegerDatatype) | IR_IntegerConstant(_)), nCols @ (IR_VariableAccess(_,IR_IntegerDatatype) | IR_IntegerConstant(_)), newValue @ (IR_VariableAccess(_,IR_FloatDatatype | IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype) | IR_FloatConstant(_) | IR_IntegerConstant(_) | IR_DoubleConstant(_) | IR_RealConstant(_))) =>
      if (call.arguments.length != 6)
        Logger.error("wrong number of arguments: " + call.arguments.length + "expected setSlice(matrix,offsetRows,offsetColumns,numberOfRows,numberOfColumns,newValue)")

      IR_GenerateBasicMatrixOperations.setSubmatrix(matrix, offsetRows, offsetCols, nRows, nCols, newValue)
      case _ => Logger.error("unexpected arguments: " + call.arguments + ", expected setSlice(matrix,offsetRows,offsetColumns,numberOfRows,numberOfColumns,newValue)")
    }

    // set an element to 'newValue'
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, _)) if (call.name == "setElement")                                             =>
    call.arguments match {
      case ListBuffer(matrix @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), offsetRows @ (IR_VariableAccess(_,IR_IntegerDatatype) | IR_IntegerConstant(_)), offsetCols @ (IR_VariableAccess(_,IR_IntegerDatatype) | IR_IntegerConstant(_)), newValue @ (IR_VariableAccess(_,IR_FloatDatatype | IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype) | IR_FloatConstant(_) | IR_IntegerConstant(_) | IR_DoubleConstant(_) | IR_RealConstant(_))) =>
        IR_Assignment(IR_HighDimAccess(matrix, IR_ExpressionIndex(offsetRows, offsetCols)), newValue)
        case _ => Logger.error("unexpected arguments: " + call.arguments + ", expected setElement(matrix,offsetRows,offsetColumns,newValue)")
    }
  })

  this += new Transformation("scalar return value", {
    // determinant: calculate in runtime (LU) if input matrix is larger than 5, in compiletime(directly,laplace expansion) if 5 or smaller
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype), call @ IR_FunctionCall(_, _), _) if (call.name == "determinant")                                                                                                              =>
      if (call.arguments.length != 1)
        Logger.error("wrong number of arguments")
      var inMatrix = call.arguments(0)
      if (!inMatrix.isInstanceOf[IR_VariableAccess] || !inMatrix.datatype.isInstanceOf[IR_MatrixDatatype])
        Logger.error("unexpected argument type")
      if (IR_BasicMatrixOperations.getSize(inMatrix)._1 > 5)
        IR_GenerateBasicMatrixOperations.determinant(inMatrix.asInstanceOf[IR_VariableAccess], dest)
      else
        IR_Assignment(dest, IR_ResolveMatrixFunctions.calculateDeterminant(IR_MatrixNodeUtilities.accessToExpression(inMatrix.asInstanceOf[IR_VariableAccess])))
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype), call @ IR_FunctionCall(_, ListBuffer(matrix @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), row : IR_Expression, col : IR_Expression)), _) if (call.name == "getElement") =>
      if (call.arguments.length != 3)
        Logger.error("wrong number of arguments: " + call.arguments.length)
      //TODO runtime evaluated values as input?
      /*
      var row_asInt : Int = 0
      var col_asInt : Int = 0
      var allCompiletimeConstant = true
      try {
        row_asInt = IR_SimplifyExpression.evalIntegral(row).toInt
        col_asInt = IR_SimplifyExpression.evalIntegral(col).toInt
      } catch {
        case e : EvaluationException => allCompiletimeConstant = false
        case _ : Throwable => Logger.error("unexpected exception")
      }
      matrix match {
        case va @ IR_VariableAccess(_, IR_MatrixDatatype(_,_,_)) =>
        case _ => Logger.error("unexpected argument: " + matrix)
      }

       */
      IR_Assignment(dest, IR_HighDimAccess(matrix, IR_ExpressionIndex(row, col)))
  })

  /*
  //TODO eigenvalues: householder transformations  + QR decomposition
  this += new Transformation("tuple return value", {
  })

   */
}

// Resolve user defined functions
object IR_ResolveUserDefinedFunctions extends DefaultStrategy("Resolve user defined functions") {
  var resolveFunctions = ListBuffer[String]()
  this.onBefore = () => {
    resolveFunctions.clear()
    resolveFunctions ++= ListBuffer("dotProduct", "dot", "crossProduct", "cross", "det", "determinant", "getSlice", "setSlice", "getElement", "setElement", "transpose", "inverse")
  }

  this += new Transformation("add assignments/decl to function returns to arguments", {
    case IR_Assignment(dest, src : IR_FunctionCall, "=") if (!resolveFunctions.contains(src.name) && dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype])                   =>
      src.arguments += dest
      IR_ExpressionStatement(src)
    case IR_VariableDeclaration(datatype, name, Some(src : IR_FunctionCall), _) if (!resolveFunctions.contains(src.name) && datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      var decl = IR_VariableDeclaration(datatype, name, None)
      src.arguments += IR_VariableAccess(decl)
      ListBuffer[IR_Statement](
        decl,
        IR_ExpressionStatement(src)
      )
    case IR_Assignment(dest, src : IR_FunctionCall, "+=") if (!resolveFunctions.contains(src.name) && dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype])                  =>
      Logger.error("+= matrix operator resolution not yet implemented")
  })

  this += new Transformation("parameters and return types", {
    case arg : IR_FunctionArgument if (arg.datatype.isInstanceOf[IR_MatrixDatatype])                                    =>
      arg.datatype = IR_ReferenceDatatype(arg.datatype)
      arg
    case func : IR_Function if (!resolveFunctions.contains(func.name) && func.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      val matrix = func.datatype.asInstanceOf[IR_MatrixDatatype]
      func.parameters += IR_FunctionArgument("_matrix_return", IR_ReferenceDatatype(matrix))
      func.datatype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp)) if (exp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          List(
            IR_Assignment(IR_VariableAccess("_matrix_return", matrix), exp),
            IR_Return())
        }
        case _                                                                      => List(stmt)
      })
      func
  })

}

// Simplifications from IR_GeneralSimplify
object IR_SimplifyMatrices extends DefaultStrategy("Simplify matrices") {
  this += new Transformation("simplify", {
    case IR_Negative(m : IR_MatrixExpression) => m.expressions = m.expressions.map { y => IR_Negative(y) : IR_Expression }; m
    case m @ IR_MatrixExpression(_, 1, 1)     => m.get(0, 0)
    case m @ IR_MatrixDatatype(dt, 1, 1)      => dt
  })
}

// old strategies
object IR_ExtractMatrices extends DefaultStrategy("Extract and split matrix expressions where necessary") {
  val annotationFctCallCounter = "IR_ResolveMatrices.fctCallCounter"
  var fctCallCounter = 0
  // temporary variable used to replace function calls in expressions
  val annotationMatExpCounter = "IR_ResolveMatrices.matrixExpressionCounter"
  var matExpCounter = 0
  var resolveFunctions = ListBuffer[String]()
  var globalCollection : Option[IR_GlobalCollection] = None

  this.onBefore = () => {
    resolveFunctions.clear()
    resolveFunctions ++= ListBuffer("dotProduct", "dot", "crossProduct", "cross", "det", "transpose")
    if (Knowledge.experimental_resolveInverseFunctionCall != "Runtime") resolveFunctions += "inverse"
    globalCollection = StateManager.findFirst[IR_GlobalCollection]()
  }

  this += new Transformation("assignment of operation with self", {
    case stmt @ IR_Assignment(dest : IR_VariableAccess, src, _) if (dest.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      // resolve M = M * M into tmp = M * M; M = tmp
      var selfassign = false
      StateManager.findAll[IR_Multiplication](HelperNode(src)).foreach(mult =>
        if (mult.factors.exists(p => p.isInstanceOf[IR_VariableAccess] && p.asInstanceOf[IR_VariableAccess].name == dest.name))
          selfassign = true
      )

      if (selfassign) {
        var newStmts = ListBuffer[IR_Statement]()
        val decl = IR_VariableDeclaration(dest.datatype, "_matrixExp" + matExpCounter, src)
        newStmts += decl
        stmt.dest = IR_VariableAccess(decl)
        newStmts += stmt
        newStmts += IR_Assignment(dest, IR_VariableAccess(decl))
        matExpCounter += 1
        newStmts
      } else {
        stmt
      }
  })

  if (Knowledge.experimental_resolveInverseFunctionCall == "Runtime") {
    this += new Transformation("preparation", {
      case call @ IR_FunctionCall(_, args) if (call.name == "inverse") =>
        args(0).datatype match {
          case _ : IR_ScalarDatatype => IR_DoubleConstant(1.0) / args(0)

          case m : IR_MatrixDatatype =>
            args(0).datatype.asInstanceOf[IR_MatrixDatatype]
            if (m.sizeM > 1) {
              call.function = IR_PlainInternalFunctionReference("_runtimeInverseMatrix", m)
            }
            call
        }
    })
  }

  this += new Transformation("global declarations", {
    case decl @ IR_VariableDeclaration(_ : IR_MatrixDatatype, _, Some(exp : IR_Expression), _) =>
      StateManager.findFirst[IR_GlobalCollection]().get.initGlobals.asInstanceOf[IR_Function].body += IR_Assignment(IR_VariableAccess(Duplicate(decl)), exp)
      decl.initialValue = None
      decl
  }, applyAtNode = StateManager.findFirst[IR_GlobalCollection]())
  /*
  this += new Transformation("declarations", {
  //Definition of matrix variable including initialisation -> split into decl and assignment
  case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(exp : IR_Expression), _) =>
  val newStmts = ListBuffer[IR_Statement]()
  // split declaration and definition so each part can be handled by subsequent transformations
  newStmts += IR_VariableDeclaration(matrix, decl.name, None)
  newStmts += IR_Assignment(IR_VariableAccess(Duplicate(decl)), exp)
  newStmts
  })
  */

  this += new Transformation("extract function calls 1/3", {
    case stmt @ IR_Assignment(_, src, _) if (src.datatype.isInstanceOf[IR_MatrixDatatype])    =>
      // Extract all function calls into separate variables since any function could have unwanted side effects if called more than once
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_FunctionCall](src).filter(f => !resolveFunctions.contains(f.function.name)).foreach(exp => {
        val decl = IR_VariableDeclaration(exp.datatype, "_fct" + fctCallCounter + "_" + exp.name.replace('<', '_').replace('>', '_'), None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
        exp.annotate(annotationFctCallCounter, fctCallCounter)
        fctCallCounter += 1
      })
      // FIXME: only do the following if necessary
      //      StateManager.findAll[IR_MatrixExpression](src).foreach(exp => {
      //        val decl = IR_VariableDeclaration(exp.datatype, "_matrixExp" + matExpCounter, None)
      //        newStmts += decl
      //        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
      //        exp.annotate(annotationMatExpCounter, matExpCounter)
      //        matExpCounter += 1
      //      })
      newStmts += stmt
      newStmts
    case stmt @ IR_ExpressionStatement(src) if (src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      // Extract all function calls into separate variables since any function could have unwanted side effects if called more than once
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_FunctionCall](src).foreach(exp => { // resolveFunction check not needed: all internally resolved function return a value
        val decl = IR_VariableDeclaration(exp.datatype, "_fct" + fctCallCounter + "_" + exp.name.replace('<', '_').replace('>', '_'), None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
        exp.annotate(annotationFctCallCounter, fctCallCounter)
        fctCallCounter += 1
      })
      //      StateManager.findAll[IR_MatrixExpression](src).foreach(exp => {
      //        val decl = IR_VariableDeclaration(exp.datatype, "_matrixExp" + matExpCounter, None)
      //        newStmts += decl
      //        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
      //        exp.annotate(annotationMatExpCounter, matExpCounter)
      //        matExpCounter += 1
      //      })
      newStmts += stmt
      newStmts
  })

  this += new Transformation("extract function calls 3/3", {
    case exp : IR_FunctionCall if (exp.hasAnnotation(annotationFctCallCounter)) =>
      IR_VariableAccess("_fct" + exp.popAnnotationAs[Int](annotationFctCallCounter) + "_" + exp.function.name.replace('<', '_').replace('>', '_'), exp.function.returnType)

    case exp : IR_MatrixExpression if (exp.hasAnnotation(annotationMatExpCounter)) =>
      IR_VariableAccess("_matrixExp" + exp.popAnnotationAs[Int](annotationMatExpCounter), exp.datatype)
  })

  this += new Transformation("parameters and return types", {
    case arg : IR_FunctionArgument if (arg.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      arg.datatype = IR_ReferenceDatatype(arg.datatype)
      arg
    case func : IR_Function if (func.datatype.isInstanceOf[IR_MatrixDatatype])       =>
      val matrix = func.datatype.asInstanceOf[IR_MatrixDatatype]
      func.parameters += IR_FunctionArgument("_matrix_return", IR_ReferenceDatatype(matrix))
      func.datatype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp)) if (exp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          List(
            IR_Assignment(IR_VariableAccess("_matrix_return", matrix), exp),
            IR_Return())
        }
        case _                                                                      => List(stmt)
      })
      func
  })

  this += new Transformation("function call returns", {
    case IR_Assignment(dest, src : IR_FunctionCall, "=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype])  =>
      // FIXME resolve IR_Assignments with operator += before this
      src.arguments += dest
      IR_ExpressionStatement(src)
    case IR_Assignment(dest, src : IR_FunctionCall, "+=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      Logger.error("+= matrix operator resolution not yet implemented")
  })

  this += new Transformation("simplify function call arguments", {
    case stmt @ IR_ExpressionStatement(exp : IR_FunctionCall)                                               =>
      var newStmts = ListBuffer[IR_Statement]()
      exp.arguments.transform {
        case argexp : IR_MultiDimFieldAccess                                             => argexp
        case argexp : IR_VariableAccess                                                  => argexp
        case argexp : IR_Expression if (argexp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var decl = IR_VariableDeclaration(argexp.datatype, "_matrixExp" + matExpCounter, argexp)
          newStmts += decl
          IR_VariableAccess(decl)
        }
        case arg                                                                         => arg
      }
      newStmts += stmt
      newStmts
    case stmt @ IR_Assignment(_, exp : IR_FunctionCall, _) if !resolveFunctions.contains(exp.function.name) =>
      var newStmts = ListBuffer[IR_Statement]()

      exp.arguments.transform {
        case argexp : IR_MultiDimFieldAccess                                             => argexp
        case argexp : IR_VariableAccess                                                  => argexp
        case argexp : IR_Expression if (argexp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var decl = IR_VariableDeclaration(argexp.datatype, "_matrixExp" + matExpCounter, argexp)
          newStmts += decl
          IR_VariableAccess(decl)
        }
        case arg                                                                         => arg
      }
      newStmts += stmt
      newStmts
  })
}

object IR_ResolveMatrixFunctions extends DefaultStrategy("Resolve special matrix and vector functions") {
  val annotationMatrixRow = "IR_ResolveMatrices.matrixRow"
  val annotationMatrixCol = "IR_ResolveMatrices.matrixCol"

  def calculateDeterminant(m : IR_MatrixExpression) : IR_Expression = {
    if (m.rows != m.columns) {
      Logger.error("determinant for non-quadratic matrices not implemented")
      // FIXME Nullzeilen/-spalten ergaenzen
    }
    if (m.rows <= 0) {
      Logger.error("MatrixExpression of size <= 0")
    } else if (m.rows == 1) {
      return Duplicate(m.get(0, 0))
    } else if (m.rows == 2) {
      return Duplicate(m.get(0, 0) * m.get(1, 1) - m.get(0, 1) * m.get(1, 0))
    } else if (m.rows == 3) {
      return Duplicate(m.get(0, 0) * m.get(1, 1) * m.get(2, 2) +
        m.get(0, 1) * m.get(1, 2) * m.get(2, 0) +
        m.get(0, 2) * m.get(1, 0) * m.get(2, 1) -
        m.get(2, 0) * m.get(1, 1) * m.get(0, 2) -
        m.get(2, 1) * m.get(1, 2) * m.get(0, 0) -
        m.get(2, 2) * m.get(1, 0) * m.get(0, 1))
    } else {
      var det : IR_Expression = IR_IntegerConstant(0)
      val tmp = IR_MatrixExpression(Some(m.innerDatatype.getOrElse(IR_RealDatatype)), m.rows - 1, m.columns - 1)
      // laplace expansion
      for (i <- 0 until m.rows) {
        var tmpRow = 0
        for (row <- 0 until m.rows) {
          if (row != i) {
            for (col <- 1 until m.columns) {
              tmp.set(tmpRow, col - 1, Duplicate(m.get(row, col)))
            }
            tmpRow += 1
          }
        }
        val tmpDet = m.get(i, 0) * calculateDeterminant(tmp) * IR_DoubleConstant(math.pow(-1, i))
        det += IR_GeneralSimplifyWrapper.process[IR_Expression](tmpDet)
      }
      IR_GeneralSimplifyWrapper.process(det)
    }
  }

  def calculateMatrixOfMinorsElement(m : IR_MatrixExpression, forRow : Integer, forColumn : Integer) : IR_Expression = {
    if (m.rows != m.columns) {
      Logger.error("matrix of minors for non-quadratic matrices not implemented ")
    }
    val tmp = IR_MatrixExpression(Some(m.innerDatatype.getOrElse(IR_RealDatatype)), m.rows - 1, m.columns - 1)
    var tmpRow = 0
    for (row <- 0 until m.rows) {
      if (row != forRow) {
        var tmpCol = 0
        for (col <- 0 until m.columns) {
          if (col != forColumn) {
            tmp.set(tmpRow, tmpCol, m.get(row, col))
            tmpCol += 1
          }
        }
        tmpRow += 1
      }
    }
    return calculateDeterminant(tmp)
  }

  def findPivot(matrix : IR_MatrixExpression, startRow : Int) : Int = {
    var myMax = (0, 0.0)
    for (row <- startRow until matrix.rows) {
      val myElem = matrix.get(row, startRow)
      myElem match {
        case x : IR_IntegerConstant => if (Math.abs(x.value) > myMax._2) myMax = (row, Math.abs(x.v))
        case x : IR_RealConstant    => if (Math.abs(x.value) > myMax._2) myMax = (row, Math.abs(x.v))
        case x : IR_FloatConstant   => if (Math.abs(x.value) > myMax._2) myMax = (row, Math.abs(x.v))
        case _                      =>
      }
    }
    if (myMax._2 == 0.0) {
      // it's not constant 0 (or all the other stuff was 0 as well), so let's hope we gonna be fine...
      return startRow
    } else {
      return myMax._1
    }
  }

  def getElem(exp : IR_Expression, row : Integer, col : Integer) = {
    exp match {
      case x : IR_MatrixExpression                                           => x.get(row, col)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(row, col)))
      case _                                                                 => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
    }
  }

  def getSingleElem(exp : IR_Expression) = {
    exp match {
      case x : IR_MatrixExpression                                           => x.get(0, 0)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => x
      case _                                                                 => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
    }
  }

  this += new Transformation("resolution of built-in functions 2/2", {

    case call : IR_FunctionCall if (call.name == "dotProduct" || call.name == "dot") =>
      if (call.arguments.length != 2) {
        Logger.error(s"dotProduct() must have two arguments; has ${ call.arguments.length }")
      }

      val left = call.arguments(0)
      val right = call.arguments(1)

      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Dot product argument is of unexpected type ${ other.getClass.getTypeName }: $other")
      }
      val rsize = right match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Dot product argument is of unexpected type ${ other.getClass.getTypeName }: $other")
      }
      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for dotProduct() - attempting ${ lsize._1 }x${ lsize._2 } * ${ rsize._1 }x${ rsize._2 }")
      }

      var additions = ListBuffer[IR_Expression]()
      if (lsize._1 > 1 || lsize._2 > 1) {
        for (row <- 0 until lsize._1) {
          for (col <- 0 until lsize._2) {
            additions += IR_Multiplication(getElem(left, row, col), getElem(right, row, col))
          }
        }
      } else {
        additions += (getSingleElem(left), getSingleElem(right))
      }
      IR_Addition(additions)

    case call : IR_FunctionCall if call.name == "crossProduct" || call.name == "cross" =>
      if (call.arguments.length != 2) {
        Logger.error(s"Cross product must have two arguments; has ${ call.arguments.length }")
      }

      val left = call.arguments(0)
      val right = call.arguments(1)

      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Cross product argument is of unexpected type ${ other.getClass.getTypeName }: $other")
      }
      val rsize = right match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Cross product argument is of unexpected type ${ other.getClass.getTypeName }: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for dotProduct() - attempting ${ lsize._1 }x${ lsize._2 } * ${ rsize._1 }x${ rsize._2 }")
      }
      lsize._1 match {
        case 2     => getElem(left, 0, 0) * getElem(right, 1, 0) - getElem(left, 1, 0) * getElem(right, 0, 0)
        case 3     => ???
        case other => Logger.error(s"Cross product is not defined for dimensionality $other")
      }

    case call : IR_FunctionCall if (Knowledge.experimental_resolveInverseFunctionCall != "Runtime" && call.name == "inverse") =>
      if (call.arguments.length != 1) {
        Logger.error("inverse() must have one argument, but has " + call.arguments.length + " arguments.")
      }
      call.arguments(0) match {
        case s : IR_Expression if (s.datatype.isInstanceOf[IR_ScalarDatatype])  => 1 / s
        case s : IR_Expression if (s.datatype.isInstanceOf[IR_ComplexDatatype]) => 1 / s
        case m : IR_MatrixExpression                                            => m.inverse
        case _                                                                  => Logger.warn("Unable to handle inverse() argument: " + call.arguments(0)); call
      }

    case call : IR_FunctionCall if (call.name == "det") =>
      if (call.arguments.length != 1) {
        Logger.error("det() must have one argument")
      }
      val m = call.arguments(0).asInstanceOf[IR_MatrixExpression]
      calculateDeterminant(m)

    // FIXME shorten this code (less code duplication)
    case IR_ElementwiseMultiplication(left, right) =>
      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of unexpected type ${ other.getClass.getTypeName }: $other")
      }
      val rsize = right match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of unexpected type ${ other.getClass.getTypeName }: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for .* - attempting ${ lsize._1 }x${ lsize._2 } * ${ rsize._1 }x${ rsize._2 }")
      }
      val me = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          me.set(row, col, IR_Multiplication(getElem(left, row, col), getElem(right, row, col)))
        }
      }
      me

    case IR_ElementwiseDivision(left, right) =>
      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of unexpected type ${ other.getClass.getTypeName }: $other")
      }
      val rsize = right match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of unexpected type ${ other.getClass.getTypeName }: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for ./ - attempting ${ lsize._1 }x${ lsize._2 } * ${ rsize._1 }x${ rsize._2 }")
      }
      val me = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          me.set(row, col, IR_Division(getElem(left, row, col), getElem(right, row, col)))
        }
      }
      me

    case IR_ElementwiseModulo(left, right) =>
      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of unexpected type ${ other.getClass.getTypeName }: $other")
      }
      val rsize = right match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of unexpected type ${ other.getClass.getTypeName }: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for .% - attempting ${ lsize._1 }x${ lsize._2 } * ${ rsize._1 }x${ rsize._2 }")
      }
      val me = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          me.set(row, col, IR_Modulo(getElem(left, row, col), getElem(right, row, col)))
        }
      }
      me

    case IR_ElementwisePower(left, right) =>
      val lsize = left match {
        case me : IR_MatrixExpression                            => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other                                               => Logger.error(s"Element-wise operation argument is of unexpected type ${ other.getClass.getTypeName }: $other")
      }
      right match {
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => Logger.error(s"Element-wise operation argument is of unexpected type ${ va.getClass.getTypeName }: $va")
        case other                                               =>
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

    case IR_ExpressionStatement(call @ IR_FunctionCall(_, ListBuffer(left : IR_VariableAccess, right : IR_VariableAccess, precision : IR_VariableAccess))) if (call.name == "compare") =>
      IR_GenerateBasicMatrixOperations.compare(left, right, precision)
  })

  if (Knowledge.experimental_resolveInverseFunctionCall == "Runtime") {

    this += new Transformation("resolve runtime inversion", {
      case IR_ExpressionStatement(call @ IR_FunctionCall(_, ListBuffer(in : IR_VariableAccess, out : IR_VariableAccess))) if (call.name == "_runtimeInverseMatrix") =>
        IR_GenerateRuntimeInversion.inverse(in, Knowledge.experimental_matrixStructure, Knowledge.experimental_blocksize, out)
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

// objects containing matrix methods
object IR_BasicMatrixOperations {
  /*
    def getElem(exp : IR_Expression, i : Int, j : Int) = {
      exp match {
        case x : IR_MatrixExpression                                                 => x.get(i, j)
        case va : IR_VariableAccess if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => IR_HighDimAccess(va, IR_ExpressionIndex(i, j))
        case va : IR_VariableAccess if (va.datatype.isInstanceOf[IR_ScalarDatatype]) => va
        case n : IR_Number                                                           => n
        case _                                                                       => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
      }
    }
    */

  def getElem(exp : IR_Expression, pos : Int*) = {
    exp match {
      case x : IR_MatrixExpression                                                 =>
        if (pos.length != 2)
          Logger.error("position arguments of wrong form: " + pos)
        x.get(pos(0), pos(1))
      case va : IR_VariableAccess if (va.datatype.isInstanceOf[IR_MatrixDatatype]) =>
        if (pos.length != 2)
          Logger.error("position arguments of wrong form: " + pos)
        IR_HighDimAccess(va, IR_ExpressionIndex(pos(0), pos(1)))
      case va : IR_VariableAccess if (va.datatype.isInstanceOf[IR_ScalarDatatype]) => va
      case n : IR_Number                                                           => n
      case _                                                                       => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
    }
  }

  def duplicateExpressions(access : IR_Expression, dt : IR_MatrixDatatype) = {
    var expressions = ListBuffer[IR_Expression]()
    for (row <- 0 until dt.sizeM)
      for (col <- 0 until dt.sizeN)
        expressions += IR_HighDimAccess(Duplicate(access), IR_ConstIndex(row, col))
    expressions.toArray
  }

  def getSize(in : IR_Expression) = {
    in match {
      case me : IR_MatrixExpression                                                => (me.rows, me.columns)
      case va : IR_VariableAccess if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
      case other                                                                   => Logger.error("argument is of unexpected type: " + in.datatype)
    }
  }

  // copy and return a submatrix of 'from' of size 'n_rows' x 'n_cols' at position 'offset_rows', 'offset_cols'
  def copySubMatrix(from : IR_Expression, offset_rows : Int, offset_cols : Int, n_rows : Int, n_cols : Int) : IR_MatrixExpression = {
    if (offset_cols < 0 || offset_rows < 0) {
      Logger.error("negative offset")
    }
    var submatrix = IR_MatrixExpression(Some(from.datatype.resolveBaseDatatype), n_rows, n_cols)
    val bound_cols = offset_cols + n_cols
    val bound_rows = offset_rows + n_rows
    for (i <- offset_rows until bound_rows) {
      for (j <- offset_cols until bound_cols) {
        var n = Duplicate(getElem(from, i, j))
        submatrix.set(i - offset_rows, j - offset_cols, n)
      }
    }
    submatrix
  }

  // insert a matrix 'source' of size 'n_rows' x 'n_cols' at position 'offset_rows', 'offset_cols' in 'target'
  def pasteSubMatrix(source : IR_MatrixExpression, target : IR_MatrixExpression, offset_rows : Int, offset_cols : Int) : Unit = {
    if (offset_rows + source.rows > target.rows || offset_cols + source.columns > target.columns) {
      //Logger.error("IR_ResolveMatrixFunctions::pasteSubMatrix content does not fit into target")
    }
    if (offset_rows < 0 || offset_cols < 0) {
      Logger.error("negative offset")
    }
    val bound_cols = offset_cols + source.columns
    val bound_rows = offset_rows + source.rows
    for (i <- offset_rows until bound_rows) {
      for (j <- offset_cols until bound_cols) {
        //TODO move node objects instead of copying?
        var n = Duplicate(source.get(i - offset_rows, j - offset_cols))
        target.set(i, j, n)
      }
    }
  }

  // transpose a matrix passed by a variable
  def transpose(source : IR_VariableAccess) : IR_MatrixExpression = {
    var ssize = IR_BasicMatrixOperations.getSize(source)
    var out = IR_MatrixExpression(source.datatype.resolveBaseDatatype, ssize._2, ssize._1)
    for (i <- 0 until ssize._1) {
      for (j <- 0 until ssize._2) {
        out.set(j, i, Duplicate(IR_BasicMatrixOperations.getElem(source, i, j)))
      }
    }
    out
  }

  // multiply two vectors/matrices per dot product
  def dotProduct(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
    (left, right) match {
      case (l @ (IR_MatrixExpression(_, _, _) | IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), r @ (IR_MatrixExpression(_, _, _) | IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)))) =>
        var lsize = getSize(l)
        var rsize = getSize(r)
        (lsize, rsize) match {
          case ((1, lcols), (rrows, 1)) if (lcols == rrows)                           =>
            var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), 1, 1)
            out.set(0, 0, IR_IntegerConstant(0))
            for (i <- 0 until rrows) {
              out.set(0, 0, IR_Addition(Duplicate(out.get(0, 0)), IR_Multiplication(getElem(l, 0, i), getElem(r, i, 0))))
            }
            out
          case ((lrows, lcols), (rrows, rcols)) if (lcols == rcols && lrows == rrows) =>
            var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), 1, 1)
            out.set(0, 0, IR_IntegerConstant(0))
            for (i <- 0 until rrows) {
              for (j <- 0 until rcols) {
                out.set(0, 0, IR_Addition(Duplicate(out.get(0, 0)), IR_Multiplication(getElem(l, i, j), getElem(r, i, j))))
              }
            }
            out
          case _                                                                      => Logger.error("unsupported argument form: " + lsize + ", " + rsize + ", expected arguments of the same size")
        }
      case _                                                                                                                                                                              => Logger.error("unexpected argument types: " + left + ", " + right + ", expected matrix variables or expressions as input")
    }

  }

  // multiply two columnvectors of size 3 per crossProduct
  def crossProduct(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
    (left, right) match {
      case (l @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) | IR_MatrixExpression(_, _, _)), r @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) | IR_MatrixExpression(_, _, _))) =>
        var lsize = getSize(left)
        var rsize = getSize(right)
        if (lsize._1 != 3 || lsize._2 != 1 || rsize._1 != 3 || rsize._2 != 1)
          Logger.error("cross product only supported for two column vectors of size 3, arguments are of form: " + lsize + "," + rsize)
        var out = IR_MatrixExpression(IR_ResultingDatatype(l.datatype, r.datatype), 3, 1)
        out.set(0, 0, IR_Subtraction(IR_Multiplication(getElem(left, 1, 0), getElem(right, 2, 0)), IR_Multiplication(getElem(left, 2, 0), getElem(right, 1, 0))))
        out.set(1, 0, IR_Subtraction(IR_Multiplication(getElem(left, 2, 0), getElem(right, 0, 0)), IR_Multiplication(getElem(left, 0, 0), getElem(right, 2, 0))))
        out.set(2, 0, IR_Subtraction(IR_Multiplication(getElem(left, 0, 0), getElem(right, 1, 0)), IR_Multiplication(getElem(left, 1, 0), getElem(right, 0, 0))))
        out
      case _                                                                                                                                                                              => Logger.error("unexpected arguments: " + left + ", " + right + ", expected accesses to matrix variables or matrix expressions")
    }
  }

  // multiplicate either two IR_MatrixExpressions like in compiletime inversions or multiple factors as a IR_Multiplication
  def mult(left : IR_MatrixExpression, right : IR_MatrixExpression) = {
   var lsize = getSize(left)
   var rsize = getSize(right)
   if(lsize._2 != rsize._1)
    Logger.error("sizes do not match: " + lsize + " vs " + rsize)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, rsize._2)
        for (i <- 0 until lsize._1) {
          for (j <- 0 until rsize._2) {
            var tmp = IR_Addition(IR_IntegerConstant(0))
            for (k <- 0 until rsize._1) {
              tmp = IR_Addition(tmp, IR_Multiplication(Duplicate(left.get(i, k)), Duplicate(right.get(k, j))))
            }
           out.set(i,j,Duplicate(tmp))
          }
        }
        out
    }

  def mult(mult : IR_Multiplication) : IR_MatrixExpression = {
    var result = IR_MatrixExpression(IR_IntegerDatatype,1,1)
    var tmp = mult.factors(0) match {
      case va @ IR_VariableAccess(_,IR_MatrixDatatype(_,_,_)) =>
        IR_MatrixNodeUtilities.accessToExpression(va)
      case x : IR_MatrixExpression =>
        Duplicate(x)
      case _ =>
        Logger.error("unexpected type: " + mult.factors(0))
    }
    for(f <- 1 until mult.factors.length) {
      result = mult.factors(f) match {
        case va @ IR_VariableAccess(_,IR_MatrixDatatype(_,_,_)) =>
          IR_BasicMatrixOperations.mult(tmp,IR_MatrixNodeUtilities.accessToExpression(va))
        case x : IR_MatrixExpression =>
          IR_BasicMatrixOperations.mult(tmp, x)
        case _ =>
          Logger.error("unexpected type: " + mult.factors(f))
      }
      tmp = Duplicate(result)
    }
    result
  }


  // add two IR_MatrixExpressions, internal add function to use in e.g. compiletime inversions
  def add(left : IR_MatrixExpression, right : IR_MatrixExpression) = {
    var lsize = getSize(left)
    var rsize = getSize(right)
    if(lsize != rsize)
      Logger.error("sizes do not match: " + lsize + " vs " + rsize)
    var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
    for (i <- 0 until lsize._1) {
      for (j <- 0 until rsize._2) {
        out.set(i,j,Duplicate(IR_Addition(left.get(i,j), right.get(i,j))))
      }
    }
    out
  }

  // add multiple matrices(summands of a IR_Addition) add two matrices of a IR_ElementwiseAddition
  def add(addition : IR_Expression) : IR_MatrixExpression = {
    addition match {
      case a : IR_Addition =>
    var size = (0, 0)
        a.summands.foreach(x => if (x.datatype.isInstanceOf[IR_MatrixDatatype]) size = getSize(x))
        if (size == (0, 0))
          Logger.error("no matrix in summands")
        var datatype = a.summands(0).datatype
        a.summands.foreach(x => datatype = IR_ResultingDatatype.apply(datatype, x.datatype))
        var out = IR_MatrixExpression(datatype, size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_IntegerConstant(0))
          }
        }
        for (k <- 0 until a.summands.length) {
          for (i <- 0 until size._1) {
            for (j <- 0 until size._2) {
              out.set(i, j, IR_Addition(Duplicate(out.get(i, j)), getElem(a.summands(k), i, j)))
            }
          }
        }
        out
      case eaddition @ IR_ElementwiseAddition(_, _) =>
        Logger.error("elementwise addition not yet supported")
      case _                                        => Logger.error("unexpected type: " + addition + ", expected IR_Addition or IR_ElementwiseAddition")
    }

  }

  //Internal subtraction: sub two IR_MatrixExpressions, internal sub function to use in e.g compiletime inversion
  def sub(left : IR_MatrixExpression, right : IR_MatrixExpression): IR_MatrixExpression = {
  var lsize = getSize(left)
  var rsize = getSize(right)
  if(lsize != rsize)
    Logger.error("sizes do not match: " + lsize + " vs " + rsize)
  var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
  for (i <- 0 until lsize._1) {
    for (j <- 0 until rsize._2) {
      out.set(i,j,Duplicate(IR_Subtraction(left.get(i,j), right.get(i,j))))
    }
  }
  out
}

  //User subtraction: subtract two operands in a IR_Subtraction or IR_ElementwiseSubtraction if one of them is a matrix
  def sub(subtraction : IR_Expression) : IR_MatrixExpression = {
    subtraction match {
      case sub : IR_Subtraction             =>
        sub.left match {
          case matrix @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) | IR_MatrixExpression(_, _, _))                   =>
            var size = getSize(matrix)
            var out = IR_MatrixExpression(matrix.datatype.resolveBaseDatatype, size._1, size._2)
            for (i <- 0 until size._1) {
              for (j <- 0 until size._2) {
                out.set(i, j, IR_Subtraction(getElem(sub.left, i, j), getElem(sub.right, i, j)))
              }
            }
            out
          case scalar @ (IR_VariableAccess(_, IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype | IR_FloatDatatype)) =>
            if(sub.right.datatype.isInstanceOf[IR_MatrixDatatype])
             IR_BasicMatrixOperations.sub(IR_Subtraction(sub.right, sub.left))
            else
              Logger.error("no matrices as operands: " + sub.left + ", " + sub.right)
          case scalar : IR_Number                                                                                           =>
            if(sub.right.datatype.isInstanceOf[IR_MatrixDatatype])
              IR_BasicMatrixOperations.sub(IR_Subtraction(sub.right, sub.left))
            else
              Logger.error("no matrices as operands: " + sub.left + ", " + sub.right)
          case _ => Logger.error("unexpected argument: " + sub.left + ", expected matrix or scalar")
        }
      case esub : IR_ElementwiseSubtraction =>
      Logger.error("IR_ElementwiseSubtraction not yet supported")
      case _ =>
        Logger.error("unexpected argument: " + subtraction + ", expected IR_Subtraction or IR_ElementwiseSubtraction")
    }
  }

  // multiplicate two matrices or a scalar and a matrix per element
  def elementwiseMultiplication(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
    (left, right) match {
      // scalar x matrix, matrix x scalar, matrix x matrix
      case (scalar @ (IR_VariableAccess(_, IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype)), matrix @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)))) =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Multiplication(getElem(matrix, i, j), getElem(scalar, i, j)))
          }
        }
        out
      case (matrix @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), scalar @ (IR_VariableAccess(_, IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype))) =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Multiplication(getElem(scalar, i, j), getElem(matrix, i, j)))
          }
        }
        out
      case (matrixLeft @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), matrixRight @ ((IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)))))                     =>
        var size = getSize(matrixLeft)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Multiplication(getElem(matrixLeft, i, j), getElem(matrixRight, i, j)))
          }
        }
        out
      case _                                                                                                                                                         => Logger.error("unexpected argument combination")
    }
  }

  // divide two matrices or a scalar and a matrix per element
  def elementwiseDivision(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
    (left, right) match {
      // scalar x matrix, matrix x scalar, matrix x matrix
      case (scalar @ (IR_VariableAccess(_, IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype)), matrix @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)))) =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Division(getElem(scalar, i, j), getElem(matrix, i, j)))
          }
        }
        out
      case (matrix @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), scalar @ (IR_VariableAccess(_, IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype))) =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Division(getElem(matrix, i, j), getElem(scalar, i, j)))
          }
        }
        out
      case (matrixLeft @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), matrixRight @ ((IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)))))                     =>
        var size = getSize(matrixLeft)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Division(getElem(matrixLeft, i, j), getElem(matrixRight, i, j)))
          }
        }
        out
      case _                                                                                                                                                         => Logger.error("unexpected argument combination")
    }
  }

  //
  def elementwisePower(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
    (left, right) match {
      // scalar x matrix, matrix x scalar
      case (scalar @ (IR_VariableAccess(_, IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype)), matrix @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)))) =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Power(getElem(scalar, i, j), getElem(matrix, i, j)))
          }
        }
        out
      case (matrix @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), scalar @ (IR_VariableAccess(_, IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype))) =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Power(getElem(matrix, i, j), getElem(scalar, i, j)))
          }
        }
        out
      case _                                                                                                                                                         => Logger.error("unexpected argument combination")
    }
  }

  // return a matrix with negative elements of input
  def negative(that : IR_MatrixExpression) : IR_MatrixExpression = {
    var out = IR_MatrixExpression(that.innerDatatype, that.rows, that.columns)
    for (i <- 0 until that.rows) {
      for (j <- 0 until that.columns) {
        //        out.set(i,j,IR_Subtraction(IR_RealConstant(0),Duplicate(that.get(i,j))))
        out.set(i, j, IR_Negative(Duplicate(that.get(i, j))))
      }
    }
    out
  }

  // return the sum of the diagonal elements of a matrix as 1x1 matrix
  def trace(matrix : IR_Expression) : IR_Addition = {
    var sum = IR_Addition(IR_IntegerConstant(0))
    matrix match {
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        var s = getSize(va)
        if (s._1 != s._2)
          Logger.error("trace only for quadratic matrices supported, matrix is of form: " + s)
        for (i <- 0 until s._1) {
          sum = IR_Addition(sum, getElem(va, i, i))
        }
      case x @ IR_MatrixExpression(_, _, _)                      =>
        if (x.rows != x.columns)
          Logger.error("trace only for quadratic matrices supported, matrix is of form: " + (x.rows, x.columns))
        for (i <- 0 until x.rows) {
          sum = IR_Addition(sum, getElem(x, i, i))
        }
    }
    sum
  }
}

object IR_CompiletimeInversion {
  // head function that branches to specific inversions
  def inverse(that : IR_MatrixExpression, matrixStructure : String, blocksize : Int) : IR_MatrixExpression = {
    if (that.rows != that.columns)
      Logger.error("inversion of non quadratic matrices not supported.")
    if (blocksize < 1)
      Logger.error("blocksize < 1")
    matrixStructure match {
      case "Diagonal"
      => {
        val tmp = Duplicate(that)
        for (row <- 0 until that.rows) {
          //Logger.error("IR_MatrixAccess::inverse: Diagonal element is 0!")
          tmp.set(row, row, IR_Division(IR_RealConstant(1.0), that.get(row, row)))
        }
        tmp
      }
      case "Blockdiagonal"
      => {
        if (that.rows < 4)
          Logger.error("Blockdiagonal inversion not applicable for matrices < 4, use diagonal")
        var out = Duplicate(that)
        if (blocksize < 1) {
          Logger.error("Blocksize must be at least 1")
        }
        else {
          val n_blocks = that.rows / blocksize
          if (that.rows % blocksize != 0) {
            Logger.error("Rows are not a multiple of blocksize")
          }
          else {
            for (block <- 0 until n_blocks) {
              val offset = block * blocksize

              // extract matrix block
              var subMatrix = IR_BasicMatrixOperations.copySubMatrix(that, offset, offset, blocksize, blocksize)

              // invert with GaussJordan method
              var subMatrix_inv = gaussJordanInverse(subMatrix)

              // copy to out matrix
              IR_BasicMatrixOperations.pasteSubMatrix(subMatrix_inv, out, offset, offset)
            }
          }
        }
        out
      }
      case "Schur"
      => {
        if (that.rows < 4)
          Logger.error("Schur inversion not applicable for matrices < 4")
        //var out = IR_MatrixExpression(that.datatype.datatype, that.rows, that.columns)
        var out = Duplicate(that)
        /* use an invert algorithm using the schur complement

          -1             -1
         M  =  (  A  B  )      =    ( A_inv + A_inv*B*S_inv*C*A_inv -A_inv*B*S_inv  )
               (  C  D  )           (           -S_inv*C*A_inv           S_inv      )

          with M of size (n + m) x (n + m) and S = D - C * A_inv * B
        */
        val n = blocksize
        val m = that.rows - n

        if (n < 1) {
          Logger.error("IR_MatrixAccess::inverse n < 1!")
        }
        else {
          if ((that.rows - 1) % n != 0) {
            Logger.error("IR_MatrixAccess::inverse Rows of A are not a multiple of n! rows = " + that.rows + ", n = " + n)
          }
          else {
            var blocksize_A = Knowledge.experimental_blocksize_A

            // extract and invert A: Blockdiagonalmatrix assumed
            var A = IR_BasicMatrixOperations.copySubMatrix(that, 0, 0, n, n)
            var A_inv = inverse(A, "Blockdiagonal", blocksize_A)
            IR_GeneralSimplify.doUntilDoneStandalone(A_inv)

            // calculate S
            val B = IR_BasicMatrixOperations.copySubMatrix(that, 0, n, n, m)
            val C = IR_BasicMatrixOperations.copySubMatrix(that, n, 0, m, n)
            val D = IR_BasicMatrixOperations.copySubMatrix(that, n, n, m, m)
            val CA_inv = IR_BasicMatrixOperations.mult(C, A_inv)
            val CA_invB = IR_BasicMatrixOperations.mult(CA_inv, B)
            val S = IR_BasicMatrixOperations.sub(D, CA_invB)

            // invert S
            // for schur complement inversion multiple structure information is necessary(n and m, blocksize of A, S is probably always filled) in case  m is larger than 1 (default should be "Filled")
            val S_inv = inverse(S, "Filled", blocksize_A)

            // copy result blocks to 'out' matrix
            val lowerLeft = IR_BasicMatrixOperations.negative(IR_BasicMatrixOperations.mult(S_inv, CA_inv))
            val lowerRight = S_inv
            val A_invB = IR_BasicMatrixOperations.mult(A_inv, B)
            val A_invBS_inv = IR_BasicMatrixOperations.mult(A_invB, S_inv)
            val upperRight = IR_BasicMatrixOperations.negative(A_invBS_inv)
            val upperLeft = IR_BasicMatrixOperations.add(A_inv, IR_BasicMatrixOperations.mult(A_invBS_inv, CA_inv))
            IR_BasicMatrixOperations.pasteSubMatrix(upperLeft, out, 0, 0)
            IR_BasicMatrixOperations.pasteSubMatrix(upperRight, out, 0, n)
            IR_BasicMatrixOperations.pasteSubMatrix(lowerLeft, out, n, 0)
            IR_BasicMatrixOperations.pasteSubMatrix(lowerRight, out, n, n)
          }
        }
        out
      }
      case "Cofactors"
      => {
        val inv_det = IR_IntegerConstant(1) / IR_ResolveMatrixFunctions.calculateDeterminant(that)
        val tmp = IR_MatrixExpression(Some(that.innerDatatype.getOrElse(IR_RealDatatype)), that.rows, that.columns)
        for (row <- 0 until that.rows) {
          for (col <- 0 until that.columns) {
            tmp.set(col, row, IR_ResolveMatrixFunctions.calculateMatrixOfMinorsElement(that, row, col) * IR_DoubleConstant(math.pow(-1, row + col)) * inv_det)
          }
        }
        tmp
      }
      case "GaussJordan"
      => {
        var tmp = gaussJordanInverse(that)
        tmp
      }
      case "Filled"
      => {
        that.rows match {
          case 1 =>
            IR_MatrixExpression(that.innerDatatype, 1, 1, Array(IR_Division(IR_RealConstant(1.0), that.get(0, 0))))

          case 2 =>
            val a = that.get(0, 0)
            val b = that.get(0, 1)
            val c = that.get(1, 0)
            val d = that.get(1, 1)
            val det : IR_Expression = IR_Division(IR_RealConstant(1.0), (a * d) - (b * c))
            IR_MatrixExpression(that.innerDatatype, 2, 2, Array(Duplicate(det) * Duplicate(d), Duplicate(det) * Duplicate(b) * IR_IntegerConstant(-1), Duplicate(det) * Duplicate(c) * IR_IntegerConstant(-1), Duplicate(det) * Duplicate(a)))

          case 3 =>
            val a = that.get(0, 0)
            val b = that.get(0, 1)
            val c = that.get(0, 2)
            val d = that.get(1, 0)
            val e = that.get(1, 1)
            val f = that.get(1, 2)
            val g = that.get(2, 0)
            val h = that.get(2, 1)
            val i = that.get(2, 2)
            val A = Duplicate(e) * Duplicate(i) - Duplicate(f) * Duplicate(h)
            val B = IR_IntegerConstant(-1) * (Duplicate(d) * Duplicate(i) - Duplicate(f) * Duplicate(g))
            val C = Duplicate(d) * Duplicate(h) - Duplicate(e) * Duplicate(g)
            val D = IR_IntegerConstant(-1) * (Duplicate(b) * Duplicate(i) - Duplicate(c) * Duplicate(h))
            val E = Duplicate(a) * Duplicate(i) - Duplicate(c) * Duplicate(g)
            val F = IR_IntegerConstant(-1) * (Duplicate(a) * Duplicate(h) - Duplicate(b) * Duplicate(g))
            val G = Duplicate(b) * Duplicate(f) - Duplicate(c) * Duplicate(e)
            val H = IR_IntegerConstant(-1) * (Duplicate(a) * Duplicate(f) - Duplicate(c) * Duplicate(d))
            val I = Duplicate(a) * Duplicate(e) - Duplicate(b) * Duplicate(d)
            val det = Duplicate(a) * A + Duplicate(b) * B + Duplicate(c) * C
            IR_MatrixExpression(that.innerDatatype, 3, 3, Array(Duplicate(A) / Duplicate(det), Duplicate(D) / Duplicate(det), Duplicate(G) / Duplicate(det), Duplicate(B) / Duplicate(det), Duplicate(E) / Duplicate(det), Duplicate(H) / Duplicate(det), Duplicate(C) / Duplicate(det), Duplicate(F) / Duplicate(det), Duplicate(I) / Duplicate(det)))
          case _ =>
            gaussJordanInverse(that)
        }
      }

      case "Runtime"
             => Logger.error("'Runtime' matrix inversion chosen but in code path for compile time")
      case _ => Logger.error(s"""Unknown matrix inversion resolution strategy "${ matrixStructure }"""")
    }
  }

  def gaussJordanInverse(that : IR_MatrixExpression) : IR_MatrixExpression = {
    val matrix = Duplicate(that)
    val other = IR_MatrixExpression(matrix.datatype, matrix.rows, matrix.columns)
    for (i <- 0 until other.rows) {
      for (j <- 0 until other.columns) {
        if (i == j) other.set(i, j, 1.0);
        else other.set(i, j, 0.0)
      }
    }

    for (i <- matrix.rows - 1 to 0) {
      var swap = false
      val topValue = matrix.get(i - 1, i)
      val currentValue = matrix.get(i, i)
      (topValue, currentValue) match {
        case (top : IR_Number, current : IR_Number) => swap = Math.abs(top.value.asInstanceOf[Number].doubleValue) > Math.abs(current.value.asInstanceOf[Number].doubleValue)
        case _                                      =>
      }

      if (swap) {
        for (j <- 0 until matrix.columns) {
          var d = matrix.get(i, j)
          matrix.set(i, j, matrix.get(i - 1, j))
          matrix.set(i - 1, j, d)
          d = other.get(i, j)
          other.set(i, j, other.get(i - 1, j))
          other.set(i - 1, j, d)
        }
      }
    }

    for (i <- 0 until matrix.rows) {
      for (j <- 0 until matrix.rows) {
        if (j != i) {
          val d = matrix.get(j, i) / matrix.get(i, i)
          for (k <- 0 until matrix.rows) {
            var newExp = matrix.get(j, k) - Duplicate(matrix.get(i, k)) * Duplicate(d)
            matrix.set(j, k, newExp)

            newExp = other.get(j, k) - Duplicate(other.get(i, k)) * Duplicate(d)
            other.set(j, k, newExp)
          }
        }
      }
    }

    IR_GeneralSimplify.doUntilDoneStandalone(matrix)

    for (i <- 0 until matrix.rows) {
      val d = matrix.get(i, i)
      for (j <- 0 until matrix.rows) {
        val newExp = other.get(i, j) / Duplicate(d)
        other.set(i, j, newExp)
      }
    }
    other
  }
}

object IR_MatrixNodeUtilities {
  // split a declaration with init to declaration and assignment with init
  def splitDeclaration(decl : IR_VariableDeclaration) : ListBuffer[IR_Statement] = {
    val newStmts = ListBuffer[IR_Statement]()
    newStmts += IR_VariableDeclaration(decl.datatype, decl.name, None)
    newStmts += IR_Assignment(IR_VariableAccess(Duplicate(decl)), decl.initialValue.getOrElse(IR_NullExpression))
    newStmts
  }

  // convert an assignment of a IR_MatrixExpression to multiple Assignments for all positions in dest/src; dest and src have to be of the same form
  def expressionToAssignments(dest : IR_VariableAccess, src : IR_MatrixExpression) : IR_Scope = {
    var destSize = IR_BasicMatrixOperations.getSize(dest)
    if (destSize != (src.rows, src.columns))
      Logger.error("sizes do not match: " + destSize + " vs " + (src.rows, src.columns))
    var stmts = ListBuffer[IR_Statement]()
    for (i <- 0 until src.rows) {
      for (j <- 0 until src.columns) {
        stmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(i, j)), src.get(i, j))
      }
    }
    IR_Scope(stmts)
  }

  // copy a matrix from a IR_VariableAccess to a IR_MatrixExpression
  def accessToExpression(src : IR_VariableAccess) : IR_MatrixExpression = {
    var size = IR_BasicMatrixOperations.getSize(src)
    var out = IR_MatrixExpression(src.datatype.resolveBaseDatatype, size._1, size._2)
    for (i <- 0 until size._1) {
      for (j <- 0 until size._2) {
        out.set(i, j, IR_HighDimAccess(src, IR_ExpressionIndex(i, j)))
      }
    }
    out
  }

}

object IR_GenerateBasicMatrixOperations {

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
        func.body += IR_VariableDeclaration(_i)
        func.body += IR_VariableDeclaration(_j)
        func.body += IR_ForLoop(IR_Assignment(_i, 0), IR_Lower(_i, sizeMLeft), IR_PreIncrement(_i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_Assignment(_j, 0), IR_Lower(_j, sizeNLeft), IR_PreIncrement(_j), ListBuffer[IR_Statement](
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
    func.body += IR_VariableDeclaration(i)
    func.body += IR_VariableDeclaration(j)
    func.body += IR_ForLoop(IR_Assignment(i, 0), IR_Lower(i, N), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(j, 0), IR_Lower(j, M), IR_PreIncrement(j), ListBuffer[IR_Statement](
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
    func.body += IR_VariableDeclaration(i)
    func.body += IR_VariableDeclaration(j)
    func.body += IR_ForLoop(IR_Assignment(i, 0), IR_Lower(i, N), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(j, 0), IR_Lower(j, M), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i, j)), IR_Multiplication(scalar, IR_HighDimAccess(in, IR_ExpressionIndex(i, j))))
      ))
    ))
    func
  }

  // multiply 'left' and 'right' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def multAtSubmatrix(left : IR_VariableAccess, right : IR_VariableAccess, out : IR_VariableAccess, offset_r : Int, offset_c : Int) : IR_Scope = {
    var func = IR_Scope(Nil)
    val leftDt = left.datatype.asInstanceOf[IR_MatrixDatatype]
    val rightDt = right.datatype.asInstanceOf[IR_MatrixDatatype]
    val outDt = out.datatype.asInstanceOf[IR_MatrixDatatype]
    if (leftDt.sizeN != rightDt.sizeM)
      Logger.error("dimensions do not match!")
    val M = leftDt.sizeM
    val K = leftDt.sizeN
    var N = rightDt.sizeN
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    var _k = IR_VariableAccess("_k", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(_i)
    func.body += IR_VariableDeclaration(_j)
    func.body += IR_VariableDeclaration(_k)
    func.body += IR_ForLoop(IR_Assignment(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), 0),
        IR_ForLoop(IR_Assignment(_k, 0), IR_Lower(_k, K), IR_PreIncrement(_k), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Addition(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Multiplication(IR_HighDimAccess(left, IR_ExpressionIndex(_i, _k)), IR_HighDimAccess(right, IR_ExpressionIndex(_k, _j)))))
        ))
      ))
    ))
    func
  }

  // add 'left' and 'right' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def addAtSubmatrix(left : IR_VariableAccess, right : IR_VariableAccess, out : IR_VariableAccess, offset_r : Int, offset_c : Int) : IR_Scope = {
    var func = IR_Scope(Nil)
    val leftDt = left.datatype.asInstanceOf[IR_MatrixDatatype]
    val rightDt = right.datatype.asInstanceOf[IR_MatrixDatatype]
    val outDt = out.datatype.asInstanceOf[IR_MatrixDatatype]
    if (leftDt.sizeM != rightDt.sizeM || leftDt.sizeN != rightDt.sizeN)
      Logger.error("dimensions do not match!")
    val M = leftDt.sizeM
    var N = leftDt.sizeN
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(_i)
    func.body += IR_VariableDeclaration(_j)
    func.body += IR_ForLoop(IR_Assignment(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Addition(IR_HighDimAccess(left, IR_ExpressionIndex(_i, _j)), IR_HighDimAccess(right, IR_ExpressionIndex(_i, _j))))
      ))
    ))
    func
  }

  // subtract 'left' from 'right' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def subAtSubmatrix(left : IR_VariableAccess, right : IR_VariableAccess, out : IR_VariableAccess, offset_r : Int, offset_c : Int) : IR_Scope = {
    var func = IR_Scope(Nil)
    val leftDt = left.datatype.asInstanceOf[IR_MatrixDatatype]
    val rightDt = right.datatype.asInstanceOf[IR_MatrixDatatype]
    val outDt = out.datatype.asInstanceOf[IR_MatrixDatatype]
    if (leftDt.sizeM != rightDt.sizeM || leftDt.sizeN != rightDt.sizeN)
      Logger.error("dimensions do not match!")
    val M = leftDt.sizeM
    var N = leftDt.sizeN
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(_i)
    func.body += IR_VariableDeclaration(_j)
    func.body += IR_ForLoop(IR_Assignment(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Subtraction(IR_HighDimAccess(left, IR_ExpressionIndex(_i, _j)), IR_HighDimAccess(right, IR_ExpressionIndex(_i, _j))))
      ))
    ))
    func
  }

  // produce negative of 'that' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def negAtSubmatrix(that : IR_VariableAccess, out : IR_VariableAccess, offset_r : Int, offset_c : Int) : IR_Scope = {
    var func = IR_Scope(Nil)
    val thatDt = that.datatype.asInstanceOf[IR_MatrixDatatype]
    val outDt = out.datatype.asInstanceOf[IR_MatrixDatatype]
    val M = thatDt.sizeM
    var N = thatDt.sizeN
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(_i)
    func.body += IR_VariableDeclaration(_j)
    func.body += IR_ForLoop(IR_Assignment(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Negative(IR_HighDimAccess(that, IR_ExpressionIndex(_i, _j))))
      ))
    ))
    func
  }

  // copy a submatrix of n_rows x n_cols to 'copy' from position 'offset_r', 'offset_c' in 'source'
  def copySubmatrix(source : IR_VariableAccess, dest : IR_VariableAccess, offset_r : IR_Expression, offset_c : IR_Expression, n_rows : IR_Expression, n_cols : IR_Expression) : IR_Scope = {
    var stmts = IR_Scope(Nil)
    val sourceDt = source.datatype.asInstanceOf[IR_MatrixDatatype]
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    stmts.body += IR_VariableDeclaration(i)
    stmts.body += IR_VariableDeclaration(j)
    stmts.body += IR_ForLoop(IR_Assignment(i, offset_r), IR_Lower(i, n_rows + offset_r), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(j, offset_c), IR_Lower(j, offset_c + n_cols), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(i - offset_r, j - offset_c)), IR_HighDimAccess(source, IR_ExpressionIndex(i, j)))
      ))
    ))
    stmts
  }

  // write a submatrix 'source' of n_rows x n_cols to 'destination' at position 'offset_r', 'offset_c'
  def pasteSubmatrix(source : IR_VariableAccess, destination : IR_VariableAccess, offset_r : IR_VariableAccess, offset_c : IR_VariableAccess) : IR_Scope = {
    var stmts = IR_Scope(Nil)
    val sourceDt = source.datatype.asInstanceOf[IR_MatrixDatatype]
    val n_rows = sourceDt.sizeM
    val n_cols = sourceDt.sizeN
    val destDt = destination.datatype.asInstanceOf[IR_MatrixDatatype]
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    stmts.body += IR_VariableDeclaration(i)
    stmts.body += IR_VariableDeclaration(j)
    stmts.body += IR_ForLoop(IR_Assignment(i, offset_r), IR_Lower(i, n_rows + offset_r), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(j, offset_c), IR_Lower(j, offset_c + n_cols), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(destination, IR_ExpressionIndex(i, j)), IR_HighDimAccess(source, IR_ExpressionIndex(i - offset_r, j - offset_c)))
      ))
    ))
    stmts
  }

  // write 'newVal' to all positions in 'n_rows' x 'n_cols' at position 'offset_r', 'offset_c' in 'matrix'
  def setSubmatrix(matrix : IR_VariableAccess, offsetRows : IR_Expression, offsetCols : IR_Expression, nRows : IR_Expression, nCols : IR_Expression, newValue : IR_Expression) : IR_Scope = {

    var stmts = IR_Scope(Nil)
    val sourceDt = matrix.datatype.asInstanceOf[IR_MatrixDatatype]
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    var stream = IR_VariableAccess("std::cout", IR_StringDatatype)
    stmts.body += IR_VariableDeclaration(i)
    stmts.body += IR_VariableDeclaration(j)
    stmts.body += IR_ForLoop(IR_Assignment(i, offsetRows), IR_Lower(i, nRows + offsetRows), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(j, offsetCols), IR_Lower(j, nCols + offsetCols), IR_PreIncrement(j), ListBuffer[IR_Statement](
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
    func.body += IR_VariableDeclaration(_i)
    func.body += IR_VariableDeclaration(_j)
    func.body += IR_ForLoop(IR_Assignment(_i, 0), IR_Lower(_i, outsize._1), IR_PreIncrement(_i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(_j, 0), IR_Lower(_j, outsize._2), IR_PreIncrement(_j), ListBuffer[IR_Statement](
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
  def luDecomposedDeterminant(in : IR_VariableAccess, P : IR_VariableAccess, out : IR_VariableAccess) : IR_Scope = {
    var func = IR_Scope(Nil)
    var det = IR_VariableAccess("det", IR_DoubleDatatype)
    var N = IR_BasicMatrixOperations.getSize(in)._1
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(i)
    func.body += IR_VariableDeclaration(det, IR_HighDimAccess(in, IR_ExpressionIndex(0, 0)))
    func.body += IR_ForLoop(IR_Assignment(i, IR_IntegerConstant(1)), IR_Lower(i, N), IR_PreIncrement(i), ListBuffer[IR_Statement](
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
      func.body += luDecomposedDeterminant(inCopy, P, out)
    }
    else {
      func.body ++= IR_GenerateRuntimeInversion.localLUDecomp(in, P, N, zeroOffset, zeroOffset)
      func.body += luDecomposedDeterminant(in, P, out)
    }
    func
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
object IR_GenerateRuntimeInversion {

  // direct inversion for small matrices
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
    stmts ++= IR_GenerateBasicMatrixOperations.printMatrix(out)
    IR_Scope(stmts)
  }

  // give a invert algorithm for diagonal matrices
  def diagonal(in : IR_VariableAccess, out : IR_VariableAccess) : IR_Scope = {
    var debug = true
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
      IR_Assignment(tmp, IR_HighDimAccess(in, IR_ExpressionIndex(i, i))),
      IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(IR_Addition(IR_Multiplication(i, N), i))), IR_Division(IR_DoubleConstant(1), tmp)),
      IR_Assignment(i, IR_Addition(i, IR_IntegerConstant(1)))
    ))

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

    func
  }

  // generate a LU decomposition for a submatrix at 'offset_r','offset_c' of 'in' inplace
  def localLUDecomp(in : IR_VariableAccess, P : IR_VariableAccess, blocksize_asInt : Int, offset_r : IR_VariableAccess, offset_c : IR_VariableAccess) : ListBuffer[IR_Statement] = {
    val debug = false

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
    var outstream = IR_VariableAccess("std::cout", IR_StringDatatype)

    func += IR_VariableDeclaration(i)
    func += IR_VariableDeclaration(j)
    func += IR_VariableDeclaration(k)
    func += IR_VariableDeclaration(tmp_row)
    func += IR_VariableDeclaration(imax)
    func += IR_VariableDeclaration(maxA)
    func += IR_VariableDeclaration(absA)
    func += IR_ForLoop(IR_Assignment(i, IR_IntegerConstant(0)), IR_Lower(i, blocksize_asInt + 1), IR_ExpressionStatement(IR_PreIncrement(i)), ListBuffer[IR_Statement](
      IR_Assignment(IR_ArrayAccess(P, i), i)
    ))
    func += IR_ForLoop(IR_Assignment(i, IR_IntegerConstant(0)), IR_Lower(i, blocksize_asInt), IR_ExpressionStatement(IR_PreIncrement(i)), ListBuffer[IR_Statement](
      IR_Assignment(maxA, IR_RealConstant(0)),
      IR_Assignment(imax, i),
      IR_ForLoop(IR_Assignment(k, i), IR_Lower(k, blocksize_asInt), IR_ExpressionStatement(IR_PreIncrement(k)), ListBuffer[IR_Statement](
        IR_Assignment(absA, IR_FunctionCall(IR_ExternalFunctionReference.fabs, ListBuffer[IR_Expression](IR_HighDimAccess(in, IR_ExpressionIndex(k + offset_r, i + offset_c))))),
        IR_IfCondition(IR_Greater(absA, maxA), ListBuffer[IR_Statement](IR_Assignment(maxA, absA), IR_Assignment(imax, k)), ListBuffer[IR_Statement]())
      )),
      IR_IfCondition(IR_Lower(maxA, Tol), ListBuffer[IR_Statement](IR_Print(outstream, ListBuffer[IR_Expression](IR_StringConstant("[Warning] inverting potentially singular matrix\\n"))), IR_Return(IR_IntegerConstant(-1))), ListBuffer[IR_Statement]()),
      IR_IfCondition(IR_Neq(imax, i), ListBuffer[IR_Statement](
        IR_Assignment(j, IR_ArrayAccess(P, i)),
        IR_Assignment(IR_ArrayAccess(P, i), IR_ArrayAccess(P, imax)),
        IR_Assignment(IR_ArrayAccess(P, imax), j),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_ArrayAccess(tmp_row, 0)), IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, 0 + offset_c))), blocksize_asInt * IR_SizeOf(baseType)))),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, 0 + offset_c))), IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(imax + offset_r, 0 + offset_c))), blocksize_asInt * IR_SizeOf(baseType)))),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(imax + offset_r, 0 + offset_c))), IR_AddressOf(IR_ArrayAccess(tmp_row, 0)), blocksize_asInt * IR_SizeOf(baseType)))),
        IR_PostIncrement(IR_ArrayAccess(P, blocksize_asInt))
      )),
      IR_ForLoop(IR_Assignment(j, i + 1), IR_Lower(j, blocksize_asInt), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, i + offset_c)), IR_Division(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, i + offset_c)), IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, i + offset_c)))),
        IR_ForLoop(IR_Assignment(k, i + 1), IR_Lower(k, blocksize_asInt), IR_PreIncrement(k), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, k + offset_c)), IR_Subtraction(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, k + offset_c)), IR_Multiplication(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, i + offset_c)), IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, k + offset_c)))))
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
    func += IR_ForLoop(IR_Assignment(j, 0), IR_Lower(j, blocksize), IR_PreIncrement(j), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(i, 0), IR_Lower(i, blocksize), IR_PreIncrement(i), ListBuffer[IR_Statement](
        IR_IfCondition(IR_EqEq(IR_ArrayAccess(P, i), j), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), 1.0)
        ), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), 0.0)
        )),
        IR_ForLoop(IR_Assignment(k, 0), IR_Lower(k, i), IR_PreIncrement(k), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Subtraction(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Multiplication(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, k + offset_c)), IR_HighDimAccess(out, IR_ExpressionIndex(k + offset_r, j + offset_c)))))
        ))
      )),
      IR_ForLoop(IR_Assignment(i, blocksize - 1), IR_GreaterEqual(i, 0), IR_PostDecrement(i), ListBuffer[IR_Statement](
        IR_ForLoop(IR_Assignment(k, i + 1), IR_Lower(k, blocksize), IR_PreIncrement(k), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Subtraction(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Multiplication(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, k + offset_c)), IR_HighDimAccess(out, IR_ExpressionIndex(k + offset_r, j + offset_c)))))
        )),
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Division(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, i + offset_c))))
      ))
    ))
    func
  }

  // combines LU decomposition and inversion of submatrix of 'in' at 'offset_r', 'offset_c' of size 'blocksize'
  def localLUInversion(in : IR_VariableAccess, blocksize_asInt : Int, offset_r : IR_VariableAccess, offset_c : IR_VariableAccess, out : IR_VariableAccess) : IR_Scope = {
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

  // give an invert algorithm for blockdiagonal matrices
  def blockdiagonal(in : IR_VariableAccess, blocksize : Int, out : IR_VariableAccess) : IR_Scope = {
    var debug = true
    var func = IR_Scope(Nil)
    var block = IR_VariableAccess("block", IR_IntegerDatatype)
    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    val N = inDt.sizeM
    if (N % blocksize != 0) Logger.error("IR_ResolveMatrixFunctions::runtimeInverseBlockdiagonal: Matrices with size not mutliple of blocksize not implemented yet")
    func.body += IR_VariableDeclaration(block)
    func.body += IR_ForLoop(IR_Assignment(block, 0), IR_Lower(block, N), IR_Assignment(block, IR_Addition(block, blocksize)), ListBuffer[IR_Statement](
    ) += localLUInversion(in, blocksize, block, block, out))

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
  def schur(in : IR_VariableAccess, blockSize : Int, blockSizeA : Int, out : IR_VariableAccess) : IR_Scope = {
    var debug = true
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
    func.body += IR_GenerateBasicMatrixOperations.copySubmatrix(in, A, offset_r, offset_c, n, n)
    func.body += IR_VariableDeclaration(A_inv)
    //func.body += GenerateRuntimeInversion.blockdiagonal(A, blocksizeA, A_inv)
    //func.body += GenerateRuntimeInversion.runtimeInverseLU(A,A_inv)
    func.body += IR_GenerateRuntimeInversion.localLUInversion(A, n_asInt, offset_r, offset_c, A_inv)

    // copy B
    func.body += IR_VariableDeclaration(B)
    func.body += IR_GenerateBasicMatrixOperations.copySubmatrix(in, B, offset_r, n, n, m)

    // copy C
    func.body += IR_VariableDeclaration(C)
    func.body += IR_GenerateBasicMatrixOperations.copySubmatrix(in, C, n, offset_c, m, n)

    // copy D
    func.body += IR_VariableDeclaration(D)
    func.body += IR_GenerateBasicMatrixOperations.copySubmatrix(in, D, n, n, m, m)

    // calculate S
    func.body += IR_VariableDeclaration(S)
    func.body += IR_VariableDeclaration(CA_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(C, A_inv, CA_inv, 0, 0)
    func.body += IR_VariableDeclaration(CA_invB)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(CA_inv, B, CA_invB, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.subAtSubmatrix(D, CA_invB, S, 0, 0)

    // calculate S_inv
    func.body += IR_VariableDeclaration(S_inv)
    func.body += IR_GenerateRuntimeInversion.localLUInversion(S, m_asInt, offset_r, offset_c, S_inv)

    // calculate upper right result block
    func.body += IR_VariableDeclaration(A_invB)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_inv, B, A_invB, 0, 0)
    func.body += IR_VariableDeclaration(A_invBS_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_invB, S_inv, A_invBS_inv, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(A_invBS_inv, out, 0, n_asInt)

    // insert lower right result block
    func.body += IR_GenerateBasicMatrixOperations.pasteSubmatrix(S_inv, out, n, n)

    // calculate lower left result block
    func.body += IR_VariableDeclaration(S_invCA_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(S_inv, CA_inv, S_invCA_inv, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(S_invCA_inv, out, n_asInt, 0)

    // calculate upper left result block
    func.body += IR_VariableDeclaration(A_invBS_invCA_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_invB, S_invCA_inv, A_invBS_invCA_inv, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.addAtSubmatrix(A_inv, A_invBS_invCA_inv, out, 0, 0)

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

    func
  }

  // head function that branches to specific inversions
  def inverse(in : IR_VariableAccess, matrixStructure : String, blockSize : Int, out : IR_VariableAccess) : IR_Scope = {
    var insize = IR_BasicMatrixOperations.getSize(in)
    var outsize = IR_BasicMatrixOperations.getSize(out)
    if (insize._1 != insize._2)
      Logger.error("inversion of matrices of size " + insize._1 + "," + insize._2 + " not supported")
    if (insize != outsize)
      Logger.error("matrix sizes of in and out do not match: " + insize + " vs " + outsize)

    matrixStructure match {
      case "Filled"        =>
        var debug = true
        //TODO maybe overload GenerateRuntimeInversion methods or 0-access-constant
        var stmts = ListBuffer[IR_Statement]()

        if (insize._1 < 4) {
          stmts += smallMatrixInversion(in, out)
        } else {
          var offsetIsZero = IR_VariableAccess("zero", IR_IntegerDatatype)
          stmts += IR_VariableDeclaration(offsetIsZero, 0)
          // use localLUInversion for the full matrix
          stmts += localLUInversion(in, insize._1, offsetIsZero, offsetIsZero, out)
        }

        if (debug)
          stmts ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

        IR_Scope(stmts)
      case "Diagonal"      => diagonal(in, out)
      case "Blockdiagonal" => blockdiagonal(in, blockSize, out)
      case "Schur"         => schur(in, blockSize, Knowledge.experimental_blocksize_A, out)
      case _               => Logger.error("runtime inversion: unknown runtimeInverse resolve: " + matrixStructure)
    }
  }
}
