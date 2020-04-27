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
import exastencils.logger.Logger
import exastencils.optimization.ir._

// resolve "Var matrix : Matrix<Datatype, rows, columns> = initialization" or split to declaration and assignment if convenient
object IR_ResolveMatrixDeclarations extends DefaultStrategy("Resolve matrix decl + initialization") {

  this += new Transformation("with constants", {
    // split to use std::fill later
    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _), _, Some(IR_FloatConstant(_) | IR_DoubleConstant(_) | IR_RealConstant(_) | IR_IntegerConstant(_)), _) =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)

    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _), _, Some(IR_VariableAccess(_, IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype)), _) =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)
  })

  this += new Transformation("with matrices", {
    // do nothing
    case decl @ IR_VariableDeclaration(declDt @ IR_MatrixDatatype(_, _, _), _, Some(srcDt @ IR_MatrixExpression(_, _, _)), _) =>
      if (declDt.sizeM != srcDt.rows || declDt.sizeN != srcDt.columns)
        Logger.error("Declaration of variable of type: " + declDt + " with expression of type: " + srcDt + ", sizes must match!")
      decl

    // split to use std::memcpy or std::copy later
    case decl @ IR_VariableDeclaration(declDt @ IR_MatrixDatatype(_, _, _), _, Some(IR_VariableAccess(_, srcDt @ IR_MatrixDatatype(_, _, _))), _) =>
      if (declDt.sizeM != srcDt.sizeM || declDt.sizeN != srcDt.sizeN)
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
        Logger.error("wrong number of arguments: " + init.arguments.length + ", expected 1 argument for transpose")
      init.arguments(0) match {
        case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
          IR_VariableDeclaration(datatype, name, Some(IR_BasicMatrixOperations.transpose(va)))
        case _                                                     =>
          Logger.error("wrong type of argument: " + init.arguments(0).datatype + ", expected variable access to matrix variable")
      }

    // split inverse call to handle as assignment
    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _), _, Some(init : IR_FunctionCall), _) if (init.name == "inverse") =>
      //TODO simplify strategies: make 1x1 matrices doubles -> inversion call on double fails -> simplify after this one?
      if (!init.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype])
        Logger.error("wrong type of argument: " + init.arguments(0).datatype + " expected matrix")
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
    case decl @ IR_VariableDeclaration(IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype, _, Some(call @ IR_FunctionCall(_, _)), _) if (call.name == "getElement") =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)

    // resolve cross product
    case IR_VariableDeclaration(datatype @ IR_MatrixDatatype(_, _, _), name, Some(call @ IR_FunctionCall(_, _)), _) if (call.name == "crossProduct" || call.name == "cross") =>
      if (call.arguments.length != 2)
        Logger.error("wrong number of arguments: " + call.arguments.length + " expected 2 arguments for crossProduct")
      IR_VariableDeclaration(datatype, name, IR_BasicMatrixOperations.crossProduct(call.arguments(0), call.arguments(1)))

    // resolve trace call
    case IR_VariableDeclaration(datatype @ (IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype), name, Some(call @ IR_FunctionCall(_, _)), _) if (call.name == "trace") =>
      if (call.arguments.length != 1)
        Logger.error("wrong number of arguments: " + call.arguments.length + ", expected 1 argument for function call 'trace'")
      IR_VariableDeclaration(datatype, name, IR_BasicMatrixOperations.trace(call.arguments(0)))

    // resolve matrix multiplication
    case IR_VariableDeclaration(datatype @ IR_MatrixDatatype(_, _, _), name, Some(call @ IR_FunctionCall(_, _)), _) if (call.name == "matmult") =>
      if (call.arguments.length < 2)
        Logger.error("expected at least two matrices to multiply, got: " + call.arguments.length)
      IR_VariableDeclaration(datatype, name, IR_BasicMatrixOperations.mult(IR_Multiplication(call.arguments)))

    //TODO experimental: eigenvalues
    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _) | IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype, _, Some(call @ IR_FunctionCall(_, _)), _) if (call.name == "eigenvalues") =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)
  })
}

// resolve "matrix = expression"
object IR_ResolveMatrixAssignments extends DefaultStrategy("Resolve matrix assignments") {
  var debug = false

  //TODO matrix infos as functions argument of inverse -> class local variables?

  // use std::fill for assignments of matrices with constants
  this += new Transformation("with constants", {
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src @ (IR_RealConstant(_) | IR_DoubleConstant(_) | IR_IntegerConstant(_) | IR_FloatConstant(_)), "=")         =>
      IR_FunctionCall(IR_ExternalFunctionReference("std::fill", IR_UnitDatatype), ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + dest.datatype.asInstanceOf[IR_MatrixDatatype].resolveFlattendSize, src)) : IR_Statement
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src @ IR_VariableAccess(_, IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype | IR_FloatDatatype), "=") =>
      IR_FunctionCall(IR_ExternalFunctionReference("std::fill", IR_UnitDatatype), ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + dest.datatype.asInstanceOf[IR_MatrixDatatype].resolveFlattendSize, src)) : IR_Statement
  })

  // assignment of a matrix with another matrix : copy other matrix
  this += new Transformation("with matrices", {
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src @ (IR_MatrixExpression(_, _, _) | IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), "=") =>
      IR_GenerateBasicMatrixOperations.copyMatrix(dest, src)
  })

  // split combined assignment: += to IR_Addition, *= to IR_Multiplication, /= to IR_Division, -= to IR_Subtraction
  this += new Transformation("split combined operators", {
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src, "+=") =>
      IR_Assignment(dest, IR_Addition(dest, src))
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src, "*=") =>
      IR_Assignment(dest, IR_Multiplication(ListBuffer[IR_Expression](dest, src)))
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src, "-=") =>
      IR_Assignment(dest, IR_Subtraction(dest, src))
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src, "/=") =>
      IR_Assignment(dest, IR_ElementwiseDivision(dest, src))
  })

  // resolve assignments of matrices with operators
  //TODO elementwise power and modulo: does not parse
  this += new Transformation("with operators", {
    // (pointwise) addition of matrices
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), addition @ (IR_Addition(_) | IR_ElementwiseAddition(_, _)), _) =>
      IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_BasicMatrixOperations.add(addition))

    // (pointwise) subtraction of two matrices
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), subtraction @ (IR_Subtraction(_, _) | IR_ElementwiseSubtraction(_, _)), _) =>
      IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_BasicMatrixOperations.sub(subtraction))

    // multiplication of matrices
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), mult : IR_Multiplication, _) =>
      IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_BasicMatrixOperations.mult(mult))

    // pointwise multiplication of two matrices
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), mult : IR_ElementwiseMultiplication, _) =>
      IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_BasicMatrixOperations.elementwiseMultiplication(mult.left, mult.right))

    // pointwise division of two matrices
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), div : IR_ElementwiseDivision, _) if (div.left.datatype.isInstanceOf[IR_MatrixExpression] | div.right.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_BasicMatrixOperations.elementwiseMultiplication(div.left, div.right))
  })

  this += new Transformation("with build-in functions", {
    // transpose a matrix
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), call @ IR_FunctionCall(_, ListBuffer(matrix @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)))), _) if (call.name == "transpose") =>
      if (call.arguments.length != 1)
        Logger.error("wrong number of arguments")
      IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_BasicMatrixOperations.transpose(matrix))

    // slice a matrix: test for compiletime constants and call slicing at compile/runtime
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), call @ IR_FunctionCall(_, ListBuffer(matrix @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), offsetRows : IR_Expression, offsetCols : IR_Expression, nRows : IR_Expression, nCols : IR_Expression)), _) if (call.name == "getSlice") =>
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
      allCompiletimeConstant match {
        case true  =>
          IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_BasicMatrixOperations.copySubMatrix(matrix, offsetRows_asInt, offsetCols_asInt, nRows_asInt, nCols_asInt))
        case false =>
          IR_GenerateBasicMatrixOperations.copySubmatrix(matrix, dest, offsetRows, offsetCols, nRows, nCols)
      }

    // dot product of two matrices
    case IR_Assignment(dest @ IR_VariableAccess(_, _), call @ IR_FunctionCall(_, _), _) if (call.name == "dotProduct" || call.name == "dot") =>
      if (call.arguments.length != 2)
        Logger.error("wrong number of arguments")
      IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_BasicMatrixOperations.dotProduct(call.arguments(0), call.arguments(1)))

    // cross product
    case IR_Assignment(dest @ IR_VariableAccess(_, _), call @ IR_FunctionCall(_, _), _) if (call.name == "crossProduct" || call.name == "cross") =>
      if (call.arguments.length != 2)
        Logger.error("wrong number of arguments: " + call.arguments.length)
      IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_BasicMatrixOperations.crossProduct(call.arguments(0), call.arguments(1)))

    // trace
    case IR_Assignment(dest @ IR_VariableAccess(_, _), call @ IR_FunctionCall(_, _), _) if (call.name == "trace") =>
      if (call.arguments.length != 1)
        Logger.error("wrong number of arguments: " + call.arguments.length + ", expected 1 argument for trace call")
      IR_Assignment(dest, IR_BasicMatrixOperations.trace(call.arguments(0)))

    // matrix multiplication
    case IR_Assignment(dest @ IR_VariableAccess(_, _), call @ IR_FunctionCall(_, _), _) if (call.name == "matmult") =>
      if (call.arguments.length < 2)
        Logger.error("expected at least 2 arguments for matrix multiplication, got: " + call.arguments.length)
      IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_BasicMatrixOperations.mult(IR_Multiplication(call.arguments)))

    //TODO TEST
    /*
    case IR_Assignment(dest @ IR_VariableAccess(_, _), call @ IR_FunctionCall(_, _), _) if (call.name == "eigenvalues") =>
      IR_GenerateQRDecomposition.householderQRDecomposition(call.arguments(0).asInstanceOf[IR_VariableAccess], dest)
    */
  })

  /*
  // resolve inversion call depending on Knowledge.experimentalResolveInverseFunctionCall and Knowledge.experimental_matrixStructure
  this += new Transformation("with inversion call", {
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), call : IR_FunctionCall, _) if (call.name == "inverse") =>
      if (call.arguments.length != 1)
        Logger.error("resolve inversion: inverse call with " + call.arguments.length + " arguments not supported")
      var inMatrix = call.arguments(0)

      if (Knowledge.experimental_matrixStructure == "determine") {
        inMatrix match {
          case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
            Logger.error("classification for matrix variables not supported")
          case x : IR_MatrixExpression                          =>
            var structureInformation = IR_DetermineMatrixStructure.isOfStructure(x)
            Knowledge.experimental_matrixStructure = structureInformation._1
            Knowledge.experimental_blocksize = structureInformation._2
            Knowledge.experimental_structure_A = structureInformation._3
            Knowledge.experimental_blocksize_A = structureInformation._4
        }
      }

      Knowledge.experimental_resolveInverseFunctionCall match {
        case "Runtime"     =>
          inMatrix match {
            case x : IR_MatrixExpression                               =>
              var tmp = IR_VariableDeclaration(IR_MatrixDatatype(x.datatype.resolveBaseDatatype, x.rows, x.columns), "tmp", x)
              IR_Scope(
                tmp,
                IR_GenerateRuntimeInversion.inverse(IR_VariableAccess(tmp), dest)
              )
            case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_GenerateRuntimeInversion.inverse(va, dest)
            case _                                                     => Logger.error("argument of unexpected type: " + inMatrix.datatype)
          }
        case "Compiletime" =>
          inMatrix match {
            case x : IR_MatrixExpression                               => IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_CompiletimeInversion.inverse(x, Knowledge.experimental_matrixStructure, Knowledge.experimental_blocksize))
            case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_CompiletimeInversion.inverse(IR_MatrixNodeUtilities.accessToExpression(va), Knowledge.experimental_matrixStructure, Knowledge.experimental_blocksize))
            case _                                                     => Logger.error("argument of unexpected type: " + inMatrix.datatype)
          }
        case _             =>
          Logger.error("resolve inversion: type of inverse resolve " + Knowledge.experimental_resolveInverseFunctionCall + " not supported")
      }
  })
*/


  this += new Transformation("with inversion call()", {
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), call : IR_FunctionCall, _) if (call.name == "inverse") =>

      // check arguments 2 to 5
      call.arguments.length match {
        case 0 =>
          Logger.error("no argument passed, expected a matrix!")
        case 1 =>
          Knowledge.experimental_matrixStructure = "Filled"
        case _ =>
          call.arguments(1) match {
            case IR_StringConstant(s @ ("Determine" | "Filled" | "Diagonal")) => Knowledge.experimental_matrixStructure = s
            case IR_StringConstant(s @ "Blockdiagonal")                     =>
              if (call.arguments.length != 3)
                Logger.error("Blockdiagonal matrix specified but no blocksize given!")
              Knowledge.experimental_matrixStructure = s
              call.arguments(2) match {
                case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                case IR_IntegerConstant(c)                       => Knowledge.experimental_blocksize = c.toInt
                case _                                           => Logger.error("blocksize of unexpected type: " + call.arguments + ", expected integer constant or access to integer variable")
              }
            case IR_StringConstant(s @ "Schur")                             =>
              if (call.arguments.length < 3)
                Logger.error("schur matrix specified but no blocksize given!")
              Knowledge.experimental_matrixStructure = s
              call.arguments(2) match {
                case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                case IR_IntegerConstant(c)                       => Knowledge.experimental_blocksize = c.toInt
                case _                                           => Logger.error("blocksize is of unexpected type: " + call.arguments + ", expected integer constant or access to integer variable")
              }
              call.arguments(3) match {
                case IR_StringConstant(s @ ("Filled" | "Diagonal")) => Knowledge.experimental_structure_A = s
                case IR_StringConstant(s @ "Blockdiagonal")       =>
                  Knowledge.experimental_structure_A = s
                  if (call.arguments.length != 5)
                    Logger.error("Blockdiagonal specified for A matrix in schur structure matrix but no blocksize given!")
                  call.arguments(4) match {
                    case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                    case IR_IntegerConstant(c)                       => Knowledge.experimental_blocksize_A = c.toInt
                    case _                                           => Logger.error("blocksize for blockdiagonal matrix A is of unexpected type: " + call.arguments + ", expected integer constant or access to integer variable")

                  }
                case _=> Logger.error("unexpected type for upper left matrix in schur structure matrix: " + call.arguments(3) + ", expected Filled or Diagonal or Blockdiagonal")
              }
            case _                                                          => Logger.error("unexpected argument combination: " + call.arguments + ", expected: 'Determine' or 'Filled' or 'Diagonal' without additional arguments or 'Blockdiagonal' with a blocksize as 3rd argument or 'Schur' with a blocksize which specifies the width of the lower right matrix D as 3rd argument and additionally the structure of the upper left matrix A as 4th argument and its blocksize as 5th in case of a blockdiagonal matrix for A;" +
              "in short: " +
              "inverse(mat, Filled) or inverse(mat, Determine) or inverse(mat,Diagonal) or inverse(mat, Blockdiagonal, blocksize) or inverse(mat, Schur, blocksize, Filled) or inverse(mat, Schur, blocksize, Diagonal) or inverse(mat, Schur, blocksize, Blockdiagonal, blocksize_A")

          }
      }

      // check for first argument: should be a matrix
      if (!((call.arguments(0).isInstanceOf[IR_VariableAccess] && call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) || call.arguments(0).isInstanceOf[IR_MatrixExpression])) {
        Logger.error("expected matrix variable or expression as first argument, got " + call.arguments(0))
      }
      var inMatrix = call.arguments(0)

      // structure is to determine
      if (Knowledge.experimental_matrixStructure == "Determine") {
        inMatrix match {
          case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
            //TODO compiletime constant check of variable accesses?
            Logger.error("classification for matrix variables not supported")
          case x : IR_MatrixExpression                          =>
            var structureInformation = IR_DetermineMatrixStructure.isOfStructure(x)
            Knowledge.experimental_matrixStructure = structureInformation._1
            Knowledge.experimental_blocksize = structureInformation._2
            Knowledge.experimental_structure_A = structureInformation._3
            Knowledge.experimental_blocksize_A = structureInformation._4
        }
      }

      Knowledge.experimental_resolveInverseFunctionCall match {
        case "Runtime"     =>
          inMatrix match {
            case x : IR_MatrixExpression                               =>
              var rt_exprToAccess = IR_VariableDeclaration(IR_MatrixDatatype(x.datatype.resolveBaseDatatype, x.rows, x.columns), "tmp", x)
              IR_Scope(
                rt_exprToAccess,
                IR_GenerateRuntimeInversion.inverse(IR_VariableAccess(rt_exprToAccess), dest, Knowledge.experimental_matrixStructure)
              )
            case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_GenerateRuntimeInversion.inverse(va, dest, Knowledge.experimental_matrixStructure)
            case _                                                     => Logger.error("argument of unexpected type: " + inMatrix.datatype)
          }
        case "Compiletime" =>
          inMatrix match {
            case x : IR_MatrixExpression                               => IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_CompiletimeInversion.inverse(x, Knowledge.experimental_matrixStructure, Knowledge.experimental_blocksize))
            case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_GenerateBasicMatrixOperations.copyMatrix(dest, IR_CompiletimeInversion.inverse(IR_MatrixNodeUtilities.accessToExpression(va), Knowledge.experimental_matrixStructure, Knowledge.experimental_blocksize))
            case _                                                     => Logger.error("argument of unexpected type: " + inMatrix.datatype)
          }
        case _             =>
          Logger.error("resolve inversion: type of inverse resolve " + Knowledge.experimental_resolveInverseFunctionCall + " not supported")
      }
  })


}

// Resolve standalone(no assign or decl to a matrix variable) matrix functions
// e.g. "function(matrix, ...)" or "Var v : Double = Function(matrix, ...)"
object IR_ResolveNoMatrixReturnOperations extends DefaultStrategy("Resolve standalone matrix functions") {
  this += new Transformation("no return value", {
    // compare two matrices or scalars
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, ListBuffer(left : IR_Expression, right : IR_Expression, precision : IR_Expression))) if (call.name == "compare") =>
      IR_GenerateBasicMatrixOperations.compare(left, right, precision)

    // set a slice to 'newValue'
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, _)) if (call.name == "setSlice") =>
      call.arguments match {
        case ListBuffer(matrix @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), offsetRows @ (IR_VariableAccess(_, IR_IntegerDatatype) | IR_IntegerConstant(_)), offsetCols @ (IR_VariableAccess(_, IR_IntegerDatatype) | IR_IntegerConstant(_)), nRows @ (IR_VariableAccess(_, IR_IntegerDatatype) | IR_IntegerConstant(_)), nCols @ (IR_VariableAccess(_, IR_IntegerDatatype) | IR_IntegerConstant(_)), newValue @ (IR_VariableAccess(_, IR_FloatDatatype | IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype) | IR_FloatConstant(_) | IR_IntegerConstant(_) | IR_DoubleConstant(_) | IR_RealConstant(_))) =>
          IR_GenerateBasicMatrixOperations.setSubmatrix(matrix, offsetRows, offsetCols, nRows, nCols, newValue)
        case _                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         => Logger.error("unexpected arguments: " + call.arguments + ", expected setElement(matrix,offsetRows,offsetColumns,newValue), with matrix=access to matrix variable, (offsetRows, offsetCols, numberOfRows, numberOfColumns)=Integer constant or access to integer variable and newValue=access to variable or constant value")
      }

    // set an element to 'newValue'
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, _)) if (call.name == "setElement") =>
      call.arguments match {
        case ListBuffer(matrix @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), offsetRows @ (IR_VariableAccess(_, IR_IntegerDatatype) | IR_IntegerConstant(_)), offsetCols @ (IR_VariableAccess(_, IR_IntegerDatatype) | IR_IntegerConstant(_)), newValue @ (IR_VariableAccess(_, IR_FloatDatatype | IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype) | IR_FloatConstant(_) | IR_IntegerConstant(_) | IR_DoubleConstant(_) | IR_RealConstant(_))) =>
          IR_Assignment(IR_HighDimAccess(matrix, IR_ExpressionIndex(offsetRows, offsetCols)), newValue)
        case _                                                                                                                                                                                                                                                                                                                                                                                                                                                 => Logger.error("unexpected arguments: " + call.arguments + ", expected setElement(matrix,offsetRows,offsetColumns,newValue), with matrix=access to matrix variable, (offsetRows, offsetCols)=Integer constant or access to integer variable and newValue=access to variable or constant value")
      }
  })

  this += new Transformation("scalar return value", {
    // determinant: calculate in runtime (LU) if input matrix is larger than 5, in compiletime(directly,laplace expansion) if 5 or smaller
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype), call @ IR_FunctionCall(_, _), _) if (call.name == "determinant") =>
      if (call.arguments.length != 1)
        Logger.error("wrong number of arguments: " + call.arguments.length + ", expected one argument for determinant")
      var inMatrix = call.arguments(0)
      if (IR_BasicMatrixOperations.getSize(inMatrix)._1 > 5)
      // runtime determinant
        inMatrix match {
          case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
            IR_GenerateBasicMatrixOperations.determinant(va, dest)
          case x @ IR_MatrixExpression(_, _, _)                      =>
            var stmts = IR_Scope(Nil)
            var tmp_access = IR_VariableAccess("tmp_" + IR_MatrixExpression.matTmpCounter, x.datatype)
            stmts.body += IR_VariableDeclaration(tmp_access, x)
            stmts.body += IR_GenerateBasicMatrixOperations.determinant(tmp_access, dest)
          case _                                                     => Logger.error("argument type not supported: " + inMatrix + ", expected matrix expression or variable")
        }
      else {
        // compiletime determinant
        inMatrix match {
          case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
            IR_Assignment(dest, IR_BasicMatrixOperations.determinantSmallMatrix(IR_MatrixNodeUtilities.accessToExpression(va)))
          case x @ IR_MatrixExpression(_, _, _)                      =>
            IR_Assignment(dest, IR_BasicMatrixOperations.determinantSmallMatrix(x))
          case _                                                     => Logger.error("argument type not supported: " + inMatrix + ", expected matrix expression or variable")
        }
      }

    // receive an element value of a matrix
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype), call @ IR_FunctionCall(_, _), _) if (call.name == "getElement") =>
      call.arguments match {
        case ListBuffer(matrix @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), offsetRows @ (IR_VariableAccess(_, IR_IntegerDatatype) | IR_IntegerConstant(_)), offsetCols @ (IR_VariableAccess(_, IR_IntegerDatatype) | IR_IntegerConstant(_))) =>
          if (call.arguments.length != 3)
            Logger.error("wrong number of arguments: " + call.arguments.length)
          IR_Assignment(dest, IR_HighDimAccess(matrix, IR_ExpressionIndex(offsetRows, offsetCols)))
        case _                                                                                                                                                                                                                                       => Logger.error("unexpected arguments:" + call.arguments + ", expected getElement(matrix,offsetRows,offsetCols) with matrix=access to matrix variable, offsetRows, offsetCols=Integer constant or access to integer variable")
      }
  })

  /*
  //TODO eigenvalues: householder transformations  + QR decomposition
  this += new Transformation("tuple return value", {
  })

   */
}

// Resolve user defined functions
object IR_ResolveUserDefinedFunctions extends DefaultStrategy("Resolve user defined functions") {
  var matExpCounter = 0
  var resolveFunctions = ListBuffer[String]()
  this.onBefore = () => {
    resolveFunctions.clear()
    resolveFunctions ++= ListBuffer("dotProduct", "dot", "crossProduct", "cross", "det", "determinant", "getSlice", "setSlice", "getElement", "setElement", "transpose", "inverse", "compare", "recognizeDiagonal", "recognizeBlockdiagonal", "recognizeSchur")
  }

  this += new Transformation("add assignments/decl to function returns to arguments", {
    case IR_Assignment(dest, src : IR_FunctionCall, "=") if (!resolveFunctions.contains(src.name) && dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype])  =>
      src.arguments += dest
      IR_ExpressionStatement(src)
    case IR_Assignment(dest, src : IR_FunctionCall, "+=") if (!resolveFunctions.contains(src.name) && dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      Logger.error("+= matrix operator resolution not yet implemented")
  })

  this += new Transformation("transform arguments and return calls in udfunctions", {

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

  this += new Transformation("simplify function call arguments", {
    case stmt @ IR_ExpressionStatement(exp : IR_FunctionCall) if (!resolveFunctions.contains(exp.function.name)) =>
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
    case stmt @ IR_Assignment(_, exp : IR_FunctionCall, _) if (!resolveFunctions.contains(exp.function.name))    =>
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

// Simplifications from IR_GeneralSimplify
object IR_SimplifyMatrices extends DefaultStrategy("Simplify matrices") {
  this += new Transformation("simplify", {
    case IR_Negative(m : IR_MatrixExpression) => m.expressions = m.expressions.map { y => IR_Negative(y) : IR_Expression }; m
    case m @ IR_MatrixExpression(_, 1, 1)     => m.get(0, 0)
    case m @ IR_MatrixDatatype(dt, 1, 1)      => dt
  })
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

// methods to transform certain types of nodes in relation to matrices
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

  /*
  // convert an assignment of a IR_MatrixExpression to multiple Assignments for all positions in dest/src; dest and src have to be of the same form
  def expressionToAssignmentsLooped(dest : IR_VariableAccess, src : IR_MatrixExpression) : IR_Scope = {
    var destSize = IR_BasicMatrixOperations.getSize(dest)
    if (destSize != (src.rows, src.columns))
      Logger.error("sizes do not match: " + destSize + " vs " + (src.rows, src.columns))
    var stmts = ListBuffer[IR_Statement]()
    var i = IR_VariableAccess("i",IR_IntegerDatatype)
    var j = IR_VariableAccess("j",IR_IntegerDatatype)

    stmts += IR_ForLoop(IR_VariableDeclaration(i,IR_IntegerConstant(0)),IR_Lower(i,destSize._1),IR_PreIncrement(i),ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(j,IR_IntegerConstant(0)),IR_Lower(j,destSize._2),IR_PreIncrement(j),ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(i, j)), src.get(i, j))
      ))
    ))
    IR_Scope(stmts)
  }
  */

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
