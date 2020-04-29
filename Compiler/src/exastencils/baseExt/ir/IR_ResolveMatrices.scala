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
import exastencils.baseExt.ir.IR_ResolveMatrixOperations.isEvaluatable
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.EvaluationException
import exastencils.optimization.ir.IR_SimplifyExpression

// handle runtime methods seperately: they must be processed with their destination/return variable
object IR_HandleRuntimeMatrices extends DefaultStrategy("Handle runtime operations") {
  // runtime methods work with a destination variable and have to be resolved with their assignment -> extract them to helper variables
  var runtimeMethods = ListBuffer[String]("invRT", "detRT", "getSliceRT")
  var extractMethodsLabel = "extractMethod"
  var extractMethodsCounter = 0

  // create new variables to extract runtime method calls to and mark nodes for later replacement
  def extractAndAnnotate(src : IR_Expression) : ListBuffer[IR_Statement] = {
    var newStmts = ListBuffer[IR_Statement]()
    var rtfuncs = StateManager.findAll[IR_FunctionCall](src).filter(f => runtimeMethods.contains(f.function.name))
    rtfuncs.foreach(f => if (!f.datatype.equals(IR_UnknownDatatype)) {
      val decl = IR_VariableDeclaration(f.datatype, "extractTmp_" + extractMethodsCounter, Duplicate(f))
      f.annotate(extractMethodsLabel, extractMethodsCounter)
      extractMethodsCounter += 1
      newStmts += decl
      //newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
    })
    newStmts
  }
  //if (!(src.isInstanceOf[IR_FunctionCall] && !runtimeMethods.contains(src.asInstanceOf[IR_FunctionCall].name)))
  // extract runtime methods in statements to helper variables
  this += new Transformation("mark and extract rtmethod calls", {
    case stmt @ IR_Assignment(_, src, _)                      =>
      extractAndAnnotate(src) += stmt
    case decl @ IR_VariableDeclaration(_, _, Some(init), _)  =>
      extractAndAnnotate(init) += decl
    case expstmt @ IR_ExpressionStatement(src)                =>
      extractAndAnnotate(src) += expstmt
  })

  this += new Transformation("replace rtmethod calls", {
    case exp : IR_FunctionCall if (exp.hasAnnotation(extractMethodsLabel)) =>
      IR_VariableAccess("extractTmp_" + exp.popAnnotationAs[Int](extractMethodsLabel), exp.function.returnType)

    // slice a matrix: test for compiletime constants and call slicing at replace with reference(not constant -> runtime slice), resolve (constant -> compiletime slice)
    case call @ IR_FunctionCall(_, args) if (call.name == "getSlice" && call.arguments.forall(a => isEvaluatable(a))) =>
      var offsetRows_asInt : Int = 0
      var offsetCols_asInt : Int = 0
      var nRows_asInt : Int = 0
      var nCols_asInt : Int = 0
      var evaluatable = true
      try {
        offsetRows_asInt = IR_SimplifyExpression.evalIntegral(args(1)).toInt
        offsetCols_asInt = IR_SimplifyExpression.evalIntegral(args(2)).toInt
        nRows_asInt = IR_SimplifyExpression.evalIntegral(args(3)).toInt
        nCols_asInt = IR_SimplifyExpression.evalIntegral(args(4)).toInt
      } catch {
        case e : EvaluationException => evaluatable = false
        case t : Throwable           => Logger.error("unexpected exception: " + t)
      }
      if (evaluatable)
        IR_BasicMatrixOperations.copySubMatrix(args(0), offsetRows_asInt, offsetCols_asInt, nRows_asInt, nCols_asInt)
      else {
        var size = IR_BasicMatrixOperations.getSize(args(0))
        IR_FunctionCall(IR_UnresolvedFunctionReference("getSliceRT", IR_MatrixDatatype(args(0).datatype.resolveBaseDatatype, size._1, size._2)), args)
      }

    // determinant: replace with reference if matrix is large, resolve if matrix is small
    case call @ IR_FunctionCall(_, args) if ((call.name == "det" | call.name == "deter" | call.name == "determinant") && call.arguments.forall(a => isEvaluatable(a))) =>
      if (IR_BasicMatrixOperations.getSize(args(0))._1 > 5)
      // runtime determinant
        IR_FunctionCall(IR_UnresolvedFunctionReference("detRT", args(0).datatype.resolveBaseDatatype), args)
      else {
        // compiletime determinant
        args(0) match {
          case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
            IR_BasicMatrixOperations.smallMatrixDeterminant(IR_MatrixNodeUtilities.accessToExpression(va))
          case x @ IR_MatrixExpression(_, _, _)                      =>
            IR_BasicMatrixOperations.smallMatrixDeterminant(x)
          case _                                                     => Logger.error("argument type not supported: " + args(0) + ", expected matrix expression or variable")
        }
      }

    // inverse : read arguments, classify matrix, resolve if compiletime, replace if runtime
    case call @ IR_FunctionCall(_, args) if ((call.name == "inv" | call.name == "inverse") && call.arguments.forall(a => isEvaluatable(a))) =>
      // check arguments 2 to 5
      call.arguments.length match {
        case 0 =>
          Logger.error("no argument passed, expected a matrix!")
        case 1 =>
        case _ =>
          call.arguments(1) match {
            case IR_StringConstant(s @ ("Determine" | "Filled" | "Diagonal")) => Knowledge.experimental_matrixStructure = s
            case IR_StringConstant(s @ "Blockdiagonal")                       =>
              if (call.arguments.length != 3)
                Logger.error("Blockdiagonal matrix specified but no blocksize given!")
              Knowledge.experimental_matrixStructure = s
              call.arguments(2) match {
                case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                case IR_IntegerConstant(c)                        => Knowledge.experimental_blocksize = c.toInt
                case _                                            => Logger.error("blocksize of unexpected type: " + call.arguments + ", expected integer constant or access to integer variable")
              }
            case IR_StringConstant(s @ "Schur")                               =>
              if (call.arguments.length < 3)
                Logger.error("schur matrix specified but no blocksize given!")
              Knowledge.experimental_matrixStructure = s
              call.arguments(2) match {
                case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                case IR_IntegerConstant(c)                        => Knowledge.experimental_blocksize = c.toInt
                case _                                            => Logger.error("blocksize is of unexpected type: " + call.arguments + ", expected integer constant or access to integer variable")
              }
              if (call.arguments.length < 4) {
                Knowledge.experimental_structure_A = "Filled"
              }
              else {
                call.arguments(3) match {
                  case IR_StringConstant(s @ ("Filled" | "Diagonal")) => Knowledge.experimental_structure_A = s
                  case IR_StringConstant(s @ "Blockdiagonal")         =>
                    Knowledge.experimental_structure_A = s
                    if (call.arguments.length != 5)
                      Logger.error("Blockdiagonal specified for A matrix in schur structure matrix but no blocksize given!")
                    call.arguments(4) match {
                      case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                      case IR_IntegerConstant(c)                        => Knowledge.experimental_blocksize_A = c.toInt
                      case _                                            => Logger.error("blocksize for blockdiagonal matrix A is of unexpected type: " + call.arguments + ", expected integer constant or access to integer variable")

                    }
                  case _                                              => Logger.error("unexpected type for upper left matrix in schur structure matrix: " + call.arguments(3) + ", expected Filled or Diagonal or Blockdiagonal")
                }
              }
            case _                                                            => Logger.error("unexpected argument combination: " + call.arguments + ", expected: 'Determine' or 'Filled' or 'Diagonal' without additional arguments or 'Blockdiagonal' with a blocksize as 3rd argument or 'Schur' with a blocksize which specifies the width of the lower right matrix D as 3rd argument and additionally the structure of the upper left matrix A as 4th argument and its blocksize as 5th in case of a blockdiagonal matrix for A;" +
              "in short: " +
              "inverse(mat) or inverse(mat, Filled) or inverse(mat, Determine) or inverse(mat,Diagonal) or inverse(mat, Blockdiagonal, blocksize) or inverse(mat, Schur, blocksize) or inverse(mat, Schur, blocksize, Filled) or inverse(mat, Schur, blocksize, Diagonal) or inverse(mat, Schur, blocksize, Blockdiagonal, blocksize_A")
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
          case va @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)) =>
            //TODO compiletime constant check of variable accesses?
            //TODO where to start search for declaration node? root seems wrong
            var foundDecls = StateManager.findAll[IR_VariableDeclaration](StateManager.root).filter(d => d.name == name)
            foundDecls.length match {
              case 0 => Logger.error("could not localize declaration of: " + va)
              case 1 =>
                var decl = foundDecls(0)

                decl.initialValue match {
                  case None                                   => Logger.error("trying to classify not initialized matrix variable at compiletime!")
                  case Some(x @ IR_MatrixExpression(_, _, _)) =>
                    //TODO what if there are assignments between declaration and inverse call -> not compiletime constant, inverse call executes nevertheless
                    // -> need to find all accesses to that matrix variable to make sure its compiletime constant -> there must not be any modifications to that matrix variable
                    var structureInformation = IR_DetermineMatrixStructure.isOfStructure(x)
                    Knowledge.experimental_matrixStructure = structureInformation._1
                    Knowledge.experimental_blocksize = structureInformation._2
                    Knowledge.experimental_structure_A = structureInformation._3
                    Knowledge.experimental_blocksize_A = structureInformation._4
                  case _                                      => Logger.error("unexpected initialization value: " + decl.initialValue + ", expected matrix expression!")
                }

              case _ => Logger.error("thought declarations should be unique, found multiple declarations with name: " + name)
            }

          case x : IR_MatrixExpression =>
            var structureInformation = IR_DetermineMatrixStructure.isOfStructure(x)
            Knowledge.experimental_matrixStructure = structureInformation._1
            Knowledge.experimental_blocksize = structureInformation._2
            Knowledge.experimental_structure_A = structureInformation._3
            Knowledge.experimental_blocksize_A = structureInformation._4
        }
      }

      Logger.warn("Inverting with the following configuration: " + Knowledge.experimental_resolveInverseFunctionCall + ", " + Knowledge.experimental_matrixStructure + ", " + Knowledge.experimental_blocksize + ", " + Knowledge.experimental_structure_A + ", " + Knowledge.experimental_blocksize_A)

      // branch to specific inversions depending on when to invert and the type of matrix to invert
      Knowledge.experimental_resolveInverseFunctionCall match {
        case "Runtime"     =>
          var inSize = IR_BasicMatrixOperations.getSize(inMatrix)
          IR_FunctionCall(IR_UnresolvedFunctionReference("invRT", IR_MatrixDatatype(inMatrix.datatype.resolveBaseDatatype, inSize._1, inSize._2)), args)
        case "Compiletime" =>
          inMatrix match {
            case x : IR_MatrixExpression                               => IR_CompiletimeInversion.inverse(x, Knowledge.experimental_matrixStructure, Knowledge.experimental_blocksize)
            case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_CompiletimeInversion.inverse(IR_MatrixNodeUtilities.accessToExpression(va), Knowledge.experimental_matrixStructure, Knowledge.experimental_blocksize)
            case _                                                     => Logger.error("argument of unexpected type: " + inMatrix.datatype)
          }
        case _             =>
          Logger.error("resolve inversion: type of inverse resolve " + Knowledge.experimental_resolveInverseFunctionCall + " not supported")
      }
  })
}

// resolve matrix operators to IR_MatrixExpressions
object IR_ResolveMatrixOperations extends DefaultStrategy("Resolve matrix operations to expressions") {

  def isEvaluatable(x : IR_Expression) : Boolean = {
    isMatrix(x) | isScalar(x) | isString(x)
  }

  // determine whether an expression is an access to a variable with type matrix or a matrix expression
  def isMatrix(x : IR_Expression) : Boolean = {
    x match {
      case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => true
      case IR_MatrixExpression(_, _, _)                     => true
      case _                                                => false
    }
  }

  // determine whether an expression is an access to a scalar variable or constant/value
  def isScalar(x : IR_Expression) : Boolean = {
    x match {
      case IR_VariableAccess(_, IR_RealDatatype | IR_IntegerDatatype | IR_DoubleDatatype | IR_FloatDatatype)                                        => true
      case (IR_IntegerConstant(_) | IR_DoubleConstant(_) | IR_FloatConstant(_) | IR_RealConstant(_))                                                => true
      case IR_HighDimAccess(_, _)                                                                                                                   => true
      case op @ (IR_Addition(_) | IR_Subtraction(_, _) | IR_Multiplication(_) | IR_Division(_, _)) if (op.datatype.isInstanceOf[IR_ScalarDatatype]) => true
      case _                                                                                                                                        => false
    }
  }

  def isString(x : IR_Expression) : Boolean = {
    x match {
      case IR_StringConstant(_)                    => true
      case IR_VariableAccess(_, IR_StringDatatype) => true
      case _                                       => false
    }
  }

  // split combined assignment: += to IR_Addition, *= to IR_Multiplication, /= to IR_Division, -= to IR_Subtraction
  this += new Transformation("split combined operators", {
    case IR_Assignment(dest @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)), src, "+=") =>
      IR_Assignment(dest, IR_Addition(dest, src))
    case IR_Assignment(dest @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)), src, "*=") =>
      IR_Assignment(dest, IR_Multiplication(ListBuffer[IR_Expression](dest, src)))
    case IR_Assignment(dest @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)), src, "-=") =>
      IR_Assignment(dest, IR_Subtraction(dest, src))
    case IR_Assignment(dest @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)), src, "/=") =>
      IR_Assignment(dest, IR_ElementwiseDivision(dest, src))
  })

  this += new Transformation("operators", {
    case mult @ IR_Multiplication(facs) if (facs.exists(f => isMatrix(f)) && facs.forall(f => isEvaluatable(f)))                              =>
      IR_BasicMatrixOperations.mult(mult)
    case add @ (IR_Addition(sums)) if (sums.exists(f => isMatrix(f)) && sums.forall(f => isEvaluatable(f)))                                   =>
      IR_BasicMatrixOperations.add(add)
    case eadd @ (IR_ElementwiseAddition(left, right)) if ((isMatrix(left) | isMatrix(right)) && isEvaluatable(left) && isEvaluatable(right))  =>
      IR_BasicMatrixOperations.add(eadd)
    case sub @ IR_Subtraction(left, right) if ((isMatrix(left) | isMatrix(right)) && isEvaluatable(left) && isEvaluatable(right))             =>
      IR_BasicMatrixOperations.sub(sub)
    case esub @ IR_ElementwiseSubtraction(left, right) if ((isMatrix(left) | isMatrix(right)) && isEvaluatable(left) && isEvaluatable(right)) =>
      IR_BasicMatrixOperations.sub(esub)
    case IR_ElementwiseMultiplication(left, right) if ((isMatrix(left) | isMatrix(right)) && isEvaluatable(left) && isEvaluatable(right))     =>
      IR_BasicMatrixOperations.elementwiseMultiplication(left, right)
    case IR_ElementwiseDivision(left, right) if ((isMatrix(left) | isMatrix(right)) && isEvaluatable(left) && isEvaluatable(right))           =>
      IR_BasicMatrixOperations.elementwiseDivision(left, right)
  })

  this += new Transformation("build-in functions", {
    // transpose a matrix
    case call @ IR_FunctionCall(_, args) if (call.name == "transpose" && args.forall(a => isEvaluatable(a))) =>
      if (args.length != 1)
        Logger.error("unexpected number of arguments: " + args.length + ", expected 1")
      if (!(args(0).isInstanceOf[IR_VariableAccess] && args(0).datatype.isInstanceOf[IR_MatrixDatatype]))
        Logger.error("unexpected argument type: " + args(0) + ", expected matrix")
      IR_BasicMatrixOperations.transpose(args(0).asInstanceOf[IR_VariableAccess])

    // dot product of two matrices
    case call @ IR_FunctionCall(_, args) if ((call.name == "dotProduct" || call.name == "dot") && args.forall(a => isEvaluatable(a))) =>
      if (args.length != 2)
        Logger.error("wrong number of arguments: " + args.length + ", expected 2")
      if (!isMatrix(args(0)) | !isMatrix(args(1)))
        Logger.error("wrong argument type, expected matrices")
      IR_BasicMatrixOperations.dotProduct(args(0), args(1))

    // cross product
    case call @ IR_FunctionCall(_, args) if ((call.name == "crossProduct" || call.name == "cross") && args.forall(a => isEvaluatable(a))) =>
      if (call.arguments.length != 2)
        Logger.error("wrong number of arguments: " + call.arguments.length + ", expected 2")
      IR_BasicMatrixOperations.crossProduct(args(0), args(1))

    // trace
    case call @ IR_FunctionCall(_, args) if (call.name == "trace" && args.forall(a => isEvaluatable(a))) =>
      if (call.arguments.length != 1)
        Logger.error("wrong number of arguments: " + call.arguments.length + ", expected 1 argument for trace call")
      IR_BasicMatrixOperations.trace(call.arguments(0))

    // matrix multiplication
    case call @ IR_FunctionCall(_, args) if (call.name == "matmult" && args.forall(a => isEvaluatable(a))) =>
      if (call.arguments.length < 2)
        Logger.error("expected at least 2 arguments for matrix multiplication, got: " + call.arguments.length)
      IR_BasicMatrixOperations.mult(IR_Multiplication(call.arguments))

    // receive an element value of a matrix
    case call @ IR_FunctionCall(_, args) if ((call.name == "getElement" || call.name == "get") && args.forall(a => isEvaluatable(a))) =>
      call.arguments match {
        case ListBuffer(matrix @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), offsetRows, offsetCols) if (isScalar(offsetRows) && isScalar(offsetCols)) =>
          if (call.arguments.length != 3)
            Logger.error("wrong number of arguments: " + call.arguments.length)
          IR_HighDimAccess(matrix, IR_ExpressionIndex(offsetRows, offsetCols))
        case _                                                                                                                                               => Logger.error("unexpected arguments:" + call.arguments + ", expected getElement(matrix,offsetRows,offsetCols) with matrix=access to matrix variable, offsetRows, offsetCols=Integer constant or access to integer variable")
      }

    // compare two matrices or scalars
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, ListBuffer(left : IR_Expression, right : IR_Expression, precision : IR_Expression))) if (call.name == "compare") =>
      IR_GenerateBasicMatrixOperations.compare(left, right, precision)

    // set a slice of a matrix to 'newValue'
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, args)) if (call.name == "setSlice" && args.forall(a => isEvaluatable(a))) =>
      call.arguments match {
        case ListBuffer(matrix @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)), offsetRows, offsetCols, nRows, nCols, newValue) if (args.forall(arg => isEvaluatable(arg))) =>
          if (isScalar(newValue))
            IR_GenerateBasicMatrixOperations.loopSetSubmatrixSc(matrix, offsetRows, offsetCols, nRows, nCols, newValue)
          else {
            newValue match {
              case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
                IR_GenerateBasicMatrixOperations.loopSetSubmatrixMat(va, matrix, offsetRows, offsetCols)
              case x @ IR_MatrixExpression(_, _, _)                      =>
                var decl = IR_MatrixNodeUtilities.expressionToDeclaration(x)
                ListBuffer[IR_Statement](decl, IR_GenerateBasicMatrixOperations.loopSetSubmatrixMat(IR_VariableAccess(decl), matrix, offsetRows, offsetCols))
              case _                                                     => Logger.error("form of newValue matrix not supported: " + newValue + ", expected variable access to matrix variable")
            }
          }
        case _                                                                                                                                                                    => Logger.error("unexpected arguments: " + call.arguments + ", expected setElement(matrix,offsetRows,offsetColumns,newValue), with matrix=access to matrix variable, (offsetRows, offsetCols, numberOfRows, numberOfColumns)=Integer constant or access to integer variable and newValue=access to variable or constant value")
      }

    // set an element to 'newValue'
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, _)) if ((call.name == "setElement" || call.name == "set") && call.arguments.forall(a => isEvaluatable(a))) =>
      IR_Assignment(IR_HighDimAccess(call.arguments(0), IR_ExpressionIndex(call.arguments(1), call.arguments(2))), call.arguments(3))

  })

  this += new Transformation("resolve rtfunctions", {

    case decl @ IR_VariableDeclaration(_, _, Some(src @ IR_FunctionCall(_, _)), _) if (src.name == "getSliceRT" | src.name == "detRT" | src.name == "invRT") =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)

    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), call @ IR_FunctionCall(_, args), _) if (call.name == "getSliceRT") =>
      IR_GenerateBasicMatrixOperations.loopCopySubmatrix(args(0), dest, args(1), args(2), args(3), args(4))

    case IR_Assignment(dest @ IR_VariableAccess(_, IR_RealDatatype | IR_IntegerDatatype | IR_DoubleDatatype | IR_FloatDatatype), call @ IR_FunctionCall(_, args), _) if (call.name == "detRT") =>
      args(0) match {
        case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
          IR_GenerateBasicMatrixOperations.determinant(va, dest)
        case x @ IR_MatrixExpression(_, _, _)                      =>
          var stmts = IR_Scope(Nil)
          var tmp_access = IR_VariableAccess("detTmp_" + IR_MatrixExpression.matTmpCounter, x.datatype)
          stmts.body += IR_VariableDeclaration(tmp_access, x)
          stmts.body += IR_GenerateBasicMatrixOperations.determinant(tmp_access, dest)
        case _                                                     => Logger.error("argument type not supported: " + args(0) + ", expected matrix expression or variable")
      }

    case IR_Assignment(dest @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)), call : IR_FunctionCall, _) if (call.name == "invRT") =>
      call.arguments(0) match {
        case x : IR_MatrixExpression                               =>
          var rt_exprToAccess = IR_VariableDeclaration(IR_MatrixDatatype(x.datatype.resolveBaseDatatype, x.rows, x.columns), "inverseTmp_" + IR_MatrixExpression.matTmpCounter, x)
          IR_MatrixExpression.matTmpCounter += 1
          IR_Scope(
            rt_exprToAccess,
            IR_GenerateRuntimeInversion.inverse(IR_VariableAccess(rt_exprToAccess), dest, Knowledge.experimental_matrixStructure)
          )
        case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_GenerateRuntimeInversion.inverse(va, dest, Knowledge.experimental_matrixStructure)
        case _                                                     => Logger.error("argument of unexpected type: " + call.arguments(0).datatype)
      }
  })
}

// resolve "Var matrix : Matrix<Datatype, rows, columns> = initialization" or split to declaration and assignment if convenient
object IR_ResolveMatrixDeclarations extends DefaultStrategy("Resolve matrix decl + initialization") {

  this += new Transformation("with constants", {
    // split to use std::fill later
    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _), _, Some(IR_FloatConstant(_) | IR_DoubleConstant(_) | IR_RealConstant(_) | IR_IntegerConstant(_)), _) =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)

    // split to use std::fill later
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
}

// resolve "matrix = expression"
object IR_ResolveMatrixAssignments extends DefaultStrategy("Resolve matrix assignments") {
  var debug = false

  // use std::fill for assignments of matrices with constants
  this += new Transformation("with constants", {
    case IR_Assignment(dest @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)), src @ (IR_RealConstant(_) | IR_DoubleConstant(_) | IR_IntegerConstant(_) | IR_FloatConstant(_)), "=")         =>
      IR_FunctionCall(IR_ExternalFunctionReference("std::fill", IR_UnitDatatype), ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + dest.datatype.asInstanceOf[IR_MatrixDatatype].resolveFlattendSize, src)) : IR_Statement
    case IR_Assignment(dest @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)), src @ IR_VariableAccess(_, IR_RealDatatype | IR_DoubleDatatype | IR_IntegerDatatype | IR_FloatDatatype), "=") =>
      IR_FunctionCall(IR_ExternalFunctionReference("std::fill", IR_UnitDatatype), ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + dest.datatype.asInstanceOf[IR_MatrixDatatype].resolveFlattendSize, src)) : IR_Statement
  })

  // assignment of a matrix with another matrix : copy other matrix
  this += new Transformation("with matrices", {
    case IR_Assignment(dest @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)), src @ (IR_MatrixExpression(_, _, _) | IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), "=") =>
      IR_GenerateBasicMatrixOperations.memcpyMatrix(dest, src)
  })
}

// simplify matrices e.g. neg(mat) to negated entries
object IR_SimplifyMatrices extends DefaultStrategy("Simplify matrices") {
  this += new Transformation("simplify", {
    case IR_Negative(m : IR_MatrixExpression)                               => m.expressions = m.expressions.map { y => IR_Negative(y) : IR_Expression }; m
    case IR_Negative(va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))) =>
      var m = IR_MatrixNodeUtilities.accessToExpression(va)
      m.expressions = m.expressions.map { y => IR_Negative(y) : IR_Expression };
      m
    case m @ IR_MatrixExpression(_, 1, 1)                                   => m.get(0, 0)
    case m @ IR_MatrixDatatype(dt, 1, 1)                                    => dt
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

// methods to transform certain types of nodes related to matrices
object IR_MatrixNodeUtilities {
  var tmpCounter = 0

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

  // copy a matrix from a IR_VariableAccess to a IR_MatrixExpression by building an expression of highDimAccesses
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

  // transform a matrix expression to a temporary variable
  def expressionToDeclaration(src : IR_MatrixExpression) : IR_VariableDeclaration = {
    var decl = IR_VariableDeclaration(IR_MatrixDatatype(src.datatype.resolveBaseDatatype, src.rows, src.columns), "expressionTmp_" + tmpCounter, src)
    tmpCounter += 1
    decl
  }

  // check for compiletime constance of a variable
  //TODO multiple variables within different scopes?
  //TODO where to start?
  def isCompiletimeConstant(name : String) : Boolean = {
    var cconst = true
    StateManager.findFirst[IR_Assignment](StateManager.root).filter(
      x => x.dest.isInstanceOf[IR_VariableAccess] && x.dest.asInstanceOf[IR_VariableAccess].name == name
    ) match {
      case None    =>
      case Some(_) => cconst = false
    }
    cconst
  }
}
