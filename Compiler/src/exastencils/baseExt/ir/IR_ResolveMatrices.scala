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
  var runtimeMethods = ListBuffer[String]("inverse", "inv", "det", "determinant", "deter", "getSlice")
  var extractMethodsLabel = "extractMethod"
  var extractMethodsCounter = 0

  // list to hold declarations of temporary variables from runtime method extraction
  var extractRuntimeTmps = ListBuffer[IR_Statement]()

  // label for operations ready to be resolved: all arguments are available
  var resolvableLabel = "resolveMatrix"

  // create new variables to extract runtime method calls and mark nodes for later replacement
  def extractAndAnnotate(src : IR_Expression) : ListBuffer[IR_Statement] = {
    var newStmts = ListBuffer[IR_Statement]()
    //TODO make findAll faster: search for certain nodes
    var rtfuncs = StateManager.findAll[IR_Expression](src).foreach({
      f => f match {
        case f @ (IR_Inverse(_,_,_) | IR_Determinant(_,_) | IR_GetSlice(_))  if(f.hasAnnotation(resolvableLabel)) =>
          val decl = IR_VariableDeclaration(f.datatype, "extractTmp_" + extractMethodsCounter, Duplicate(f))
          f.annotate(extractMethodsLabel, extractMethodsCounter)
          extractMethodsCounter += 1
          newStmts += decl
        case other => other
      }
    })
    newStmts
  }

  // extract runtime methods in right sides of statements to helper variables if chained calls are found
  this += new Transformation("mark and extract runtime method calls", {
    case stmt @ IR_Assignment(_, src, _) if ((extractRuntimeTmps ++= extractAndAnnotate(src)).isEmpty == false)                     =>
      var out = Duplicate(extractRuntimeTmps) += stmt
      extractRuntimeTmps.clear()
      out
    case decl @ IR_VariableDeclaration(_, _, Some(init), _) if ((extractRuntimeTmps ++= extractAndAnnotate(init)).isEmpty == false) =>
      var out = Duplicate(extractRuntimeTmps) += decl
      extractRuntimeTmps.clear()
      out
    case expstmt @ IR_ExpressionStatement(src) if ((extractRuntimeTmps ++= extractAndAnnotate(src)).isEmpty == false)               =>
      var out = Duplicate(extractRuntimeTmps) += expstmt
      extractRuntimeTmps.clear()
      out
  })

  // replace calls to runtime methods with variable accesses to helper variables
  this += new Transformation("replace runtime method calls", {
        //TODO inverse,det,getslice supertype
    case exp : IR_FunctionCall if (exp.hasAnnotation(extractMethodsLabel)) =>
      IR_VariableAccess("extractTmp_" + exp.popAnnotationAs[Int](extractMethodsLabel), exp.function.returnType)
  })

}

// resolve matrix operators to IR_MatrixExpressions
object IR_ResolveMatrixOperations extends DefaultStrategy("Resolve matrix operations to expressions") {
  // label for operations that are ready to be resolved: all arguments are available
  var resolvableLabel = "resolveMatrix"

  // all built-in methods
  var matrixFunctions = ListBuffer[String]("getSlice", "det", "deter", "determinant", "inv", "inverse", "transpose", "dot", "dotProduct", "cross", "crossProduct", "trace", "matmult", "get", "getElement", "set", "SetElement", "setSlice")

  // hold temporary variable accessses
  var temporaries = ListBuffer[IR_VariableAccess]()

  // check if an argument is ready to be evaluated
  //TODO other datatypes?
  def isEvaluatable(x : IR_Expression) : Boolean = {
    isMatrix(x) | isScalar(x) | isString(x)
  }

  // determine whether an expression is an access to a variable with type matrix or a matrix expression
  def isMatrix(x : IR_Expression) : Boolean = {
    x match {
      case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))                                                 => true
      case IR_MatrixExpression(_, _, _)                                                                     => true
      case IR_VariableAccess(_, IR_ReferenceDatatype(innerDt)) if (innerDt.isInstanceOf[IR_MatrixDatatype]) => true
      case _                                                                                                => false
    }
  }

  // determine whether an expression is an access to a scalar variable or constant/value
  def isScalar(x : IR_Expression) : Boolean = {
    x match {
      case IR_VariableAccess(_, IR_RealDatatype | IR_IntegerDatatype | IR_DoubleDatatype | IR_FloatDatatype)                                                                           => true
      case (IR_IntegerConstant(_) | IR_DoubleConstant(_) | IR_FloatConstant(_) | IR_RealConstant(_))                                                                                   => true
      case IR_HighDimAccess(_, _)                                                                                                                                                      => true
      case op @ (IR_Addition(_) | IR_Subtraction(_, _) | IR_Multiplication(_) | IR_Division(_, _) | IR_Modulo(_, _) | IR_Power(_, _)) if (op.datatype.isInstanceOf[IR_ScalarDatatype]) => true
      case minmax @ (IR_Minimum(_) | IR_Maximum(_)) if (minmax.datatype.isInstanceOf[IR_ScalarDatatype])                                                                               => true
      case IR_VariableAccess(_, IR_ReferenceDatatype(innerDt)) if (innerDt.isInstanceOf[IR_ScalarDatatype])                                                                            => true
      case IR_ArrayAccess(_, _, _)                                                                                                                                                     => true
      case IR_MultiDimArrayAccess(_, _)                                                                                                                                                => true
      case IR_Negative(x) if (x.datatype.isInstanceOf[IR_ScalarDatatype])                                                                                                              => true
      case _                                                                                                                                                                           => false
    }
  }

  def isString(x : IR_Expression) : Boolean = {
    x match {
      case IR_StringConstant(_)                    => true
      case IR_VariableAccess(_, IR_StringDatatype) => true
      case _                                       => false
    }
  }

  // replace function calls to matrix methods to dedicated nodes so they dont appear in function call tree and are easy to recognize
  this += new Transformation("replace function calls to matrix method nodes", {
    case f @ IR_FunctionCall(_, args) if (f.name == "getSlice")                                            =>
      IR_GetSlice(args)
    case f @ IR_FunctionCall(_, args) if (f.name == "inverse")                                             =>
      var determineStructure = "no"
      var matrixStructure = "Filled"
      var blocksize = -1
      var matrixStructure_A = "no schur"
      var blocksize_A = -1
      // check arguments 2 to 5
      f.arguments.length match {
        case 0 =>
          Logger.error("no argument passed, expected a matrix!")
        case 1 =>
        case _ =>
          f.arguments(1) match {
            case IR_StringConstant(s @ ("DetermineRuntime" | "DetermineCompiletime")) => determineStructure = s
            case IR_StringConstant(s @ ("Diagonal"))                                  => matrixStructure = s
            case IR_StringConstant(s @ "Blockdiagonal")                               =>
              matrixStructure = s
              if (f.arguments.length != 3)
                Logger.error("Blockdiagonal matrix specified but no blocksize given!")
              f.arguments(2) match {
                case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                case IR_IntegerConstant(c)                        => blocksize = c.toInt
                case _                                            => Logger.error(s"blocksize of unexpected type: ${ f.arguments }, expected integer constant or access to integer variable")
              }
            case IR_StringConstant(s @ "Schur")                                           =>
              matrixStructure = s
              if (f.arguments.length < 3)
                Logger.error("schur matrix specified but no blocksize given!")
              f.arguments(2) match {
                case IR_VariableAccess(_, IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                case IR_IntegerConstant(c)                    => blocksize = c.toInt
                case _                                        => Logger.error(s"blocksize is of unexpected type: ${ f.arguments }, expected integer constant or access to integer variable")
              }
              if (f.arguments.length < 4) {
                matrixStructure_A = "Filled"
              }
              else {
                f.arguments(3) match {
                  case IR_StringConstant(s @ ("Filled" | "Diagonal")) => matrixStructure_A = s
                  case IR_StringConstant(s @ "Blockdiagonal")         =>
                    matrixStructure_A = s
                    if (f.arguments.length != 5)
                      Logger.error("Blockdiagonal specified for A matrix in schur structure matrix but no blocksize given!")
                    f.arguments(4) match {
                      case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                      case IR_IntegerConstant(c)                        => blocksize_A = c.toInt
                      case _                                            => Logger.error(s"blocksize for blockdiagonal matrix A is of unexpected type: ${ f.arguments }, expected integer constant or access to integer variable")

                    }
                  case _                                              => Logger.error(s"unexpected type for upper left matrix in schur structure matrix: ${ f.arguments(3) }, expected Filled or Diagonal or Blockdiagonal")
                }
              }
            case _                                                                    => Logger.error(s"unexpected argument combination: ${ f.arguments }, expected: 'Determine' or 'Filled' or 'Diagonal' without additional arguments or 'Blockdiagonal' with a blocksize as 3rd argument or 'Schur' with a blocksize which specifies the width of the lower right matrix D as 3rd argument and additionally the structure of the upper left matrix A as 4th argument and its blocksize as 5th in case of a blockdiagonal matrix for A;" +
              "in short: " +
              "inverse(mat) or inverse(mat, Filled) or inverse(mat, Determine) or inverse(mat,Diagonal) or inverse(mat, Blockdiagonal, blocksize) or inverse(mat, Schur, blocksize) or inverse(mat, Schur, blocksize, Filled) or inverse(mat, Schur, blocksize, Diagonal) or inverse(mat, Schur, blocksize, Blockdiagonal, blocksize_A")
          }
      }
      // check for first argument: should be a matrix
      if (!((f.arguments(0).isInstanceOf[IR_VariableAccess] && f.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) || f.arguments(0).isInstanceOf[IR_MatrixExpression])) {
        Logger.error(s"expected matrix variable or expression as first argument, got ${ f.arguments(0) }")
      }
      IR_Inverse(args(0), (matrixStructure, blocksize, matrixStructure_A, blocksize_A))
    case f @ IR_FunctionCall(_, args) if (f.name == "det" || f.name == "deter" || f.name == "determinant") =>
      if (IR_BasicMatrixOperations.getSize(args(0))._1 > 5) {
        IR_Determinant(args(0), true)
      } else {
        IR_Determinant(args(0))
      }
    case f @ IR_FunctionCall(_, args) if (f.name == "transpose")                                           =>
      IR_Transpose(args(0))
    case f @ IR_FunctionCall(_, args) if (f.name == "crossProduct" || f.name == "cross")                   =>
      IR_CrossProduct(args)
    case f @ IR_FunctionCall(_, args) if (f.name == "DotProduct" || f.name == "dot")                       =>
      IR_DotProduct(args)
    case f @ IR_FunctionCall(_, args) if (f.name == "trace")                                               =>
      IR_Trace(args(0))
    case f @ IR_FunctionCall(_, args) if (f.name == "get" || f.name == "getElement")                       =>
      IR_GetElement(args)
    case f @ IR_FunctionCall(_, args) if (f.name == "set" || f.name == "setElement")                       =>
      IR_SetElement(args)
    case IR_ExpressionStatement(f @ IR_FunctionCall(_, args)) if (f.name == "setSlice")                                            =>
      IR_SetSlice(args)
  })

  // mark operations with evaluatable arguments
  this += new Transformation("mark operations with evaluatable arguments", {
        //TODO match on supertype? -> introduce supertype
    case mult @ IR_Multiplication(facs) if (facs.exists(f => isMatrix(f)) && facs.forall(f => isEvaluatable(f)))                                                   =>
      mult.annotate(resolvableLabel)
      mult
    case add @ (IR_Addition(sums)) if (sums.exists(f => isMatrix(f)) && sums.forall(f => isEvaluatable(f)))                                                        =>
      add.annotate(resolvableLabel)
      add
    case binOp @ IR_ElementwiseSubtraction(_, _) if ((isMatrix(binOp.left) | isMatrix(binOp.right)) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))    =>
      binOp.annotate(resolvableLabel)
      binOp
    case binOp @ IR_Subtraction(_, _) if ((isMatrix(binOp.left) | isMatrix(binOp.right)) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))               =>
      binOp.annotate(resolvableLabel)
      binOp
    case binOp @ IR_ElementwiseMultiplication(_, _) if ((isMatrix(binOp.left) | isMatrix(binOp.right)) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right)) =>
      binOp.annotate(resolvableLabel)
      binOp
    case binOp @ IR_ElementwiseDivision(_, _) if ((isMatrix(binOp.left) | isMatrix(binOp.right)) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))       =>
      binOp.annotate(resolvableLabel)
      binOp
    case binOp @ IR_ElementwiseAddition(_, _) if ((isMatrix(binOp.left) | isMatrix(binOp.right)) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))       =>
      binOp.annotate(resolvableLabel)
      binOp
      //TODO match on supertype?
    case fc @ IR_Inverse(arg, _, _) if (isEvaluatable(arg))                                                                                                        =>
      fc.annotate(resolvableLabel)
      fc
    case fc @ IR_Determinant(arg, _) if (isEvaluatable(arg))                                                                                                       =>
      fc.annotate(resolvableLabel)
      fc
    case fc @ IR_GetSlice(args) if (args.forall(arg => isEvaluatable(arg)))                                                                                        =>
      fc.annotate(resolvableLabel)
      fc
    case fc @ IR_SetSlice(args) if (args.forall(arg => isEvaluatable(arg)))                                                                                        =>
      fc.annotate(resolvableLabel)
      fc
    case fc @ IR_DotProduct(args) if (args.forall(arg => isEvaluatable(arg)))                                                                                      =>
      fc.annotate(resolvableLabel)
      fc
    case fc @ IR_CrossProduct(args) if (args.forall(arg => isEvaluatable(arg)))                                                                                    =>
      fc.annotate(resolvableLabel)
      fc
    case fc @ IR_SetElement(args) if (args.forall(arg => isEvaluatable(arg)))                                                                                      =>
      fc.annotate(resolvableLabel)
      fc
    case fc @ IR_GetElement(args) if (args.forall(arg => isEvaluatable(arg)))                                                                                      =>
      fc.annotate(resolvableLabel)
      fc
    case fc @ IR_SetSlice(args) if (args.forall(arg => isEvaluatable(arg)))                                                                                        =>
      fc.annotate(resolvableLabel)
      fc
  })

  /*
  // if a condition for runtime execution of the methods slice, determinant, inverse is met
  // -> replace function call with '*RT' call (to extract them to helper variables)
  // else resolve the call directly like the other built-in matrix functions
  //TODO old remove
  this += new Transformation("branch methods that can be executed at runtime and compiletime", {
    // slice a matrix: test for compiletime constants in arguments and :
    // replace with reference (not constant arguments -> runtime slice)
    // or resolve (constant -> compiletime slice)
    case call @ IR_FunctionCall(_, args) if (call.hasAnnotation(resolvableLabel) && call.name == "getSlice") =>
      call.removeAnnotation(resolvableLabel)
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
        case t : Throwable           => Logger.error(s"unexpected exception: $t")
      }
      if (evaluatable)
        IR_BasicMatrixOperations.copySubMatrix(args(0), offsetRows_asInt, offsetCols_asInt, nRows_asInt, nCols_asInt)
      else {
        var size = IR_BasicMatrixOperations.getSize(args(0))
        IR_FunctionCall(IR_UnresolvedFunctionReference("getSliceRT", IR_MatrixDatatype(args(0).datatype.resolveBaseDatatype, size._1, size._2)), args)
      }

    // determinant: replace with reference if matrix is large, resolve if matrix is small
    case call @ IR_FunctionCall(_, args) if (call.hasAnnotation(resolvableLabel) && (call.name == "det" | call.name == "deter" | call.name == "determinant")) =>
      call.removeAnnotation(resolvableLabel)
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
          case _                                                     => Logger.error(s"argument type not supported: $args(0), expected matrix expression or variable")
        }
      }

    // inverse : read arguments, classify matrix, resolve if compiletime is specified in knowledge, replace with reference if runtime
    case call @ IR_FunctionCall(_, args) if (call.hasAnnotation(resolvableLabel) && (call.name == "inv" | call.name == "inverse")) =>
      call.removeAnnotation(resolvableLabel)
      var matrixStructure = "Filled"
      var blocksize = -1
      var matrixStructure_A = "no schur"
      var blocksize_A = -1
      // check arguments 2 to 5
      call.arguments.length match {
        case 0 =>
          Logger.error("no argument passed, expected a matrix!")
        case 1 =>
        case _ =>
          matrixStructure = call.arguments(1).asInstanceOf[IR_StringConstant].value
          call.arguments(1) match {
            case IR_StringConstant("Determine" | "DetermineRuntime" | "DetermineCompiletime" | "Filled" | "Diagonal") =>
            case IR_StringConstant("Blockdiagonal")                                                                   =>
              if (call.arguments.length != 3)
                Logger.error("Blockdiagonal matrix specified but no blocksize given!")
              call.arguments(2) match {
                case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                case IR_IntegerConstant(c)                        => blocksize = c.toInt
                case _                                            => Logger.error(s"blocksize of unexpected type: ${ call.arguments }, expected integer constant or access to integer variable")
              }
            case IR_StringConstant("Schur")                                                                           =>
              if (call.arguments.length < 3)
                Logger.error("schur matrix specified but no blocksize given!")
              call.arguments(2) match {
                case IR_VariableAccess(_, IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                case IR_IntegerConstant(c)                    => blocksize = c.toInt
                case _                                        => Logger.error(s"blocksize is of unexpected type: ${ call.arguments }, expected integer constant or access to integer variable")
              }
              if (call.arguments.length < 4) {
                matrixStructure_A = "Filled"
              }
              else {
                call.arguments(3) match {
                  case IR_StringConstant(s @ ("Filled" | "Diagonal")) => matrixStructure_A = s
                  case IR_StringConstant(s @ "Blockdiagonal")         =>
                    matrixStructure_A = s
                    if (call.arguments.length != 5)
                      Logger.error("Blockdiagonal specified for A matrix in schur structure matrix but no blocksize given!")
                    call.arguments(4) match {
                      case IR_VariableAccess(_, d @ IR_IntegerDatatype) => Logger.error("Variables as blocksizes not yet implemented")
                      case IR_IntegerConstant(c)                        => blocksize_A = c.toInt
                      case _                                            => Logger.error(s"blocksize for blockdiagonal matrix A is of unexpected type: ${ call.arguments }, expected integer constant or access to integer variable")

                    }
                  case _                                              => Logger.error(s"unexpected type for upper left matrix in schur structure matrix: ${ call.arguments(3) }, expected Filled or Diagonal or Blockdiagonal")
                }
              }
            case _                                                                                                    => Logger.error(s"unexpected argument combination: ${ call.arguments }, expected: 'Determine' or 'Filled' or 'Diagonal' without additional arguments or 'Blockdiagonal' with a blocksize as 3rd argument or 'Schur' with a blocksize which specifies the width of the lower right matrix D as 3rd argument and additionally the structure of the upper left matrix A as 4th argument and its blocksize as 5th in case of a blockdiagonal matrix for A;" +
              "in short: " +
              "inverse(mat) or inverse(mat, Filled) or inverse(mat, Determine) or inverse(mat,Diagonal) or inverse(mat, Blockdiagonal, blocksize) or inverse(mat, Schur, blocksize) or inverse(mat, Schur, blocksize, Filled) or inverse(mat, Schur, blocksize, Diagonal) or inverse(mat, Schur, blocksize, Blockdiagonal, blocksize_A")
          }
      }

      // check for first argument: should be a matrix
      if (!((call.arguments(0).isInstanceOf[IR_VariableAccess] && call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) || call.arguments(0).isInstanceOf[IR_MatrixExpression])) {
        Logger.error(s"expected matrix variable or expression as first argument, got ${ call.arguments(0) }")
      }
      var inMatrix = call.arguments(0)

      if (matrixStructure == "DetermineRuntime") {
        if (Knowledge.experimental_resolveInverseFunctionCall == "Compiletime") Logger.error("cannot determine structure at runtime and invert at compiletime!")
        Logger.warn("determining matrix structure for inversion at runtime")
        var inSize = IR_BasicMatrixOperations.getSize(inMatrix)
        IR_FunctionCall(IR_UnresolvedFunctionReference("determineInvRT", IR_MatrixDatatype(inMatrix.datatype.resolveBaseDatatype, inSize._1, inSize._2)), args)
      } else {
        // structure is to determine at compiletime
        if (matrixStructure == "DetermineCompiletime" || matrixStructure == "Determine") {
          Logger.warn("determining matrix structure for inversion at compiletime")
          inMatrix match {
            case va @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)) =>
              var foundDecls = StateManager.findAll[IR_VariableDeclaration]().filter(d => d.name == name)
              foundDecls.length match {
                case 0 => Logger.error(s"could not localize declaration of: $va")
                case 1 =>
                  var decl = foundDecls(0)

                  decl.initialValue match {
                    case None                                   => Logger.error("trying to classify not initialized matrix variable at compiletime!")
                    case Some(x @ IR_MatrixExpression(_, _, _)) =>
                      //TODO what if there are assignments between declaration and inverse call -> not compiletime constant, inverse call executes nevertheless
                      // -> need to find all accesses to that matrix variable to make sure its compiletime constant -> there must not be any modifications to that matrix variable
                      if (IR_MatrixNodeUtilities.notWrittenTo(name)) {
                        var structureInformation = IR_DetermineMatrixStructure.isOfStructure(x)
                        matrixStructure = structureInformation._1
                        blocksize = structureInformation._2
                        matrixStructure_A = structureInformation._3
                        blocksize_A = structureInformation._4
                      }
                      else
                        Logger.error("found assignment to matrix input that was to classify, cannot classify non compiletime constant matrices!")
                    case _                                      => Logger.error(s"unexpected initialization value: ${ decl.initialValue }, expected matrix expression!")
                  }
                case _ => Logger.error(s"thought declarations should be unique, found multiple declarations with name: $name")
              }

            case x : IR_MatrixExpression =>
              var structureInformation = IR_DetermineMatrixStructure.isOfStructure(x)
              matrixStructure = structureInformation._1
              blocksize = structureInformation._2
              matrixStructure_A = structureInformation._3
              blocksize_A = structureInformation._4
          }
        }

        Logger.warn(s"Inverting with the following configuration: ${ Knowledge.experimental_resolveInverseFunctionCall }, ${ matrixStructure }, ${ blocksize }, ${ matrixStructure_A },${ blocksize_A }")

        // branch to specific inversions depending on when to invert and the type of matrix to invert
        Knowledge.experimental_resolveInverseFunctionCall match {
          case "Runtime"     =>
            var inSize = IR_BasicMatrixOperations.getSize(inMatrix)
            IR_FunctionCall(IR_UnresolvedFunctionReference("invRT", IR_MatrixDatatype(inMatrix.datatype.resolveBaseDatatype, inSize._1, inSize._2)), ListBuffer[IR_Expression](inMatrix, matrixStructure, blocksize, matrixStructure_A, blocksize_A))
          case "Compiletime" =>
            inMatrix match {
              case x : IR_MatrixExpression                               => IR_CompiletimeInversion.inverse(x, matrixStructure, blocksize, matrixStructure_A, blocksize_A)
              case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_CompiletimeInversion.inverse(IR_MatrixNodeUtilities.accessToExpression(va), matrixStructure, blocksize, matrixStructure_A, blocksize_A)
              case _                                                     => Logger.error(s"argument of unexpected type: ${ inMatrix.datatype }")
            }
          case _             =>
            Logger.error(s"resolve inversion: type of inverse resolve ${ Knowledge.experimental_resolveInverseFunctionCall } not supported")
        }
      }
  })
*/
  def findTemporaries(src : IR_Expression) : List[IR_VariableAccess] = {
    val pattern = "^extractTmp_".r
    StateManager.findAll[IR_VariableAccess](src).filter(va => va.name match {
      case pattern(_) => true
      case _          => false
    })
  }

  object IR_InlineTemporaries extends DefaultStrategy("inline initialization expressions of temporaries where possible") {
    this += new Transformation("inline", {
      case tmp : IR_VariableAccess =>
        var decl = StateManager.findFirst[IR_VariableDeclaration]().getOrElse(Logger.error(s"decl of ${ tmp.name } not found"))
        var init = decl.initialValue.getOrElse(Logger.error(s"temporary ${ decl.name } not initialized"))
        init match {
          case inv : IR_Inverse if (Knowledge.experimental_resolveInverseFunctionCall == "Compiletime") =>
            if (inv.determineStructure == "Compiletime") {
              Logger.warn("determining matrix structure for inversion at compiletime")
              inv.arg match {
                case va @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)) =>
                  var foundDecls = StateManager.findAll[IR_VariableDeclaration]().filter(d => d.name == name)
                  foundDecls.length match {
                    case 0 => Logger.error(s"could not localize declaration of: $va")
                    case 1 =>
                      var decl = foundDecls(0)

                      decl.initialValue match {
                        case None                                   => Logger.error("trying to classify not initialized matrix variable at compiletime!")
                        case Some(x @ IR_MatrixExpression(_, _, _)) =>
                          //TODO what if there are assignments between declaration and inverse call -> not compiletime constant, inverse call executes nevertheless
                          // -> need to find all accesses to that matrix variable to make sure its compiletime constant -> there must not be any modifications to that matrix variable
                          //TODO collector check
                          if (IR_MatrixNodeUtilities.notWrittenTo(name)) {
                            inv.structureInformation = IR_DetermineMatrixStructure.isOfStructure(x)
                          }
                          else
                            Logger.error("found assignment to matrix input that was to classify, cannot classify non compiletime constant matrices!")
                        case _                                      => Logger.error(s"unexpected initialization value: ${ decl.initialValue }, expected matrix expression!")
                      }
                    case _ => Logger.error(s"thought declarations should be unique, found multiple declarations with name: $name")
                  }

                case x : IR_MatrixExpression =>
                  inv.structureInformation = IR_DetermineMatrixStructure.isOfStructure(x)
              }
            }
            Logger.warn(s"Inverting with the following configuration: ${ Knowledge.experimental_resolveInverseFunctionCall }, ${ inv.structureInformation }")
            inv.arg = inv.arg match {
              case x : IR_MatrixExpression                               => x
              case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_MatrixNodeUtilities.accessToExpression(va)
              case _                                                     => Logger.error(s"argument of unexpected type: ${ inv.arg.datatype }")
            }
            IR_CompiletimeInversion.inverse(inv.arg.asInstanceOf[IR_MatrixExpression], inv.structureInformation)

          case det : IR_Determinant if (!det.resolveAtRuntime) =>
            IR_BasicMatrixOperations.smallMatrixDeterminant(det.arg.asInstanceOf[IR_MatrixExpression])

          case IR_GetSlice(args) =>
            var evaluatable = true
            var args_asInt = ListBuffer[Int]()
            try {
              for (i <- 1 until 5) args_asInt += IR_SimplifyExpression.evalIntegral(args(i)).toInt
            } catch {
              case e : EvaluationException => evaluatable = false
              case t : Throwable           => Logger.error(s"unexpected exception: $t")
            }
            if (evaluatable)
              IR_BasicMatrixOperations.copySubMatrix(args(0), args_asInt(0), args_asInt(1), args_asInt(2), args_asInt(3))
            else {
             tmp
            }
        }
    })
  }

  this += new Transformation("resolve temporaries", {
    case assign @ IR_Assignment(_, src, _) if ((temporaries ++= findTemporaries(src)).isEmpty == false) =>
      //TODO verändert check
      temporaries.foreach(tmp => {
        IR_InlineTemporaries(Some(tmp))
      })
      temporaries.clear()
      assign
  })

  // split combined assignment: += to IR_Addition, *= to IR_Multiplication, /= to IR_Division, -= to IR_Subtraction
  // TODO NO IT
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

  // resolve operators to IR_MatrixExpressions if arguments are available
  this += new Transformation("operators", {
    case mult @ IR_Multiplication(_) if (mult.hasAnnotation(resolvableLabel))                        =>
      mult.removeAnnotation("markResolvableLabel")
      IR_BasicMatrixOperations.mult(mult)
    case add @ (IR_Addition(_)) if (add.hasAnnotation(resolvableLabel))                              =>
      add.removeAnnotation("markResolvableLabel")
      IR_BasicMatrixOperations.add(add)
    case eadd @ (IR_ElementwiseAddition(_, _)) if (eadd.hasAnnotation(resolvableLabel))              =>
      eadd.removeAnnotation("markResolvableLabel")
      IR_BasicMatrixOperations.add(eadd)
    case sub @ IR_Subtraction(_, _) if (sub.hasAnnotation(resolvableLabel))                          =>
      sub.removeAnnotation("markResolvableLabel")
      IR_BasicMatrixOperations.sub(sub)
    case esub @ IR_ElementwiseSubtraction(_, _) if (esub.hasAnnotation(resolvableLabel))             =>
      esub.removeAnnotation("markResolvableLabel")
      IR_BasicMatrixOperations.sub(esub)
    case emult @ IR_ElementwiseMultiplication(left, right) if (emult.hasAnnotation(resolvableLabel)) =>
      emult.removeAnnotation("markResolvableLabel")
      IR_BasicMatrixOperations.elementwiseMultiplication(left, right)
    case ediv @ IR_ElementwiseDivision(left, right) if (ediv.hasAnnotation(resolvableLabel))         =>
      ediv.removeAnnotation("markResolvableLabel")
      IR_BasicMatrixOperations.elementwiseDivision(left, right)
  })

  /*
  // resolve built-in compiletime functions if arguments are available
  //TODO old remove
  this += new Transformation("built-in functions", {
    // transpose a matrix
    case call @ IR_FunctionCall(_, args) if (call.hasAnnotation(resolvableLabel) && call.name == "transpose") =>
      if (args.length != 1)
        Logger.error(s"unexpected number of arguments: ${ args.length }, expected 1")
      if (!(args(0).isInstanceOf[IR_VariableAccess] && args(0).datatype.isInstanceOf[IR_MatrixDatatype]))
        Logger.error(s"unexpected argument type: ${ args(0) }, expected matrix")
      IR_BasicMatrixOperations.transpose(args(0).asInstanceOf[IR_VariableAccess])

    // dot product of two matrices
    case call @ IR_FunctionCall(_, args) if (call.hasAnnotation(resolvableLabel) && (call.name == "dotProduct" || call.name == "dot")) =>
      if (args.length != 2)
        Logger.error(s"wrong number of arguments: ${ args.length }, expected 2")
      if (!isMatrix(args(0)) | !isMatrix(args(1)))
        Logger.error("wrong argument type, expected matrices")
      IR_BasicMatrixOperations.dotProduct(args(0), args(1))

    // cross product
    case call @ IR_FunctionCall(_, args) if (call.hasAnnotation(resolvableLabel) && (call.name == "crossProduct" || call.name == "cross")) =>
      if (call.arguments.length != 2)
        Logger.error(s"wrong number of arguments: ${ call.arguments.length }, expected 2")
      IR_BasicMatrixOperations.crossProduct(args(0), args(1))

    // trace
    case call @ IR_FunctionCall(_, args) if (call.hasAnnotation(resolvableLabel) && call.name == "trace") =>
      if (call.arguments.length != 1)
        Logger.error(s"wrong number of arguments: ${ call.arguments.length }, expected 1 argument for trace call")
      IR_BasicMatrixOperations.trace(call.arguments(0))

    // matrix multiplication
    case call @ IR_FunctionCall(_, args) if (call.hasAnnotation(resolvableLabel) && call.name == "matmult") =>
      if (call.arguments.length < 2)
        Logger.error(s"expected at least 2 arguments for matrix multiplication, got: ${ call.arguments.length }")
      IR_BasicMatrixOperations.mult(IR_Multiplication(call.arguments))

    // receive an element value of a matrix
    case call @ IR_FunctionCall(_, args) if (call.hasAnnotation(resolvableLabel) && (call.name == "getElement" || call.name == "get")) =>
      call.arguments match {
        case ListBuffer(matrix @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), offsetRows, offsetCols) if (isScalar(offsetRows) && isScalar(offsetCols)) =>
          if (call.arguments.length != 3)
            Logger.error(s"wrong number of arguments: ${ call.arguments.length }")
          IR_HighDimAccess(matrix, IR_ExpressionIndex(offsetRows, offsetCols))
        case _                                                                                                                                               => Logger.error("unexpected arguments:" + call.arguments + ", expected getElement(matrix,offsetRows,offsetCols) with matrix=access to matrix variable, offsetRows, offsetCols=Integer constant or access to integer variable")
      }

    // compare two matrices or scalars
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, ListBuffer(left : IR_Expression, right : IR_Expression, precision : IR_Expression))) if (call.name == "compare") =>
      IR_GenerateBasicMatrixOperations.compare(left, right, precision)

    // set a slice of a matrix to 'newValue' (matrix or scalar value)
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, args)) if (call.hasAnnotation(resolvableLabel) && call.name == "setSlice") =>
      call.arguments match {
        case ListBuffer(matrix @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)), offsetRows, offsetCols, nRows, nCols, newValue) =>
          if (isScalar(newValue))
            IR_GenerateBasicMatrixOperations.loopSetSubmatrixSc(matrix, offsetRows, offsetCols, nRows, nCols, newValue)
          else {
            var insize = IR_BasicMatrixOperations.getSize(newValue)
            newValue match {
              case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
                IR_GenerateBasicMatrixOperations.loopSetSubmatrixMat(va, matrix, IR_IntegerConstant(insize._1), IR_IntegerConstant(insize._2), offsetRows, offsetCols)
              case x @ IR_MatrixExpression(_, _, _)                      =>
                var decl = IR_MatrixNodeUtilities.expressionToDeclaration(x)
                ListBuffer[IR_Statement](decl, IR_GenerateBasicMatrixOperations.loopSetSubmatrixMat(IR_VariableAccess(decl), matrix, IR_IntegerConstant(insize._1), IR_IntegerConstant(insize._2), offsetRows, offsetCols))
              case _                                                     => Logger.error(s"form of newValue matrix not supported: ${ newValue }, expected variable access to matrix variable")
            }
          }
        case _                                                                                                                        => Logger.error(s"unexpected arguments: ${ call.arguments }, expected setElement(matrix,offsetRows,offsetColumns,newValue), with matrix=access to matrix variable, (offsetRows, offsetCols, numberOfRows, numberOfColumns)=Integer constant or access to integer variable and newValue=access to variable or constant value")
      }

    // set an element to 'newValue'
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, _)) if (call.hasAnnotation(resolvableLabel) && (call.name == "setElement" || call.name == "set")) =>
      IR_Assignment(IR_HighDimAccess(call.arguments(0), IR_ExpressionIndex(call.arguments(1), call.arguments(2))), call.arguments(3))

  })
*/


/*
  // replace statements with runtime functions with generated code which executes  the operation at runtime
  // TODO old remove
  this += new Transformation("resolve rtfunctions", {

    case decl @ IR_VariableDeclaration(_, _, Some(src @ IR_FunctionCall(_, _)), _) if (IR_HandleRuntimeMatrices.runtimeMethods.contains(src.name)) =>
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
        case _                                                     => Logger.error(s"argument type not supported: ${ args(0) }, expected matrix expression or variable")
      }

    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), call : IR_FunctionCall, _) if (call.name == "invRT") =>
      call.arguments(0) match {
        case x : IR_MatrixExpression                               =>
          var rt_exprToAccess = IR_VariableDeclaration(IR_MatrixDatatype(x.datatype.resolveBaseDatatype, x.rows, x.columns), "inverseTmp_" + IR_MatrixExpression.matTmpCounter, x)
          IR_MatrixExpression.matTmpCounter += 1
          IR_Scope(
            rt_exprToAccess,
            IR_GenerateRuntimeInversion.inverse(IR_VariableAccess(rt_exprToAccess), dest, call.arguments(1), call.arguments(2), call.arguments(3), call.arguments(4))
          )
        case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_GenerateRuntimeInversion.inverse(va, dest, call.arguments(1), call.arguments(2), call.arguments(3), call.arguments(4))
        case _                                                     => Logger.error(s"argument of unexpected type: ${ call.arguments(0).datatype }")
      }

    case IR_Assignment(dest @ IR_VariableAccess(name, IR_MatrixDatatype(_, _, _)), call : IR_FunctionCall, _) if (call.name == "determineInvRT") =>
      var newstmts = ListBuffer[IR_Statement]()
      var inMatrix = call.arguments(0) match {
        case x @ IR_MatrixExpression(_, _, _) =>
          var decl = IR_MatrixNodeUtilities.expressionToDeclaration(x)
          newstmts += decl
          IR_VariableAccess(Duplicate(decl))
        case va : IR_VariableAccess           => va
        case _                                => Logger.error(s"unexpected argument type: ${ call.arguments(0) }")
      }
      IR_GenerateRuntimeInversion.inverseBranchAtRuntime(inMatrix, name, dest)
  })
*/
  //TODO new
  //TODO no it
  this += new Transformation("replace assignments to runtime functions", {
    case decl @ IR_VariableDeclaration(_, _, Some(init @ (IR_Determinant(_,_) | IR_GetSlice(_) | IR_Inverse(_,_,_))), _) =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)

    case IR_Assignment(dest : IR_VariableAccess, gs : IR_GetSlice, _) =>
      IR_GetSliceRT(dest, gs.arguments)

    case IR_Assignment(dest : IR_VariableAccess, det : IR_Determinant, _) =>
      IR_DeterminantRT(dest, det.arg)

    case IR_Assignment(dest : IR_VariableAccess, inv : IR_Inverse, _) =>
      //TODO constructor of RTs from normal nodes
      IR_InverseRT(dest, inv.arg, inv.structureInformation)

    //DEBUG compare two matrices or scalars
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, ListBuffer(left : IR_Expression, right : IR_Expression, precision : IR_Expression))) if (call.name == "compare") =>
      IR_GenerateBasicMatrixOperations.compare(left, right, precision)

  })
  //TODO new
  this += new Transformation("built-in functions", {
    case f : IR_Expression with IR_MatrixExpressionFunction if f.hasAnnotation(resolvableLabel) =>
      f.execute()
  })
  //TODO new
  //TODO no it
  this += new Transformation("resolve runtime functions", {
    case exp : IR_Expression with IR_MatrixStatementFunction =>
      exp.execute()
  })
}

// resolve "Var matrix : Matrix<Datatype, rows, columns> = initialization" or split to declaration and assignment if convenient
object IR_ResolveMatrixDeclarations extends DefaultStrategy("Resolve matrix decl + initialization") {
  this += new Transformation("with scalars", {
    // split to use std::fill later
    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _), _, Some(init), _) if (IR_ResolveMatrixOperations.isScalar(init)) =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)
  })

  this += new Transformation("with matrices", {
    // do nothing
    case decl @ IR_VariableDeclaration(declDt @ IR_MatrixDatatype(_, _, _), _, Some(srcDt @ IR_MatrixExpression(_, _, _)), _) =>
      if (declDt.sizeM != srcDt.rows || declDt.sizeN != srcDt.columns)
        Logger.error(s"Declaration of variable of type: $declDt with expression of type: $srcDt, sizes must match!")
      decl

    // split to use std::memcpy or std::copy later
    case decl @ IR_VariableDeclaration(declDt @ IR_MatrixDatatype(_, _, _), _, Some(IR_VariableAccess(_, srcDt @ IR_MatrixDatatype(_, _, _))), _) =>
      if (declDt.sizeM != srcDt.sizeM || declDt.sizeN != srcDt.sizeN)
        Logger.error(s"Declaration of variable of type: $declDt with expression of type: $srcDt, sizes must match!")
      IR_MatrixNodeUtilities.splitDeclaration(decl)
  })
}

// resolve "matrix = expression"
object IR_ResolveMatrixAssignments extends DefaultStrategy("Resolve matrix assignments") {
  var debug = false

  /*
  this.onBefore = () =>
  {
    if (!Settings.additionalIncludes.contains("cstring")) {
      Settings.additionalIncludes +=  "cstring"
    }
  }
  */

  // use std::fill for assignments of matrices with constants
  this += new Transformation("with constants", {
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src, "=") if (IR_ResolveMatrixOperations.isScalar(src)) =>
      IR_FunctionCall(IR_ExternalFunctionReference("std::fill", IR_UnitDatatype), ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + dest.datatype.asInstanceOf[IR_MatrixDatatype].resolveFlattendSize, src)) : IR_Statement
  })

  // assignment of a matrix with another matrix : copy other matrix
  this += new Transformation("with matrices", {
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src @ (IR_MatrixExpression(_, _, _) | IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), "=") =>
      IR_GenerateBasicMatrixOperations.memcpyMatrix(dest, src)
  })
}

// simplify matrices e.g. neg(mat) to negated entries and resolve user defined functions
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

object IR_ResolveUserDefinedFunctions extends DefaultStrategy("Resolve user defined functions") {
  var voidedFunctions = ListBuffer[String]()
  var tmpCounter = 0

  this += new Transformation("parameters and return types", {
    case arg : IR_FunctionArgument if (arg.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      arg.datatype = IR_ReferenceDatatype(arg.datatype)
      arg
    case func : IR_Function if (func.datatype.isInstanceOf[IR_MatrixDatatype])       =>
      val matrix = func.datatype.asInstanceOf[IR_MatrixDatatype]
      func.parameters += IR_FunctionArgument("_matrix_return", IR_ReferenceDatatype(matrix))
      func.datatype = IR_UnitDatatype
      voidedFunctions += func.name

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
    case IR_VariableDeclaration(dt, name, Some(src @ IR_FunctionCall(_, _)), _) if (src.datatype.isInstanceOf[IR_MatrixDatatype] | (src.datatype == IR_UnitDatatype && voidedFunctions.contains(src.name))) =>
      var decl = IR_VariableDeclaration(dt, name, None)
      src.arguments += IR_VariableAccess(decl)
      ListBuffer[IR_Statement](
        decl,
        IR_ExpressionStatement(src)
      )
    /*
  case IR_VariableDeclaration(dt @ IR_ReferenceDatatype(IR_MatrixDatatype(_,_,_)), name,  Some(src @ IR_FunctionCall(_, _)), _)  =>
    var decl = IR_VariableDeclaration(dt.datatype, "referenceTmp_" + tmpCounter, None)
    src.arguments += IR_VariableAccess(decl)
    ListBuffer[IR_Statement](
      decl,
      IR_ExpressionStatement(src),
      IR_VariableDeclaration(dt,name,IR_VariableAccess(decl))
    )

     */
    case IR_Assignment(dest, src : IR_FunctionCall, "=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype])  =>
      // FIXME resolve IR_Assignments with operator += before this
      src.arguments += dest
      IR_ExpressionStatement(src)
    case IR_Assignment(dest, src : IR_FunctionCall, "+=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      Logger.error("+= matrix operator resolution not yet implemented")
  })

  /*
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

   */
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
      Logger.error(s"sizes do not match: $destSize vs ${ (src.rows, src.columns) }")
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

  // check if a matrix variable will be written to at any point in the course of the program
  //TODO multiple variables within different scopes?
  //TODO where to start?
  //TODO what if we check for constance when assignment were not yet build from other functions -> need to find all possibilities of writing to a matrix with name 'name' at all stages of processing
  def notWrittenTo(name : String) : Boolean = {
    var cconst = true
    // assignments to matrix 'name'
    if (StateManager.findAll[IR_Assignment]().exists(
      x => x.dest.isInstanceOf[IR_VariableAccess] && x.dest.asInstanceOf[IR_VariableAccess].name == name
    )) {
      cconst = false
    }

    // setting elements of matrix 'name': find function calls to setElement with matrix 'name' as argument
    var fcalls = StateManager.findAll[IR_FunctionCall]()
    if (fcalls.exists(
      x => (x.name == "set" | x.name == "setElement") && x.arguments(0).isInstanceOf[IR_VariableAccess] && x.arguments(0).asInstanceOf[IR_VariableAccess].name == name)) {
      cconst = false
    }

    // find external function calls with matrix 'name' as argument
    if (fcalls.exists(
      x => x.function match {
        case e : IR_ExternalFunctionReference if (x.arguments.exists(arg => arg.isInstanceOf[IR_VariableAccess] && arg.asInstanceOf[IR_VariableAccess].name == name)) => true
        case _                                                                                                                                                        => false
      })) {
      cconst = false
    }
    //TODO calls nach inverse call? -> matrix "bis dahin" compiletime constant

    // find inplace determinant or inverse calls
    if (Knowledge.experimental_inplaceDeterminant) {
      if (fcalls.exists(
        x => (x.name == "det" | x.name == "deter" | x.name == "determinant" | x.name == "detRT") && x.arguments(0).isInstanceOf[IR_VariableAccess] && x.arguments(0).asInstanceOf[IR_VariableAccess].name == name)) {
        cconst = false
      }
    }
    if (Knowledge.experimental_inplaceInversion) {
      if (fcalls.exists(
        x => (x.name == "inv" | x.name == "inverse" | x.name == "invRT") && x.arguments(0).isInstanceOf[IR_VariableAccess] && x.arguments(0).asInstanceOf[IR_VariableAccess].name == name)) {
        cconst = false
      }
    }

    cconst
  }
}
