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

// handle runtime methods seperately: they must be processed with their destination/return variable
object IR_PreItMatOps extends DefaultStrategy("Prelimirary transformations") {
  // replace function calls to matrix methods to dedicated nodes so they dont appear in function call tree and are easy to recognize
  this += new Transformation("replace function calls with matrix method nodes", {
    case f @ IR_FunctionCall(_, args) if (f.name == "getSlice")                                              =>
      IR_GetSlice(args)
    case f @ IR_FunctionCall(_, args) if (f.name == "inverse")                                               =>
      IR_Inverse(args)
    case f @ IR_FunctionCall(_, args) if (f.name == "det" || f.name == "deter" || f.name == "determinant")   =>
      IR_Determinant(args(0))
    case f @ IR_FunctionCall(_, args) if (f.name == "transpose")                                             =>
      IR_Transpose(args(0))
    case f @ IR_FunctionCall(_, args) if (f.name == "crossProduct" || f.name == "cross")                     =>
      IR_CrossProduct(args)
    case f @ IR_FunctionCall(_, args) if (f.name == "dotProduct" || f.name == "dot")                         =>
      IR_DotProduct(args)
    case f @ IR_FunctionCall(_, args) if (f.name == "trace")                                                 =>
      IR_Trace(args(0))
    case f @ IR_FunctionCall(_, args) if (f.name == "get" || f.name == "getElement")                         =>
      IR_GetElement(args)
    case IR_ExpressionStatement(f @ IR_FunctionCall(_, args)) if (f.name == "set" || f.name == "setElement") =>
      IR_SetElement(args)
    case IR_ExpressionStatement(f @ IR_FunctionCall(_, args)) if (f.name == "setSlice")                      =>
      IR_SetSlice(args)
    case f @ IR_FunctionCall(_, args) if (f.name == "matmult")                                               =>
      IR_Multiplication(args)
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
}

object IR_ExtractMatOps extends DefaultStrategy("Extract matrix operations") {
  // runtime methods work with a destination variable and have to be resolved with their assignment -> extract them to helper variables
  var extractMethodsLabel = "extractMethod"
  var extractMethodsCounter = 0

  // list to hold declarations of temporary variables from runtime method extraction
  var extractRuntimeTmps = ListBuffer[IR_Statement]()

  // create new variables to extract runtime method calls and mark nodes for later replacement
  def extractAndAnnotate(src : IR_Expression) : ListBuffer[IR_Statement] = {
    var newStmts = ListBuffer[IR_Statement]()
    StateManager.findFirst[IR_Extractable](src) match {
      //TODO how to identify already extracted variables? -> SOLVED dont need to cuz start seach from subassignment
      // -> stops when all extractables are extracted and no hits anymore
      case Some(f : IR_Extractable) =>
        val decl = IR_VariableDeclaration(f.datatype, "extractTmp_" + extractMethodsCounter, Duplicate(f))
        f.annotate(extractMethodsLabel, extractMethodsCounter)
        extractMethodsCounter += 1
        newStmts += decl
      case _                        =>
    }
    newStmts
  }

  this += new Transformation("extract extractable-method calls", {
    case stmt @ IR_Assignment(_, src, _) if (extractRuntimeTmps ++= extractAndAnnotate(src)).nonEmpty                     =>
      var out = Duplicate(extractRuntimeTmps) += stmt
      extractRuntimeTmps.clear()
      out
    case stmt @ IR_VariableDeclaration(_, _, Some(init), _) if (extractRuntimeTmps ++= extractAndAnnotate(init)).nonEmpty =>
      var out = Duplicate(extractRuntimeTmps) += stmt
      extractRuntimeTmps.clear()
      out
  })

  this += new Transformation("replace extracted method calls", {
    case exp : IR_Extractable if (exp.hasAnnotation(extractMethodsLabel)) =>
      IR_VariableAccess("extractTmp_" + exp.popAnnotationAs[Int](extractMethodsLabel), exp.datatype)
  })
}

object IR_ResolveUnknownDts extends DefaultStrategy("Resolve Unknown dts") {
  this += new Transformation("resolve", {
    case decl @ IR_VariableDeclaration(_, _, Some(init : IR_Extractable), _) =>
      decl.datatype = decl.datatype match {
        case IR_UnknownDatatype =>
          init.datatype
        case _                  => decl.datatype
      }
      IR_MatrixNodeUtilities.splitDeclaration(decl)
  })
}

object IR_InlineMatOps extends DefaultStrategy("Inline temporaries") {
  // label for operations ready to be resolved: all arguments are available
  var resolvable = "resolveMatrix"
  var tryInline = "inlinableTmp"
  var notInlinable = "notInlinableTmp"
  var removeTmpDecl = "removeTmpDecl"

  this += new Transformation("mark for potential inline", {
    case assign @ IR_Assignment(_, src, _) if (!assign.hasAnnotation(notInlinable))               =>
      var tmpHits = false
      StateManager.findAll[IR_VariableAccess](src).foreach(va => va.uniqueID match {
        case id if (id.startsWith("extractTmp_") && !va.hasAnnotation(notInlinable)) =>
          va.annotate(tryInline)
          tmpHits = true
        case _                                                                       =>
      })
      if (!tmpHits) assign.annotate(notInlinable)
      assign
    case decl @ IR_VariableDeclaration(_, _, Some(src), _) if (!decl.hasAnnotation(notInlinable)) =>
      var tmpHits = false
      StateManager.findAll[IR_VariableAccess](src).foreach(va => va.uniqueID match {
        case id if (id.startsWith("extractTmp_") && !va.hasAnnotation(notInlinable)) =>
          va.annotate(tryInline)
          tmpHits = true
        case _                                                                       =>
      })
      if (!tmpHits) decl.annotate(notInlinable)
      decl
  })

  this += new Transformation("inline temporaries", {
    case tmp : IR_VariableAccess if (tmp.hasAnnotation(tryInline)) =>
      tmp.removeAnnotation(tryInline)
      var decl = StateManager.findFirst[IR_VariableDeclaration]({ d : IR_VariableDeclaration => d.name == tmp.name }, StateManager.root).getOrElse(Logger.error(s"decl of ${ tmp.name } not found"))
      var init = decl.initialValue.getOrElse(Logger.error(s"temporary ${ decl.name } not initialized"))
      init match {
        case inv : IR_Inverse if (!inv.resolveAtRuntime) =>
          decl.annotate(removeTmpDecl)
          IR_InverseCT(inv)

        case det : IR_Determinant if (!det.resolveAtRuntime) =>
          decl.annotate(removeTmpDecl)
          IR_DeterminantCT(det)

        case gs : IR_GetSlice if (!gs.resolveAtRuntime) =>
          decl.annotate(removeTmpDecl)
          IR_GetSliceCT(gs)

        case _ =>
          tmp.annotate(notInlinable)
          tmp
      }
  })

  this += new Transformation("remove temporary declarations", {
    case decl : IR_VariableDeclaration if decl.hasAnnotation(removeTmpDecl) => IR_NullStatement
  })

  this += new Transformation("insert compiletime functions", {
    case inv : IR_Inverse if (!inv.resolveAtRuntime)     =>
      IR_InverseCT(inv)
    case det : IR_Determinant if (!det.resolveAtRuntime) =>
      IR_DeterminantCT(det)
    case gs : IR_GetSlice if (!gs.resolveAtRuntime)      =>
      IR_GetSliceCT(gs)
  })

  //TODO no it? -> after everything
  this += new Transformation("insert runtime functions", {

    case IR_Assignment(dest : IR_VariableAccess, gs : IR_GetSlice, _) if (gs.resolveAtRuntime) =>
      IR_GetSliceRT(dest, gs.arguments)

    case IR_Assignment(dest : IR_VariableAccess, det : IR_Determinant, _) if (det.resolveAtRuntime) =>
      IR_DeterminantRT(dest, det.arg)

    // inv det -> inv rt if "Runtime"
    case IR_Assignment(dest : IR_VariableAccess, inv : IR_Inverse, _) if (inv.resolveAtRuntime) =>
      IR_InverseRT(dest, inv)

    //DEBUG compare two matrices or scalars
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, ListBuffer(left : IR_Expression, right : IR_Expression, precision : IR_Expression))) if (call.name == "compare") =>
      IR_GenerateBasicMatrixOperations.compare(left, right, precision)
  })

}

object IR_MarkResolvableMatOps extends DefaultStrategy("Mark operations ready for resolve") {
  // label for operations ready to be resolved: all arguments are available
  var resolvableLabel = "resolveMatrix"

  import exastencils.baseExt.ir.IR_MatrixNodeUtilities.isEvaluatable
  import exastencils.baseExt.ir.IR_MatrixNodeUtilities.isMatrix

  // mark operations with evaluatable arguments
  this += new Transformation("operators", {
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

  })

  this += new Transformation("functions", {
    case n : IR_ResolvableMatrixNode if (n.isResolvable()) =>
      n.annotate((resolvableLabel))
      n
  })

}

// resolve matrix operators to IR_MatrixExpressions
object IR_ResolveMatOps extends DefaultStrategy("Resolve matrix operations") {
  // label for operations ready to be resolved: all arguments are available
  var resolvableLabel = "resolveMatrix"

  // resolve operators to IR_MatrixExpressions if arguments are available
  this += new Transformation("operators", {
    case mult @ IR_Multiplication(_) if (mult.hasAnnotation(resolvableLabel))                        =>
      mult.removeAnnotation(resolvableLabel)
      IR_BasicMatrixOperations.mult(mult)
    case add @ (IR_Addition(_)) if (add.hasAnnotation(resolvableLabel))                              =>
      add.removeAnnotation(resolvableLabel)
      IR_BasicMatrixOperations.add(add)
    case eadd @ (IR_ElementwiseAddition(_, _)) if (eadd.hasAnnotation(resolvableLabel))              =>
      eadd.removeAnnotation(resolvableLabel)
      IR_BasicMatrixOperations.add(eadd)
    case sub @ IR_Subtraction(_, _) if (sub.hasAnnotation(resolvableLabel))                          =>
      sub.removeAnnotation(resolvableLabel)
      IR_BasicMatrixOperations.sub(sub)
    case esub @ IR_ElementwiseSubtraction(_, _) if (esub.hasAnnotation(resolvableLabel))             =>
      esub.removeAnnotation(resolvableLabel)
      IR_BasicMatrixOperations.sub(esub)
    case emult @ IR_ElementwiseMultiplication(left, right) if (emult.hasAnnotation(resolvableLabel)) =>
      emult.removeAnnotation(resolvableLabel)
      IR_BasicMatrixOperations.elementwiseMultiplication(left, right)
    case ediv @ IR_ElementwiseDivision(left, right) if (ediv.hasAnnotation(resolvableLabel))         =>
      ediv.removeAnnotation(resolvableLabel)
      IR_BasicMatrixOperations.elementwiseDivision(left, right)
  })

  this += new Transformation("compiletime functions", {
    case f : IR_MatrixExpressionFunction if f.hasAnnotation(resolvableLabel) =>
      f.removeAnnotation(resolvableLabel)
      f.resolve()
  })

  //TODO no it? after everything
  this += new Transformation("runtime functions", {
    case f : IR_MatrixStatementFunction =>
      f.removeAnnotation(resolvableLabel)
      f.resolve()
  })
}

// resolve "Var matrix : Matrix<Datatype, rows, columns> = initialization" or split to declaration and assignment if convenient
object IR_ResolveMatrixDeclarations extends DefaultStrategy("Resolve matrix decl + initialization") {

  this += new Transformation("with scalars", {
    // split to use std::fill later
    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _), _, Some(init), _) if (IR_MatrixNodeUtilities.isScalar(init)) =>
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
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src, "=") if (IR_MatrixNodeUtilities.isScalar(src)) =>
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
