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

import exastencils.base.ir
import exastencils.base.ir.IR_Addition
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Division
import exastencils.base.ir.IR_DoubleConstant
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_ElementwiseAddition
import exastencils.base.ir.IR_ElementwiseDivision
import exastencils.base.ir.IR_ElementwiseMultiplication
import exastencils.base.ir.IR_ElementwiseSubtraction
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ExpressionStatement
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FloatConstant
import exastencils.base.ir.IR_FloatDatatype
import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_Index
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Maximum
import exastencils.base.ir.IR_Minimum
import exastencils.base.ir.IR_Modulo
import exastencils.base.ir.IR_MultiDimArrayAccess
import exastencils.base.ir.IR_Multiplication
import exastencils.base.ir.IR_Negative
import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_NullExpression
import exastencils.base.ir.IR_Power
import exastencils.base.ir.IR_RealConstant
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_ReferenceDatatype
import exastencils.base.ir.IR_Return
import exastencils.base.ir.IR_ScalarDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_StringDatatype
import exastencils.base.ir.IR_Subtraction
import exastencils.base.ir.IR_UnitDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_MatNodes.IR_CrossProduct
import exastencils.baseExt.ir.IR_MatNodes.IR_Determinant
import exastencils.baseExt.ir.IR_MatNodes.IR_DeterminantCT
import exastencils.baseExt.ir.IR_MatNodes.IR_DeterminantRT
import exastencils.baseExt.ir.IR_MatNodes.IR_DotProduct
import exastencils.baseExt.ir.IR_MatNodes.IR_ExtractableMNode
import exastencils.baseExt.ir.IR_MatNodes.IR_ExtractableStatement
import exastencils.baseExt.ir.IR_MatNodes.IR_GetElement
import exastencils.baseExt.ir.IR_MatNodes.IR_GetSlice
import exastencils.baseExt.ir.IR_MatNodes.IR_GetSliceCT
import exastencils.baseExt.ir.IR_MatNodes.IR_GetSliceRT
import exastencils.baseExt.ir.IR_MatNodes.IR_InlineableDeclaration
import exastencils.baseExt.ir.IR_MatNodes.IR_IntermediateInv
import exastencils.baseExt.ir.IR_MatNodes.IR_InverseCT
import exastencils.baseExt.ir.IR_MatNodes.IR_InverseRT
import exastencils.baseExt.ir.IR_MatNodes.IR_ResolvableMNode
import exastencils.baseExt.ir.IR_MatNodes.IR_RuntimeMNode
import exastencils.baseExt.ir.IR_MatNodes.IR_SetElement
import exastencils.baseExt.ir.IR_MatNodes.IR_SetSlice
import exastencils.baseExt.ir.IR_MatNodes.IR_ToMatrix
import exastencils.baseExt.ir.IR_MatNodes.IR_Trace
import exastencils.baseExt.ir.IR_MatNodes.IR_Transpose
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateBasicMatrixOperations
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateRuntimeInversion
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.core.StateManager
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_MultiDimFieldAccess
import exastencils.logger.Logger

/** Strategy: preparatory transformations to resolve matrices */
object IR_PreItMOps extends DefaultStrategy("Prelimirary transformations") {
  // collector to check for writes to variables
  var variableCollector = new IR_MatrixVarCollector()
  this.register(variableCollector)
  this.onBefore = () => this.resetCollectors()

  // list to temporarily hold extractable nodes
  var extractables = ListBuffer[IR_ExtractableMNode]()

  // label for matrix expression nodes that have not been considered for inlining yet
  val potentialInline = "potentially inlineable"

  // all functions: call referenced constructor to build specialized matrix function node from function call
  val fctMap = Map[String, ListBuffer[IR_Expression] => IR_ExtractableMNode](
    ("getSlice", IR_GetSlice.apply),
    ("inverse", IR_IntermediateInv.apply),
    ("det", IR_Determinant.apply),
    ("deter", IR_Determinant.apply),
    ("determinant", IR_Determinant.apply),
    ("transpose", IR_Transpose.apply),
    ("cross", IR_CrossProduct.apply),
    ("crossProduct", IR_CrossProduct.apply),
    ("dot", IR_DotProduct.apply),
    ("dotProduct", IR_DotProduct.apply),
    ("trace", IR_Trace.apply),
    ("get", IR_GetElement.apply),
    ("getElement", IR_GetElement.apply),
    ("set", IR_SetElement.apply),
    ("setElement", IR_SetElement.apply),
    ("setSlice", IR_SetSlice.apply),
    ("toMatrix", IR_ToMatrix.apply)
  )

  import exastencils.baseExt.ir.IR_MatrixNodeUtilities.checkIfMatOp
  import exastencils.baseExt.ir.IR_MatrixNodeUtilities.isMatOp

  // replace function calls to matrix methods with dedicated nodes so they dont appear in function call tree and are easier to recognize and process
  /** Transformation: replace function calls with matrix method nodes */
  this += new Transformation("Replace function calls with matrix method nodes", {
    case f @ IR_FunctionCall(ref, args) if (fctMap.contains(ref.name) && checkIfMatOp(f)) =>
      f.removeAnnotation(isMatOp)
      fctMap(ref.name)(args)
  })

  /** Transformation: split combined assignment: += to IR_Addition, *= to IR_Multiplication, /= to IR_Division, -= to IR_Subtraction */
  this += new Transformation("Split combined operators", {
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src, "+=") =>
      IR_Assignment(dest, IR_Addition(dest, src))
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src, "*=") =>
      IR_Assignment(dest, IR_Multiplication(ListBuffer[IR_Expression](dest, src)))
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src, "-=") =>
      IR_Assignment(dest, IR_Subtraction(dest, src))
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src, "/=") =>
      IR_Assignment(dest, IR_ElementwiseDivision(dest, src))
  }, false)

  /** Transformation: go through statements and mark extractable matrix nodes as potentially inlinable,
    * as well as statements as a statement containing extractable nodes */
  //TODO expression statement
  this += new Transformation("Prepare extraction", {
    case stmt @ (IR_Assignment(_, src, _)) if (extractables ++= StateManager.findAll[IR_ExtractableMNode](src)).nonEmpty                   =>
      extractables.foreach(e => e.annotate(potentialInline))
      var out = new IR_ExtractableStatement(stmt, extractables.length)
      extractables.clear()
      out
    case stmt @ (IR_VariableDeclaration(_, _, Some(src), _)) if (extractables ++= StateManager.findAll[IR_ExtractableMNode](src)).nonEmpty =>
      extractables.foreach(e => e.annotate(potentialInline))
      var out = new IR_ExtractableStatement(stmt, extractables.length)
      extractables.clear()
      out
  }, false)
}

/** Strategy: inline or extract matrix operations from statements */
object IR_MatOpsInline extends DefaultStrategy("extract and inline matrix operations") {
  /** Attribute: list to hold temporary variable declarations temporarily and hand them over between strategies */
  var inlineDeclHolder = ListBuffer[IR_InlineableDeclaration]()
  /** Attribute: list to hold temporary variable accesses temporarily and hand them over between strategies */
  var inlineAccessHolder = ListBuffer[IR_VariableAccess]()
  /** Attribute: int number of extractables extracted to update counter of current statement */
  var extractMethodsCounter = 0

  /** Attribute: label for statements to document the number of extractables left in the statement */
  val nExtractables = "number of extractables"
  /** Attribute: label to inline this node */
  val inline = "inline"
  /** Attribute: label to mark this node as considered and rejected for inline */
  val notInlinable = "notInlinable"
  /** Attribute: label to mark this node as not yet considered for inline */
  val potentialInline = "potentially inlineable"

  /** Strategy: collect variable declarations for extractables found in statements */
  object IR_ExtractMatrices extends QuietDefaultStrategy("extract") {
    /** Transformation: if an expression is extractable, extract to temporary variable declaration */
    this += new Transformation("Extract", {
      case e : IR_ExtractableMNode if (e.isExtractable() && !e.hasAnnotation(notInlinable)) =>
        // produce a new declaration for extractable expression
        val tmpname = "extractTmp_" + extractMethodsCounter
        extractMethodsCounter += 1
        val tmpDecl = IR_InlineableDeclaration(e.datatype, tmpname, e)
        inlineDeclHolder += tmpDecl

        // replace with access to new declaration
        val nacc = IR_VariableAccess(tmpname, e.datatype)
        IR_ExtractMatrices.applyStandalone(tmpDecl.initialValue)
        inlineAccessHolder += nacc
        inlineAccessHolder.last
    }, false)
  }

  /** Transformation: extract extractables in statements with extractables left in them  */
  this += new Transformation("Search stmts", {
    case estmt : IR_ExtractableStatement if (estmt.nExtractables > 0) =>
      // find extractables, produce declarations, replace with access to declaration
      IR_ExtractMatrices.applyStandalone(estmt)
      var newstmts = Duplicate(inlineDeclHolder.reverse)
      // update extractable counter with handled extractables in this statement
      estmt.nExtractables -= newstmts.length
      inlineDeclHolder.clear()
      var out = ListBuffer[IR_Node]()
      out ++= newstmts
      out += estmt
      out
  }, false)

  /** Transformation: resolve inlineable declarations and mark accesses as resolvable
    * or remove the declaration and assign the expression to inline as annotation */
  this += new Transformation("Resolve and remove ext/inl statements", {
    case d : IR_InlineableDeclaration =>
      d.removeAnnotation(nExtractables)
      d.initialValue.removeAnnotation(potentialInline)
      val accs = inlineAccessHolder.filter(a => (a.name == d.name))
      accs.foreach(a => {
        a.removeAnnotation(potentialInline)
        a.annotate(notInlinable)
      })
      if (d.isInlineable()) {
        accs.foreach(a => a.annotate(inline, d.initialValue))
        None
      } else {
        var out = IR_VariableDeclaration(d.datatype, d.name, Some(d.initialValue))
        out.removeAnnotation(nExtractables)
        out
      }
    // remove extractable statements if they are extracted
    case estmt : IR_ExtractableStatement if (estmt.nExtractables == 0) =>
      estmt.inner
  }, false)

  /** Transformation: replace accesses with expressions to inline */
  this += new Transformation("Inline values", {
    case va : IR_VariableAccess if (va.hasAnnotation(inline)) =>
      va.popAnnotationAs[IR_Expression](inline)
  })
}

/** Strategy: resolve matrix functions */
object IR_ResolveMatFuncs extends DefaultStrategy("Resolve matFuncs") {
  /** Attribute: Map to convert intermediate matrix function nodes to resolvable compiletime nodes */
  val ctFctMap = Map[String, IR_RuntimeMNode => IR_ResolvableMNode](
    ("IR_IntermediateInv", IR_InverseCT.apply),
    ("IR_GetSlice", IR_GetSliceCT.apply),
    ("IR_Determinant", IR_DeterminantCT.apply)
  )

  /** Attribute: Map to convert intermediate matrix function nodes to resolvable compiletime nodes */
  val rtFctMap = Map[String, (IR_VariableAccess, IR_RuntimeMNode) => IR_ResolvableMNode](
    ("IR_IntermediateInv", IR_InverseRT.apply),
    ("IR_GetSlice", IR_GetSliceRT.apply),
    ("IR_Determinant", IR_DeterminantRT.apply)
  )

  import exastencils.baseExt.ir.IR_MatOpsInline.potentialInline

  /** Transformation: replace special(eventually to resolve at runtime)
    * function nodes with their resolvable counterparts if they are ready (considered for inline)
    * and resolve
    */
  this += new Transformation("Insert resolvables and resolve", {
    case decl @ IR_VariableDeclaration(_, _, Some(r : IR_RuntimeMNode), _) =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)
    case estmt : IR_ExtractableStatement                                   =>
      estmt
    // not to resolve at runtime
    case r : IR_RuntimeMNode if (!r.resolveAtRuntime && !r.hasAnnotation(potentialInline)) =>
      ctFctMap(r.name)(r)
    // to resolve at runtime
    case IR_Assignment(dest : IR_VariableAccess, r : IR_RuntimeMNode, _) if (r.resolveAtRuntime && !r.hasAnnotation(potentialInline)) =>
      rtFctMap(r.name)(dest, r)

    case IR_ExpressionStatement(mn : IR_ResolvableMNode) if (mn.isResolvable()) =>
      mn.resolve()

    // resolve
    case mn : IR_ResolvableMNode if mn.isResolvable() =>
      mn.resolve()

    // debug
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, ListBuffer(left : IR_Expression, right : IR_Expression, precision : IR_Expression))) if (call.name == "compare") =>
      IR_GenerateBasicMatrixOperations.compare(left, right, precision)
  })

}

/** Strategy:  resolve Matrix operators like addition of the operands are ready */
object IR_ResolveMatOperators extends DefaultStrategy("resolve operators") {

  import exastencils.baseExt.ir.IR_MatrixNodeUtilities.checkIfMatOp
  import exastencils.baseExt.ir.IR_MatrixNodeUtilities.isEvaluatable
  import exastencils.baseExt.ir.IR_MatrixNodeUtilities.isMatOp

  this += new Transformation("Resolve operators", {
    //TODO match on supertype? -> introduce supertype
    case mult @ IR_Multiplication(facs) if (checkIfMatOp(mult) && facs.forall(f => isEvaluatable(f)))                                                                =>
      mult.removeAnnotation(isMatOp)
      IR_BasicMatrixOperations.mult(mult)
    case add @ (IR_Addition(sums)) if (checkIfMatOp(add) && sums.forall(f => isEvaluatable(f)) && !add.hasAnnotation(IR_GenerateRuntimeInversion.pointerArithmetic)) =>
      add.removeAnnotation(isMatOp)
      IR_BasicMatrixOperations.add(add)
    case binOp @ IR_ElementwiseSubtraction(_, _) if (checkIfMatOp(binOp) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))                                 =>
      binOp.removeAnnotation(isMatOp)
      IR_BasicMatrixOperations.sub(binOp)
    case binOp @ IR_Subtraction(_, _) if (checkIfMatOp(binOp) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))                                            =>
      binOp.removeAnnotation(isMatOp)
      IR_BasicMatrixOperations.sub(binOp)
    case binOp @ IR_ElementwiseMultiplication(_, _) if (checkIfMatOp(binOp) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))                              =>
      binOp.removeAnnotation(isMatOp)
      IR_BasicMatrixOperations.elementwiseMultiplication(binOp.left, binOp.right)
    case binOp @ IR_ElementwiseDivision(_, _) if (checkIfMatOp(binOp) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))                                    =>
      binOp.removeAnnotation(isMatOp)
      IR_BasicMatrixOperations.elementwiseDivision(binOp.left, binOp.right)
    case binOp @ IR_ElementwiseAddition(_, _) if (checkIfMatOp(binOp) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))                                    =>
      binOp.removeAnnotation(isMatOp)
      IR_BasicMatrixOperations.add(binOp)
  })
}

/** Strategy: resolve "Var matrix : Matrix<Datatype, rows, columns> = initialization" or split to declaration and assignment if convenient */
object IR_PostItMOps extends DefaultStrategy("Resolve matrix decls and assignments") {
  var debug = false
  val annotationMatrixRow = "IR_ResolveMatrices.matrixRow"
  val annotationMatrixCol = "IR_ResolveMatrices.matrixCol"


  /** Transformation: resolve or split declarations */
  this += new Transformation("decls", {
    // split to use std::fill later
    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _), _, Some(init), _) if (IR_MatrixNodeUtilities.isScalar(init)) =>
      IR_MatrixNodeUtilities.splitDeclaration(decl)

    // do nothing
    case decl @ IR_VariableDeclaration(declDt @ IR_MatrixDatatype(_, _, _), _, Some(srcDt @ IR_MatrixExpression(_, _, _, _)), _) =>
      if (declDt.sizeM != srcDt.rows || declDt.sizeN != srcDt.columns)
        Logger.error(s"Declaration of variable of type: $declDt with expression of type: $srcDt, sizes must match!")
      decl

    // split to use std::memcpy or std::copy later
    case decl @ IR_VariableDeclaration(declDt @ IR_MatrixDatatype(_, _, _), _, Some(IR_VariableAccess(_, srcDt @ IR_MatrixDatatype(_, _, _))), _) =>
      if (declDt.sizeM != srcDt.sizeM || declDt.sizeN != srcDt.sizeN)
        Logger.error(s"Declaration of variable of type: $declDt with expression of type: $srcDt, sizes must match!")
      IR_MatrixNodeUtilities.splitDeclaration(decl)

    // add helper matrix  decls from schur compiletime inversion
    case s @ (IR_Assignment(_, _, _) | IR_VariableDeclaration(_, _, _, _)) if (Knowledge.experimental_schurWithHelper) =>
      val ms = StateManager.findAll[IR_MatrixExpression](s).filter(x => if (x.hasAnnotation("helperMatrices")) true else false)
      val helperDecls = ListBuffer[IR_Statement]()
      for (m <- ms) {
        helperDecls ++= m.popAnnotationAs[IR_VariableDeclaration]("helperMatrices").asInstanceOf[ListBuffer[IR_Statement]]
      }
      helperDecls += s.asInstanceOf[IR_Statement]
  })

  /** Transformation: resolve assignments */
  this += new Transformation("assignments", {
    // use std::fill for assignments of matrices with constants
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src, "=") if (IR_MatrixNodeUtilities.isScalar(src)) =>
      IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::fill", IR_UnitDatatype), ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + IR_IntegerConstant(dest.datatype.asInstanceOf[IR_MatrixDatatype].resolveFlattendSize), src)))

    // assignment of a matrix with another matrix : copy other matrix
    case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src @ (IR_MatrixExpression(_, _, _, _) | _ : IR_VariableAccess ), "=") if (src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      IR_GenerateBasicMatrixOperations.copyMatrix(dest, src)

    // other assignments
    case stmt @ IR_Assignment(dest, _, _) if (dest.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      val matrix = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      var newStmts = ListBuffer[IR_Statement]()
      for (row <- 0 until matrix.sizeM) {
        for (col <- 0 until matrix.sizeN) {
          var cloned = Duplicate(stmt)
          StateManager.findAll[IR_Expression](cloned).foreach {
            case _ : IR_FunctionArgument                                                                                                            => // do not mark function arguments to be resolved into individual accesses
            case x @ ( _ : IR_MultiDimFieldAccess) if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => {
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

  /** Transformation: simplify matrices e.g. neg(mat) to negated entries and resolve user defined functions */
  this += new Transformation("simplify", {
    case IR_Negative(m : IR_MatrixExpression)                               => m.expressions = m.expressions.map { y => IR_Negative(y) : IR_Expression }; m
    case IR_Negative(va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))) =>
      var m = IR_MatrixNodeUtilities.accessToExpression(va)
      m.expressions = m.expressions.map { y => IR_Negative(y) : IR_Expression };
      m
    case m @ IR_MatrixExpression(_, 1, 1, _)                                => m.get(0, 0)
    case m @ IR_MatrixDatatype(dt, 1, 1)                                    => dt
  })

  var voidedFunctions = ListBuffer[String]()
  var tmpCounter = 0

  this += new Transformation("function parameters and return types", {
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
    case IR_VariableDeclaration(dt @ IR_ReferenceDatatype(IR_MatrixDatatype(_, _, _)), name, Some(src @ IR_FunctionCall(_, _)), _)                                                                          =>
      var decl = IR_VariableDeclaration(dt.datatype, "referenceTmp_" + tmpCounter, None)
      src.arguments += IR_VariableAccess(decl)
      ListBuffer[IR_Statement](
        decl,
        IR_ExpressionStatement(src),
        IR_VariableDeclaration(dt, name, IR_VariableAccess(decl))
      )

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
       var decl = IR_VariableDeclaration(argexp.datatype, "_matrixExp" + tmpCounter, argexp)
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
       var decl = IR_VariableDeclaration(argexp.datatype, "_matrixExp" + tmpCounter, argexp)
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

/** Strategy: linearize matrix expressions */
object IR_LinearizeMatrices extends DefaultStrategy("linearize matrices") {
  this += Transformation("Linearize", {
    case IR_HighDimAccess(base, _) if (!base.datatype.isInstanceOf[IR_MatrixDatatype] && !base.datatype.isInstanceOf[IR_TensorDatatype]) => base

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
        ir.IR_ArrayAccess(base, IR_IntegerConstant(matrix.sizeN * idx.indices(0) + idx.indices(1)))
      else
        base

    case IR_HighDimAccess(base, idx : IR_ExpressionIndex) if idx.indices.length == 2 =>
      val (rows, cols) = base.datatype match {
        case tdt1 : IR_TensorDatatype1 => (tdt1.dims, 1)
        case tdt2 : IR_TensorDatatype2 => (tdt2.dims, tdt2.dims)
        case mdt : IR_MatrixDatatype   => (mdt.sizeM, mdt.sizeN)
      }

      if (rows > 1 || cols > 1)
        IR_ArrayAccess(base, IR_IntegerConstant(cols) * idx.indices(0) + idx.indices(1))
      else
        base
  }, false)
}

/** Strategy: methods to transform certain types of nodes related to matrices */
object IR_MatrixNodeUtilities {
  /** Attribute: int give temporary variables unique names */
  var tmpCounter = 0

  /** Method: check if an argument is ready to be evaluated
    *
    * @param n : IR_Node, node to be checked for evaluatability
    * @return is evaluatable?
    **/
  //TODO other datatypes?
  def isEvaluatable(n : IR_Node) : Boolean = {
    n match {
      case x : IR_Expression => isMatrix(x) | isScalar(x) | isString(x) | isTensor(x)
      case _                 => Logger.error(s"unexpected type ${ n }")
    }
  }

  /** Method: determine whether an expression is an access to a variable with type matrix or a matrix expression
    *
    * @param x : IR_Expression, expression to be checked for
    * @return is matrix?
    **/
  def isMatrix(x : IR_Expression) : Boolean = {
    x match {
      case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))                                                 => true
      case IR_MatrixExpression(_, _, _, _)                                                                  => true
      case IR_VariableAccess(_, IR_ReferenceDatatype(innerDt)) if (innerDt.isInstanceOf[IR_MatrixDatatype]) => true
      case _                                                                                                => false
    }
  }

  /** Method: determine whether an expression is an access to a variable with type tensor or a tensor expression
    *
    * @param x : IR_Expression, expression to be checked for
    * @return is tensor?
    **/
  def isTensor(x : IR_Expression) : Boolean = {
    x match {
      case IR_VariableAccess(_, inner : IR_TensorDatatype)                                                  => true
      case t : IR_TensorExpression                                                                          => true
      case IR_VariableAccess(_, IR_ReferenceDatatype(innerDt)) if (innerDt.isInstanceOf[IR_TensorDatatype]) => true
      case _                                                                                                => false
    }
  }
  /** Method: determine whether an expression is an access to a scalar variable or constant/value
    *
    * @param x : IR_Expression, expression to be checked for
    * @return is scalar value?
    **/
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

  /** Method: determine whether an expression is an access to a scalar variable or constant/value string
    *
    * @param x : IR_Expression, expression to be checked for
    * @return is string?
    **/
  def isString(x : IR_Expression) : Boolean = {
    x match {
      case IR_StringConstant(_)                    => true
      case IR_VariableAccess(_, IR_StringDatatype) => true
      case _                                       => false
    }
  }

  /** Attribute: label to mark an operation as matrix operation */
  val isMatOp = "isMatrixOperation"

  /** Attribute: label to mark an operation as non-matrix operation */
  val isNotMatOp = "isNotMatrixOperation"

  /** Method: check if an expression is a matrix operation or find out and label
    *
    * @param op : IR_Expression, expression to be checked
    * @return is mat op?
    **/
  def checkIfMatOp(op : IR_Expression) : Boolean = {
    if (op.hasAnnotation(isNotMatOp)) false
    else if (op.hasAnnotation(isMatOp)) true
    else {
      val b = op match {
        case m : IR_Multiplication                                        => m.factors.exists(f => isMatrix(f))
        case a : IR_Addition                                              => a.summands.exists(f => isMatrix(f))
        case s : IR_Subtraction                                           => isMatrix(s.left) | isMatrix(s.right)
        case s : IR_ElementwiseSubtraction                                => isMatrix(s.left) | isMatrix(s.right)
        case e : IR_ElementwiseMultiplication                             => isMatrix(e.left) | isMatrix(e.right)
        case e : IR_ElementwiseAddition                                   => isMatrix(e.left) | isMatrix(e.right)
        case e : IR_ElementwiseDivision                                   => isMatrix(e.left) | isMatrix(e.right)
        case cast @ IR_FunctionCall(ref, _) if (ref.name == "toMatrix")   => true
        case inverse @ IR_FunctionCall(ref, _) if (ref.name == "inverse") => true
        case e : IR_FunctionCall                                          => e.arguments.exists(a => isMatrix(a))
      }
      if (b) {
        op.annotate(isMatOp)
        true
      }
      else {
        op.annotate(isNotMatOp)
        false
      }
    }
  }

  /** Method: split a declaration with init to declaration and assignment with init
    *
    * @param decl : IR_VariableDeclaration, declaration to be split
    * @return list containing variable declaration without init and assignment of that variable with init expresion
    **/
  def splitDeclaration(decl : IR_VariableDeclaration) : ListBuffer[IR_Statement] = {
    val newStmts = ListBuffer[IR_Statement]()
    newStmts += IR_VariableDeclaration(decl.datatype, decl.name, None)
    newStmts += IR_Assignment(IR_VariableAccess(Duplicate(decl)), decl.initialValue.getOrElse(IR_NullExpression))
    newStmts
  }

  /** Method: copy a matrix from an IR_VariableAccess to an IR_MatrixExpression
    * by building an expression of highDimAccesses
    *
    * @param src : IR_VariableAccess, access to convert
    * @return expression of hdas
    **/
  def accessToExpression(src : IR_VariableAccess) : IR_MatrixExpression = {
    var size = IR_BasicMatrixOperations.getSize(src)
    if (size._1 > 1 || size._2 > 1) {
      var out = IR_MatrixExpression(src.datatype.resolveBaseDatatype, size._1, size._2)
      for (i <- 0 until size._1) {
        for (j <- 0 until size._2) {
          out.set(i, j, IR_HighDimAccess(src, IR_ExpressionIndex(IR_IntegerConstant(i), IR_IntegerConstant(j))))
        }
      }
      out
    } else {
      IR_MatrixExpression(Some(src.datatype.resolveBaseDatatype), 1, 1, Array[IR_Expression](src))
    }
  }

  /** Method: transform a matrix expression to a temporary variable
    *
    * @param src  : IR_MatrixExpression, used as initialization
    * @param name : String, name of the new temporary variable
    * @return declaration of the tmp
    **/
  def expressionToDeclaration(src : IR_MatrixExpression, name : String) : IR_VariableDeclaration = {
    var decl = IR_VariableDeclaration(IR_MatrixDatatype(src.datatype.resolveBaseDatatype, src.rows, src.columns), name + tmpCounter, src)
    tmpCounter += 1
    decl
  }

}
