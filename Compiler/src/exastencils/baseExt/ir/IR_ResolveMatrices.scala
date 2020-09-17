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

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatNodes._
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateBasicMatrixOperations
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateRuntimeInversion
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.core.StateManager
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.HelperNode
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_MultiDimFieldAccess
import exastencils.globals.ir.IR_GlobalCollection

/** Strategy: preparatory transformations to resolve matrices */
object IR_PreItMOps extends DefaultStrategy("Prelimirary transformations") {
  // collector to check for writes to variables
  var variableCollector = new IR_MatrixVarCollector()
  this.register(variableCollector)
  this.onBefore = () => this.resetCollectors()

  /////////////////////////////////////////////////////////////////////
  var tmpCounter = 0
  val annotationFctCallCounter = "IR_ResolveMatrices.fctCallCounter"
  var fctCallCounter = 0
  // temporary variable used to replace function calls in expressions
  val annotationMatExpCounter = "IR_ResolveMatrices.matrixExpressionCounter"
  var matExpCounter = 0
  /////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////// function nodes
  val fctMapExprs = Map[String, ListBuffer[IR_Expression] => IR_ExtractableMNode](
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
    ("toMatrix", IR_ToMatrix.apply)
  )
  val fctMapStmts = Map[String, ListBuffer[IR_Expression] => IR_Statement](
    ("set", IR_SetElement.apply),
    ("setElement", IR_SetElement.apply),
    ("setSlice", IR_SetSlice.apply),
  )

  import exastencils.baseExt.ir.IR_MatNodeUtils.checkIfMatOp
  import exastencils.baseExt.ir.IR_MatNodeUtils.isMatOp

  // replace function calls to matrix methods with dedicated nodes so they dont appear in function call tree and are easier to recognize and process
  this += new Transformation("Replace function calls with matrix method nodes", {
    case f @ IR_FunctionCall(ref, args) if fctMapExprs.contains(ref.name) && checkIfMatOp(f)                         =>
      f.removeAnnotation(isMatOp)
      fctMapExprs(ref.name)(args)
    case IR_ExpressionStatement(f @ IR_FunctionCall(ref, args)) if fctMapStmts.contains(ref.name) && checkIfMatOp(f) =>
      f.removeAnnotation(isMatOp)
      fctMapStmts(ref.name)(args)
  })
  //////////////////////////////////////////////////////////////////

  this += new Transformation("Split combined operators", {
    case IR_Assignment(dest, src, "+=") =>
      IR_Assignment(dest, IR_Addition(dest, src))
    case IR_Assignment(dest, src, "*=") =>
      IR_Assignment(dest, IR_Multiplication(ListBuffer[IR_Expression](dest, src)))
    case IR_Assignment(dest, src, "-=") =>
      IR_Assignment(dest, IR_Subtraction(dest, src))
    case IR_Assignment(dest, src, "/=") =>
      IR_Assignment(dest, IR_Division(dest, src))
  }, false)

  ///////////////////////////////////////////// matAccesses
  this += new Transformation("Wrap matAccesses around field accesses with defined matIndices", {
    case fa : IR_FieldAccess if (fa.matIndex.isDefined) =>
      val ma = IR_MatrixAccess(fa, fa.matIndex.get(0), if (fa.matIndex.get.length == 2) Some(fa.matIndex.get(1)) else None)
      //fa.matIndex = None
      ma

  }, false)

  object TransformMatAccesses extends QuietDefaultStrategy("Transforming MatAccesses to slice nodes") {
    this += new Transformation("transform", {
      case macc : IR_MatrixAccess => macc.expand(false, None)
    })
  }

  this += new Transformation("Transform lval Matrix accesses to slice nodes", {
    case IR_Assignment(dest : IR_MatrixAccess, src, _) =>
      dest.expand(true, Some(src))
  })

  this += new Transformation("Transform rval matrix accesses to slice nodes", {
    case stmt : IR_Assignment          =>
      TransformMatAccesses.applyStandalone(stmt)
      stmt
    case stmt : IR_VariableDeclaration =>
      TransformMatAccesses.applyStandalone(stmt)
      stmt
    case stmt : IR_ExpressionStatement =>
      TransformMatAccesses.applyStandalone(stmt)
      stmt
    case setSlice : IR_SetSlice =>
      TransformMatAccesses.applyStandalone(setSlice)
      setSlice
    case setElement : IR_SetElement =>
      TransformMatAccesses.applyStandalone(setElement)
      setElement
  })
  /////////////////////////////////////////////

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
        val decl = IR_VariableDeclaration(dest.datatype, "selfassignTmp_" + tmpCounter, src)
        newStmts += decl
        stmt.src = IR_VariableAccess(decl)
        newStmts += stmt
        //newStmts += IR_Assignment(dest, IR_VariableAccess(decl))
        tmpCounter += 1
        newStmts
      } else {
        stmt
      }
    case stmt @ IR_Assignment(dest : IR_FieldAccess, src, _) if (dest.datatype.isInstanceOf[IR_MatrixDatatype])    =>
      // resolve M = M * M into tmp = M * M; M = tmp
      var selfassign = false
      StateManager.findAll[IR_Multiplication](HelperNode(src)).foreach(mult =>
        if (mult.factors.exists(p => p.isInstanceOf[IR_FieldAccess] && p.asInstanceOf[IR_FieldAccess].name == dest.name))
          selfassign = true
      )

      if (selfassign) {
        var newStmts = ListBuffer[IR_Statement]()
        val decl = IR_VariableDeclaration(dest.datatype, "selfassignTmp_" + tmpCounter, src)
        newStmts += decl
        stmt.src = IR_VariableAccess(decl)
        newStmts += stmt
        //newStmts += IR_Assignment(dest, IR_VariableAccess(decl))
        tmpCounter += 1
        newStmts
      } else {
        stmt
      }
  })

  ///////////////////////////////////////////// extraction

  var extDeclHolder = ListBuffer[IR_Statement]()
  var extDeclCounter = 0

  object IR_ExtractFunctionCalls extends QuietDefaultStrategy("extract") {
    this += new Transformation("extract", {
      case e : IR_ExtractableMNode =>
        val extAcc = IR_VariableAccess("fct_" + e.name + "_" + extDeclCounter, e.datatype)
        extDeclCounter += 1
        IR_ExtractFunctionCalls.applyStandalone(e)
        extDeclHolder += IR_VariableDeclaration(extAcc, Duplicate(e))
        //extDeclHolder += IR_Assignment(extAcc, Duplicate(e))
        extAcc
    }, false)
  }

  this += new Transformation("extract function calls", {
    case stmt @ (_ : IR_Assignment | _ : IR_VariableDeclaration | _ : IR_ExpressionStatement) =>
      var newstmts = ListBuffer[IR_Statement]()
      IR_ExtractFunctionCalls.applyStandalone(stmt)
      newstmts ++= Duplicate(extDeclHolder)
      extDeclHolder.clear()
      newstmts += stmt.asInstanceOf[IR_Statement]
      newstmts
  })
  /*
  this += new Transformation("extract function calls 1/2", {
    case stmt @ (IR_Assignment(_, src, _)) // if (src.datatype.isInstanceOf[IR_MatrixDatatype])
    =>
      // Extract all function calls into separate variables since any function could have unwanted side effects if called more than once
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_ExtractableMNode](src).foreach(exp => {
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
      newStmts += stmt.asInstanceOf[IR_Statement]
      newStmts
    case stmt @ IR_VariableDeclaration(_,_,init,_) if(init.isDefined )
      //init.get.datatype.isInstanceOf[IR_MatrixDatatype])
    =>
      // Extract all function calls into separate variables since any function could have unwanted side effects if called more than once
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_ExtractableMNode](init.get).foreach(exp => {
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
      newStmts += stmt.asInstanceOf[IR_Statement]
      newStmts
    case stmt @ IR_ExpressionStatement(src) if (src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_ExtractableMNode](src).foreach(exp => { // resolveFunction check not needed: all internally resolved function return a value
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

  this += new Transformation("extract function calls 2/2", {
    case exp : IR_ExtractableMNode if(exp.hasAnnotation(annotationFctCallCounter)) =>
      IR_VariableAccess("_fct" + exp.popAnnotationAs[Int](annotationFctCallCounter) + "_" + exp.name.replace('<', '_').replace('>', '_'), exp.datatype)

    case exp : IR_MatrixExpression if (exp.hasAnnotation(annotationMatExpCounter)) =>
      IR_VariableAccess("_matrixExp" + exp.popAnnotationAs[Int](annotationMatExpCounter), exp.datatype)
  })
*/
  //////////////////////////////////////////////////////////////////////////

  this += new Transformation("global declarations", {
    case decl @ IR_VariableDeclaration(_ : IR_MatrixDatatype, _, Some(exp : IR_Expression), _) =>
      StateManager.findFirst[IR_GlobalCollection]().get.initGlobals.asInstanceOf[IR_Function].body += IR_Assignment(IR_VariableAccess(Duplicate(decl)), exp)
      decl.initialValue = None
      decl
  }, applyAtNode = StateManager.findFirst[IR_GlobalCollection]())
}

//////////////////////////////////////////////////////////// mat expr setup
object IR_SetupMatrixExpressions extends DefaultStrategy("Convert accesses to matrices and vectors to MatrixExpressions") {
  def duplicateExpressions(access : IR_Expression, dt : IR_MatrixDatatype) = {
    var expressions = ListBuffer[IR_Expression]()
    for (row <- 0 until dt.sizeM)
      for (col <- 0 until dt.sizeN)
        expressions += IR_HighDimAccess(Duplicate(access), IR_ConstIndex(row, col))
    expressions.toArray
  }

  this += Transformation("Wrap", {

    //case m @ IR_MatrixExpression(_, 1, 1, _)          => m.get(0, 0)
    //case IR_MatrixDatatype(dt, 1, 1)                  => dt
    case m : IR_MatrixExpression => m // no need to process further
    case hda : IR_HighDimAccess  => hda // no need to process further
    //case x : IR_FunctionCall if (x.name == "inverse") => x

    case x : IR_GetSlice                                                         => x
    case x : IR_SetSlice                                                         => x
    case x : IR_GetElement                                                       => x
    case x : IR_SetElement                                                       => x
    case a @ IR_Assignment(_, x : IR_IntermediateInv, _) if (x.resolveAtRuntime) => a

    case epstmt @ IR_ExpressionStatement(call @ IR_FunctionCall(_, _)) if (call.name == "compare") => epstmt

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

///////////////////////////////////////////////////////////////

/** Strategy: resolve matrix functions */
object IR_ResolveMatFuncs extends DefaultStrategy("Resolve matFuncs") {
  // collector to check for writes to variables
  var variableCollector = new IR_MatrixVarCollector()
  this.register(variableCollector)
  this.onBefore = () => this.resetCollectors()

  /** Attribute: Map to convert intermediate matrix function nodes to resolvable compiletime nodes */
  val ctFctMap = Map[String, IR_RuntimeMNode => IR_Expression](
    ("inverse", IR_InverseCT.apply),
    ("getSlice", IR_GetSliceCT.apply),
    ("determinant", IR_DeterminantCT.applyWithCheck)
  )

  /** Attribute: Map to convert intermediate matrix function nodes to resolvable compiletime nodes */
  val rtFctMap = Map[String, (IR_Access, IR_RuntimeMNode) => IR_Statement](
    ("inverse", IR_InverseRT.apply),
    ("getSlice", IR_GetSliceRT.apply),
    ("determinant", IR_DeterminantRT.applyWithCheck)
  )

  /** Transformation: replace special(eventually to resolve at runtime)
    * function nodes with their resolvable counterparts if they are ready (considered for inline)
    * and resolve
    */

  this += new Transformation("Insert resolvables and resolve", {
    case decl @ IR_VariableDeclaration(_, _, Some(r : IR_RuntimeMNode), _) if r.resolveAtRuntime =>
      IR_MatNodeUtils.splitDeclaration(decl)

    // not to resolve at runtime
    case r : IR_RuntimeMNode if !r.resolveAtRuntime   =>
      ctFctMap(r.name)(r)
    case mn : IR_ResolvableMNode if mn.isResolvable() =>
      mn.resolve()

    // to resolve at runtime
    case IR_Assignment(dest, r : IR_RuntimeMNode, _) if r.resolveAtRuntime =>
      rtFctMap(r.name)(dest.asInstanceOf[IR_Access], r)

    case IR_ExpressionStatement(mn : IR_ResolvableMNode) if mn.isResolvable() =>
      mn.resolve()

    // debug
    case IR_ExpressionStatement(call @ IR_FunctionCall(_, args)) if (call.name == "compare") =>
      IR_GenerateBasicMatrixOperations.compare(args(0), args(1), args(2), if (args.length == 4 && args(3).isInstanceOf[IR_BooleanConstant] && args(3).asInstanceOf[IR_BooleanConstant].value) true else false)
  })

}

/** Strategy:  resolve Matrix operators like addition if the operands are ready */
object IR_ResolveMatOperators extends DefaultStrategy("Resolve operators") {

  import exastencils.baseExt.ir.IR_MatNodeUtils.checkIfMatOp
  import exastencils.baseExt.ir.IR_MatNodeUtils.isEvaluatable
  import exastencils.baseExt.ir.IR_MatNodeUtils.isMatOp

  this += new Transformation("resolve", {
    //TODO match on supertype? -> introduce supertype
    case mult @ IR_Multiplication(facs) if (checkIfMatOp(mult) && facs.forall(f => isEvaluatable(f)))                                                                =>
      mult.removeAnnotation(isMatOp)
      IR_CompiletimeMatOps.mult(mult)
    case add @ (IR_Addition(sums)) if (checkIfMatOp(add) && sums.forall(f => isEvaluatable(f)) && !add.hasAnnotation(IR_GenerateRuntimeInversion.pointerArithmetic)) =>
      add.removeAnnotation(isMatOp)
      IR_CompiletimeMatOps.add(add)
    case binOp @ IR_ElementwiseSubtraction(_, _) if (checkIfMatOp(binOp) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))                                 =>
      binOp.removeAnnotation(isMatOp)
      IR_CompiletimeMatOps.sub(binOp)
    case binOp @ IR_Subtraction(_, _) if (checkIfMatOp(binOp) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))                                            =>
      binOp.removeAnnotation(isMatOp)
      IR_CompiletimeMatOps.sub(binOp)
    case binOp @ IR_ElementwiseMultiplication(_, _) if (checkIfMatOp(binOp) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))                              =>
      binOp.removeAnnotation(isMatOp)
      IR_CompiletimeMatOps.elementwiseMultiplication(binOp.left, binOp.right)
    case binOp @ IR_ElementwiseDivision(_, _) if (checkIfMatOp(binOp) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))                                    =>
      binOp.removeAnnotation(isMatOp)
      IR_CompiletimeMatOps.elementwiseDivision(binOp.left, binOp.right)
    case binOp @ IR_ElementwiseAddition(_, _) if (checkIfMatOp(binOp) && isEvaluatable(binOp.left) && isEvaluatable(binOp.right))                                    =>
      binOp.removeAnnotation(isMatOp)
      IR_CompiletimeMatOps.add(binOp)
  })
}

/** Strategy: resolve "Var matrix : Matrix<Datatype, rows, columns> = initialization" or split to declaration and assignment if convenient */
object IR_PostItMOps extends DefaultStrategy("Resolve matrix decls and assignments") {
  var debug = false
  val annotationMatrixRow = "IR_ResolveMatrices.matrixRow"
  val annotationMatrixCol = "IR_ResolveMatrices.matrixCol"
  val annotationFctCallCounter = "IR_ResolveMatrices.fctCallCounter"
  var fctCallCounter = 0
  // temporary variable used to replace function calls in expressions
  val annotationMatExpCounter = "IR_ResolveMatrices.matrixExpressionCounter"
  var matExpCounter = 0
  var resolveFunctions = ListBuffer[String]()
  var globalCollection : Option[IR_GlobalCollection] = None

  this.onBefore = () => {
    resolveFunctions.clear()
    resolveFunctions ++= ListBuffer("dotProduct", "dot", "crossProduct", "cross", "det", "transpose", "inverse", "getSlice", "setSlice", "getElement", "setElement", "set", "get")
    globalCollection = StateManager.findFirst[IR_GlobalCollection]()
  }

  /** Transformation: resolve or split declarations */
  this += new Transformation("decls", {
/*
    // split to use std::fill later
    case decl @ IR_VariableDeclaration(IR_MatrixDatatype(_, _, _), _, Some(init), _) if (IR_MatNodeUtils.isScalar(init)) =>
      IR_MatNodeUtils.splitDeclaration(decl)

    // do nothing
    case decl @ IR_VariableDeclaration(declDt @ IR_MatrixDatatype(_, _, _), _, Some(srcDt @ IR_MatrixExpression(_, _, _, _)), _) =>
      if (declDt.sizeM != srcDt.rows || declDt.sizeN != srcDt.columns)
        Logger.error(s"Declaration of variable of type: $declDt with expression of type: $srcDt, sizes must match!")
      decl

    // split to use std::copy later
    case decl @ IR_VariableDeclaration(declDt @ IR_MatrixDatatype(_, _, _), _, Some(IR_VariableAccess(_, srcDt @ IR_MatrixDatatype(_, _, _))), _) =>
      if (declDt.sizeM != srcDt.sizeM || declDt.sizeN != srcDt.sizeN)
        Logger.error(s"Declaration of variable of type: $declDt with expression of type: $srcDt, sizes must match!")
      IR_MatNodeUtils.splitDeclaration(decl)
*/
    // add helper matrix  decls from schur compiletime inversion
    case stmt @ (IR_VariableDeclaration(_, _, _, _) | IR_Assignment(_, _, _)) if (Knowledge.experimental_schurWithHelper) =>
      val ms = StateManager.findAll[IR_MatrixExpression](stmt).filter(x => if (x.hasAnnotation("helperMatrices")) true else false)
      val helperDecls = ListBuffer[IR_Statement]()
      for (m <- ms) {
        helperDecls ++= m.popAnnotationAs[IR_VariableDeclaration]("helperMatrices").asInstanceOf[ListBuffer[IR_Statement]]
      }
      helperDecls += stmt.asInstanceOf[IR_Statement]
      helperDecls

    // add pivot elements and checks of ct inversion
    case stmt @ (IR_VariableDeclaration(_, _, _, _) | IR_Assignment(_, _, _)) if (Knowledge.experimental_checkCTInversionPivots) =>
      var newstmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_MatrixExpression](stmt)
        .filter(mexpr => if (mexpr.hasAnnotation("checkCTInversionPivots")) true else false)
        .map(mexpr => {
          // mexpr.removeAnnotation("checkCTInversionPivots")
          IR_GenerateBasicMatrixOperations.pivotCheck(mexpr.popAnnotationAs[IR_MatrixExpression]("checkCTInversionPivots"))
        })
        .foreach(stmts => newstmts ++= stmts)
      newstmts += stmt.asInstanceOf[IR_Statement]
      newstmts
  }, false)

  /** Transformation: resolve assignments */
  this += new Transformation("assignments", {
    // use std::fill for assignments of matrices with constants
    case IR_Assignment(dest : IR_Access, src, "=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && !dest.isInstanceOf[IR_FieldAccess] && IR_MatNodeUtils.isScalar(src)) =>
      IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::fill", IR_UnitDatatype), ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + IR_IntegerConstant(dest.datatype.asInstanceOf[IR_MatrixDatatype].resolveFlattendSize), src)))

    // assignment of a matrix with another matrix : copy other matrix
  //  case IR_Assignment(dest @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)), src @ (IR_MatrixExpression(_, _, _, _) | _ : IR_VariableAccess ), "=") if (src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
  //  IR_GenerateBasicMatrixOperations.copyMatrix(dest, src)

   case IR_Assignment(dest, src : IR_Access, "=") if dest.datatype.isInstanceOf[IR_MatrixDatatype] && !dest.isInstanceOf[IR_MatrixExpression] && src.datatype.isInstanceOf[IR_MatrixDatatype] =>
      val dt = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      IR_ExpressionStatement(IR_FunctionCall("std::copy", ListBuffer[IR_Expression](Duplicate(src), Duplicate(src) + IR_IntegerConstant(dt.resolveFlattendSize), dest)))


    // other assignments
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

  this += new Transformation("setup matrix entries", {
    case exp : IR_MatrixExpression if (exp.hasAnnotation(annotationMatrixRow)) =>
      exp.get(exp.popAnnotationAs[Int](annotationMatrixRow), exp.popAnnotationAs[Int](annotationMatrixCol))

    case exp : IR_Expression if (exp.hasAnnotation(annotationMatrixRow)) =>
      IR_HighDimAccess(Duplicate(exp), IR_ConstIndex(Array(exp.popAnnotationAs[Int](annotationMatrixRow), exp.popAnnotationAs[Int](annotationMatrixCol))))
  }, false)

  /** Transformation: simplify matrices e.g. neg(mat) to negated entries and resolve user defined functions */
  this += new Transformation("simplify", {
    case IR_Negative(m : IR_MatrixExpression)                               => m.expressions = m.expressions.map { y => IR_Negative(y) : IR_Expression }; m
    case IR_Negative(va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))) =>
      var m = IR_MatNodeUtils.accessToExpression(va)
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

    case IR_Assignment(dest, src : IR_FunctionCall, "=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      // FIXME resolve IR_Assignments with operator += before this
      src.arguments += dest
      IR_ExpressionStatement(src)
  })
  /*
    this += new Transformation("simplify function call arguments", {
      case stmt @ IR_ExpressionStatement(exp : IR_FunctionCall)                                               =>
        var newStmts = ListBuffer[IR_Statement]()

        exp.arguments.transform {
          case argexp : IR_MultiDimFieldAccess                                             => argexp
          case argexp : IR_VariableAccess                                                  => argexp
          case argexp : IR_Expression if (argexp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
            var decl = IR_VariableDeclaration(argexp.datatype, "_matrixExp_" + matExpCounter, argexp)
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
            var decl = IR_VariableDeclaration(argexp.datatype, "_matrixExp_" + matExpCounter, argexp)
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
        IR_ArrayAccess(base, IR_IntegerConstant(matrix.sizeN * idx.indices(0) + idx.indices(1)))
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
