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

package exastencils.optimization.ir

import scala.collection.mutable.{ ArrayBuffer, HashMap, ListBuffer, Map, Queue }
import scala.util.DynamicVariable

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.domain.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.cuda._
import exastencils.simd._

object IR_Vectorization extends DefaultStrategy("Vectorization") {

  final val VECT_ANNOT : String = "VECT"
  final val COND_VECTABLE : String = "VECT_C"
  final val COND_IGN_INCR : String = "VECT_ign++"

  this += new Transformation("optimize", VectorizeInnermost, recursive = false, isParallel = true)
}

final class VectorizationException(val msg : String) extends Exception(msg)

private object VectorizeInnermost extends PartialFunction[Node, Transformation.OutputType] {

  private val DEBUG : Boolean = false

  private val skipFunction = new DynamicVariable[Boolean](false)

  override def isDefinedAt(node : Node) : Boolean = {
    // do not vectorize device code!
    val cuAnn = CUDA_Util.CUDA_LOOP_ANNOTATION
    node match {
      case n if n.hasAnnotation(cuAnn) => skipFunction.value = true
      case _ : CUDA_Kernel             => skipFunction.value = true
      case _ : IR_FunctionLike         => skipFunction.value = false
      case _                           => // no change in skipFunction
    }
    if (skipFunction.value)
      return false

    node.removeAnnotation(IR_AddressPrecalculation.ORIG_IND_ANNOT) // remove old annotations
    node match {
      case loop : IR_ForLoop =>
        loop.parallelization.isInnermost &&
          (loop.parallelization.potentiallyParallel || loop.parallelization.isVectorizable) &&
          !loop.hasAnnotation(IR_Vectorization.VECT_ANNOT)
      case _                 =>
        false
    }
  }

  override def apply(node : Node) : Transformation.OutputType = {
    try {
      vectorizeLoop(node.asInstanceOf[IR_ForLoop])
    } catch {
      case ex : VectorizationException =>
        if (DEBUG) {
          val msg : String = "[vect]  unable to vectorize loop: " + ex.msg + "  (line " + ex.getStackTrace()(0).getLineNumber + ')'
          println(msg) // print directly, logger may be silenced by any surrounding strategy
          return List(IR_Comment(msg), node)
        }
        node
    }
  }

  private def vectorizeLoop(loop : IR_ForLoop) : IR_Statement = {

    // excessive testing if loop header allows vectorization
    loop match {
      case IR_ForLoop(IR_VariableDeclaration(IR_IntegerDatatype, itName, Some(lBound), _), condExpr, incrExpr, body, parallelization) =>

        val uBoundExcl : IR_Expression =
          condExpr match {
            case IR_Lower(IR_VariableAccess(bName, IR_IntegerDatatype), upperBoundExcl) if itName == bName      =>
              upperBoundExcl
            case IR_LowerEqual(IR_VariableAccess(bName, IR_IntegerDatatype), upperBoundIncl) if itName == bName =>
              IR_Addition(upperBoundIncl, IR_IntegerConstant(1))
            case _                                                                                              =>
              throw new VectorizationException("no upper bound")
          }

        val incr : Long =
          incrExpr match {
            case IR_ExpressionStatement(IR_PreIncrement(IR_VariableAccess(n, IR_IntegerDatatype))) if itName == n  => 1L
            case IR_ExpressionStatement(IR_PostIncrement(IR_VariableAccess(n, IR_IntegerDatatype))) if itName == n => 1L
            case IR_Assignment(IR_VariableAccess(n, IR_IntegerDatatype),
            IR_IntegerConstant(i),
            "+=") if itName == n                                                                                   => i
            case IR_Assignment(IR_VariableAccess(n1, IR_IntegerDatatype),
            IR_Addition(ListBuffer(IR_IntegerConstant(i), IR_VariableAccess(n2, IR_IntegerDatatype))),
            "=") if itName == n1 && itName == n2                                                                   => i
            case IR_Assignment(IR_VariableAccess(n1, IR_IntegerDatatype),
            IR_Addition(ListBuffer(IR_VariableAccess(n2, IR_IntegerDatatype), IR_IntegerConstant(i))),
            "=") if itName == n1 && itName == n2                                                                   => i
            case _                                                                                                 => throw new VectorizationException("loop increment must be constant or cannot be extracted:  " + incrExpr)
          }

        vectorizeLoop(Duplicate(loop), itName, lBound, uBoundExcl, incr, body, parallelization.reduction)

      case _ => throw new VectorizationException("cannot analyze loop (yet)")
    }
  }

  private final class LoopCtx(val itName : String, val incr : Long) {

    private val vectStmtsStack = new ArrayBuffer[ListBuffer[IR_Statement]]()
    var storesTmp : IR_Statement = null
    var ignIncr : Boolean = false
    var divResidue : Long = -1

    private val preLoopStmts = new ListBuffer[IR_Statement]()
    private val postLoopStmts = new ListBuffer[IR_Statement]()

    val toFinish_LCSE = Map[String, SIMD_ConcShift]()

    private val temporaryMappingStack = new ArrayBuffer[Map[IR_Expression, String]]()
    private val temporaryProperties = Map[String, (Boolean, Boolean)]()
    private var isStore_ : Boolean = false
    private var varID : Int = -1
    private var incrVectDeclared : Boolean = false
    private var alignedResidue : Long = -1
    private val nameTempl : String = "_vec%02d"

    // init
    pushScope()

    def addStmt(stmt : IR_Statement) : Unit = {
      vectStmtsStack.last += stmt
    }

    def addStmtPreLoop(decl : IR_VariableDeclaration, origExpr : IR_Expression) : Unit = {
      moveNameMappingUp(origExpr)
      preLoopStmts += decl
    }

    def getPreLoopStmts() : ListBuffer[IR_Statement] = {
      preLoopStmts
    }

    def addStmtPostLoop(stmt : IR_Statement) : Unit = {
      postLoopStmts += stmt
    }

    def getPostLoopStmts() : ListBuffer[IR_Statement] = {
      postLoopStmts
    }

    def pushScope() : Unit = {
      temporaryMappingStack += Map[IR_Expression, String]()
      vectStmtsStack += new ListBuffer[IR_Statement]()
    }

    def popScope() : ListBuffer[IR_Statement] = {
      temporaryMappingStack.remove(temporaryMappingStack.length - 1)
      vectStmtsStack.remove(vectStmtsStack.length - 1)
    }

    def getName(expr : IR_Expression) : (String, Boolean) = {
      var i = temporaryMappingStack.length - 1
      while (i >= 0) {
        val nameOpt : Option[String] = temporaryMappingStack(i).get(expr)
        if (nameOpt.isDefined)
          return (nameOpt.get, false)
        i -= 1
      }
      varID += 1
      val name : String = nameTempl.format(varID)
      temporaryMappingStack.last.update(expr, name)
      (name, true)
    }

    def moveNameMappingUp(expr : IR_Expression) : Unit = {
      temporaryMappingStack.head(expr) = temporaryMappingStack(temporaryMappingStack.length - 1).remove(expr).get
    }

    def getIncrVector() : IR_VariableAccess = {
      val name : String = "_veci"
      if (!incrVectDeclared) {
        SIMD_IncrementVectorDeclaration(name, incr) +=: preLoopStmts
        incrVectDeclared = true
      }
      IR_VariableAccess(name, SIMD_RealDatatype)
    }

    def setLoad() : this.type = {
      isStore_ = false
      this
    }

    def setStore() : this.type = {
      isStore_ = true
      this
    }

    def isLoad() : Boolean = {
      !isStore_
    }

    def isStore() : Boolean = {
      isStore_
    }

    def setAlignAndAccess1(tmp : String, alignment : Boolean, access1 : Boolean) : Unit = {
      temporaryProperties(tmp) = (alignment, access1)
    }

    def getAlignAndAccess1(tmp : String) : (Boolean, Boolean) = {
      temporaryProperties(tmp)
    }

    def setAlignedResidue(residue : Long) : Unit = {
      if (alignedResidue < 0)
        alignedResidue = residue
      else if (alignedResidue != residue)
        throw new VectorizationException("Stores with different alignment are not allowed.")
    }

    def getAlignedResidue() : Long = {
      alignedResidue
    }
  }

  private def containsVarAcc(node : IR_Node, varName : String) : Boolean = {
    var found = false
    val search = new QuietDefaultStrategy("Find VariableAccess...")
    search += new Transformation("seaching...", {
      case acc @ IR_VariableAccess(name, _) if name == varName =>
        found = true
        acc
    })
    // ensure node itself is found, too
    search.applyStandalone(IR_Root(ListBuffer(node)))
    found
  }

  private def vectorizeLoop(oldLoop : IR_ForLoop, itVar : String, begin : IR_Expression, endExcl : IR_Expression,
      incr : Long, body : ListBuffer[IR_Statement], reduction : Option[IR_Reduction]) : IR_Statement = {

    val ctx = new LoopCtx(itVar, incr)
    var postLoopStmt : IR_Statement = null
    if (reduction.isDefined) {
      val target = reduction.get.target
      val operator = reduction.get.op

      val (vecTmp : String, true) = ctx.getName(target)
      val identityElem : IR_Expression =
        operator match {
          case "+"   => SIMD_Scalar2Vector(IR_RealConstant(0.0))
          case "*"   => SIMD_Scalar2Vector(IR_RealConstant(1.0))
          case "min" => SIMD_Scalar2Vector(IR_RealConstant(Double.MaxValue))
          case "max" => SIMD_Scalar2Vector(IR_RealConstant(Double.MinValue))
          case _     => throw new VectorizationException("unknown reduction operator:  " + operator)
        }
      ctx.addStmtPreLoop(IR_VariableDeclaration(SIMD_RealDatatype, vecTmp, identityElem), target)

      val vecTmpAcc = IR_VariableAccess(vecTmp, SIMD_RealDatatype)
      postLoopStmt =
        operator match {
          case "+"   => SIMD_HorizontalAdd(Duplicate(target), vecTmpAcc)
          case "*"   => SIMD_HorizontalMul(Duplicate(target), vecTmpAcc)
          case "min" => SIMD_HorizontalMin(Duplicate(target), vecTmpAcc)
          case "max" => SIMD_HorizontalMax(Duplicate(target), vecTmpAcc)
        }
    }

    // ensure all stores are aligned (heuristics)
    var alignmentExpr : IR_Expression = null
    val vs = Platform.simd_vectorSize
    if (Knowledge.data_alignFieldPointers) {
      for (stmt <- body) {
        stmt match {
          case IR_Assignment(acc @ IR_ArrayAccess(_, index, true), _, _) =>
            val annot = acc.getAnnotation(IR_AddressPrecalculation.ORIG_IND_ANNOT)
            val ind : IR_Expression = if (annot.isDefined) annot.get.asInstanceOf[IR_Expression] else index
            val indExprs = IR_SimplifyExpression.extractIntegralSum(ind)
            val const : Long = indExprs.remove(IR_SimplifyExpression.constName).getOrElse(0L)
            for (iE <- indExprs) iE match {
              case (IR_Division(divd, IR_IntegerConstant(divs)), 1L)                       =>
                val sum = IR_SimplifyExpression.extractIntegralSum(divd)
                ctx.divResidue = (sum.remove(IR_SimplifyExpression.constName).getOrElse(0L) % divs + divs) % divs
                for (s <- sum) s match {
                  case (IR_VariableAccess(name, IR_IntegerDatatype), 1L) if name == ctx.itName =>
                    // is ok
                  case (exp, _) if containsVarAcc(exp, ctx.itName) =>
                    throw new VectorizationException("cannot deal with summand \"" + iE._2 + " * " + iE._1.prettyprint() + '"')
                  case _                                           =>
                    // is ok
                }
              case (IR_VariableAccess(name, IR_IntegerDatatype), 1L) if name == ctx.itName =>
                // nothing to do here
              case (mod : IR_Modulo, _) =>
                if (containsVarAcc(mod, ctx.itName))
                  throw new VectorizationException("no linear memory access: " + mod.prettyprint())
              case _                    =>
                if (containsVarAcc(iE._1, ctx.itName))
                  throw new VectorizationException("cannot deal with summand \"" + iE + '"')
            }
            val residue : Long = (const % vs + vs) % vs
            ctx.setAlignedResidue(residue)
            alignmentExpr = ind
          case _                                                         =>
        }
      }

      val indexExprs = new ListBuffer[HashMap[IR_Expression, Long]]()
      val collectIndexExprs = new QuietDefaultStrategy("Collect all array index expressions...")
      collectIndexExprs += new Transformation("seaching...", {
        case acc @ IR_ArrayAccess(_, index, true) =>
          if (containsVarAcc(index, ctx.itName)) {
            val annot = acc.removeAnnotation(IR_AddressPrecalculation.ORIG_IND_ANNOT)
            indexExprs += IR_SimplifyExpression.extractIntegralSum(if (annot.isDefined) annot.get.asInstanceOf[IR_Expression] else index)
          }
          acc
      })
      collectIndexExprs.applyStandalone(body)

      // no store available, so align as many loads as possible
      if (alignmentExpr == null) {
        alignmentExpr = IR_SimplifyExpression.recreateExprFromIntSum(indexExprs.head)
        val counts = new Array[Long](vs)
        for (ind <- indexExprs) {
          val const : Long = ind.remove(IR_SimplifyExpression.constName).getOrElse(0L)
          val residue : Long = (const % vs + vs) % vs
          counts(residue.toInt) += 1
        }
        var max = (0, counts(0))
        for (i <- 1 until vs)
          if (counts(i) > max._2)
            max = (i, counts(i))
        ctx.setAlignedResidue(max._1)

      } else {
        for (ind <- indexExprs)
          ind.remove(IR_SimplifyExpression.constName)
      }

      if (Knowledge.simd_avoidUnaligned) {
        // check if index expressions are "good", i.e., all (except the constant summand) have the same residue
        while (indexExprs.head.nonEmpty) {
          val key : IR_Expression = indexExprs.head.head._1
          var residue : Long = -1
          for (ind <- indexExprs) {
            val res = (ind.remove(key).getOrElse(0L) % vs + vs) % vs
            if (residue < 0)
              residue = res
            else if (res != residue)
              throw new VectorizationException("Cannot determine alignment properly")
          }
        }
        // at least one sum is empty, so all remaining coefficients must be evenly divisible
        for (ind <- indexExprs)
          for ((_, coeff) <- ind)
            if (coeff % vs != 0)
              throw new VectorizationException("Cannot determine alignment properly")
      }
    }

    for (stmt <- body)
      vectorizeStmt(stmt, ctx)

    def itVarAcc = IR_VariableAccess(itVar, IR_IntegerDatatype)

    val newIncr : Long = incr * vs

    oldLoop.begin = IR_VariableDeclaration(IR_IntegerDatatype, itVar, IR_Unrolling.startVarAcc)
    oldLoop.end = IR_Lower(itVarAcc, IR_Unrolling.intermVarAcc)
    oldLoop.inc = IR_Assignment(itVarAcc, IR_IntegerConstant(newIncr), "+=")
    oldLoop.body = ctx.popScope()

    var postLoop : IR_Statement = null
    val annot = oldLoop.removeAnnotation(IR_Unrolling.UNROLLED_ANNOT)
    val unrolled : Boolean = annot.isDefined
    var res : ListBuffer[IR_Statement] = null
    if (unrolled) {
      res = new ListBuffer[IR_Statement]()
    } else {
      // old AST will be replaced completely, so we can reuse the body once here (and duplicate later)
      val (boundsDecls, postLoop_) : (ListBuffer[IR_Statement], IR_Statement) =
        IR_Unrolling.getBoundsDeclAndPostLoop(itVar, begin, endExcl, incr, body, Duplicate(reduction))
      postLoop = postLoop_
      res = boundsDecls
    }

    if (Knowledge.data_alignFieldPointers) {
      // no need to ensure alignment of iteration variable if data is not aligned
      val preEndVar : String = "_preEnd"

      def preEndVarAcc = IR_VariableAccess(preEndVar, IR_IntegerDatatype)

      val wrappedAlignExpr = IR_ExpressionStatement(Duplicate(alignmentExpr))
      val replItVar = new QuietDefaultStrategy("Replace iteration variable...")
      replItVar += new Transformation("seaching...", {
        case IR_VariableAccess(name, _) if name == itVar =>
          IR_Unrolling.startVarAcc
      })
      // ensure node itself is found, too
      replItVar.applyStandalone(wrappedAlignExpr)
      // don't forget to multiply alignment correction summand by the divisor of the loop iterator in the access, which must correspond to the loop increment
      val preEndExpr = IR_Minimum(IR_Unrolling.endVarAcc,
        IR_Unrolling.startVarAcc + (IR_IntegerConstant(incr) * ((IR_IntegerConstant(vs) - (wrappedAlignExpr.expression Mod IR_IntegerConstant(vs))) Mod IR_IntegerConstant(vs))))
      res += IR_VariableDeclaration(IR_IntegerDatatype, preEndVar, preEndExpr)

      res += IR_ForLoop(IR_VariableDeclaration(IR_IntegerDatatype, itVar, IR_Unrolling.startVarAcc),
        IR_Lower(itVarAcc, preEndVarAcc),
        IR_Assignment(itVarAcc, IR_IntegerConstant(incr), "+="),
        Duplicate(body))

      res += IR_Assignment(IR_Unrolling.startVarAcc, preEndVarAcc, "=")
    }
    var intermDecl : IR_VariableDeclaration = null
    if (unrolled) {
      intermDecl = annot.get.asInstanceOf[IR_VariableDeclaration]
      intermDecl.initialValue = Some(IR_Unrolling.getIntermExpr(newIncr))
    } else {
      intermDecl = IR_Unrolling.getIntermDecl(newIncr)
      res += intermDecl
    }

    val emptyLoopGuard = IR_IfCondition(IR_Lower(IR_Unrolling.startVarAcc, IR_Unrolling.intermVarAcc), new ListBuffer[IR_Statement]())
    emptyLoopGuard.trueBody ++= ctx.getPreLoopStmts()
    emptyLoopGuard.trueBody += oldLoop
    emptyLoopGuard.trueBody ++= ctx.getPostLoopStmts()
    if (postLoopStmt != null)
      emptyLoopGuard.trueBody += postLoopStmt
    res += emptyLoopGuard
    if (!unrolled)
      res += postLoop

    oldLoop.annotate(IR_Vectorization.VECT_ANNOT)
    oldLoop.annotate(IR_Unrolling.UNROLLED_ANNOT, intermDecl)
    IR_Scope(res)
  }

  private def vectorizeStmt(stmt : IR_Statement, ctx : LoopCtx) : Unit = {
    stmt match {
      case IR_NullStatement => // SIMD_NullStatement? no way...

      case IR_Comment(str) =>
        ctx.addStmt(IR_Comment(str)) // new instance

      case IR_Assignment(lhsSca, rhsSca, assOp) =>
        ctx.addStmt(IR_Comment(stmt.prettyprint()))
        val srcWrap = IR_ExpressionStatement(Duplicate(assOp match {
          case "="  => rhsSca
          case "+=" => IR_Addition(lhsSca, rhsSca)
          case "-=" => IR_Subtraction(lhsSca, rhsSca)
          case "*=" => IR_Multiplication(lhsSca, rhsSca)
          case _    => throw new VectorizationException("cannot deal with assignment operator \"" + assOp + "\" in " + stmt.prettyprint())
        }))
        IR_GeneralSimplify.doUntilDoneStandalone(srcWrap)
        IR_GeneralSimplify.doUntilDoneStandalone(lhsSca) // simplify lhsSca too, to ensure identical array accesses have the same AST structure
        // create rhs before lhs to ensure all loads are created
        val rhsVec = vectorizeExpr(srcWrap.expression, ctx.setLoad())
        val lhsVec = vectorizeExpr(lhsSca, ctx.setStore())
        // ---- special handling of loop-carried cse variables ----
        lhsSca match {
          case IR_ArrayAccess(_ : IR_IV_LoopCarriedCSBuffer, _, _) =>
            val initOpt : Option[SIMD_ConcShift] = ctx.toFinish_LCSE.get(ctx.getName(lhsSca)._1)
            if (initOpt.isDefined) {
              val concShiftRight : IR_VariableAccess =
                rhsVec match {
                  case va : IR_VariableAccess => Duplicate(va)
                  case _                      => throw new VectorizationException("cannot vectorize code with lcse buffer and conventional CSE disabled")
                }
              initOpt.get.right = concShiftRight
            }
          case _                                                   => // nothing to do
        }
        // --------------------------------------------------------
        ctx.addStmt(IR_Assignment(lhsVec, rhsVec, "="))
        if (ctx.storesTmp != null)
          ctx.addStmt(ctx.storesTmp)
        ctx.storesTmp = null

      case decl @ IR_VariableDeclaration(IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype, name, Some(init), _) =>
        ctx.addStmt(IR_Comment(stmt.prettyprint()))
        val initWrap = IR_ExpressionStatement(Duplicate(init))
        IR_GeneralSimplify.doUntilDoneStandalone(initWrap)
        val initVec = vectorizeExpr(initWrap.expression, ctx.setLoad())
        val (vecTmp : String, true) = ctx.getName(IR_VariableAccess(name, decl.datatype))
        ctx.addStmt(new IR_VariableDeclaration(SIMD_RealDatatype, vecTmp, Some(initVec)))

      case IR_VariableDeclaration(dt : IR_HigherDimensionalDatatype, name, None, _) =>
        ctx.addStmt(IR_Comment(stmt.prettyprint()))
        val (vecTmp : String, true) = ctx.getName(IR_VariableAccess(name, dt))
        val newDt =
          dt match {
            case IR_MatrixDatatype(IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype, m, n) => IR_MatrixDatatype(SIMD_RealDatatype, m, n)
            case IR_VectorDatatype(IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype, m, n) => IR_VectorDatatype(SIMD_RealDatatype, m, n)
            case _                                                                               => throw new VectorizationException("cannot deal with datatype of variable declaration: " + dt + "; " + stmt.prettyprint())
          }
        ctx.addStmt(new IR_VariableDeclaration(newDt, vecTmp, None))

      case IR_IfCondition(cond, trueBody, falseBody) if stmt.hasAnnotation(IR_Vectorization.COND_VECTABLE) =>
        if (stmt.hasAnnotation(IR_Vectorization.COND_IGN_INCR))
          ctx.ignIncr = true
        ctx.pushScope()
        for (s <- trueBody)
          vectorizeStmt(s, ctx)
        val trueBodyVec = ctx.popScope()
        ctx.pushScope()
        for (s <- falseBody)
          vectorizeStmt(s, ctx)
        val falseBodyVec = ctx.popScope()
        ctx.ignIncr = false
        val njuCond = IR_IfCondition(Duplicate(cond), trueBodyVec, falseBodyVec)
        ctx.addStmt(njuCond)

      case _ @ IR_IfCondition(cond, trueBody, falseBody) =>

        ctx.pushScope()
        ctx.addStmt(IR_Comment("if (" + cond.prettyprint() + ")"))

        // evaluate condition: declare mask and init
        val (maskName, _) = ctx.getName(IR_VariableAccess("condMask", SIMD_MaskDatatype))
        val mask = IR_VariableAccess(maskName, SIMD_MaskDatatype)
        ctx.addStmt(IR_VariableDeclaration(mask, vectorizeExpr(cond, ctx.setLoad())))

        // vectorize body: assignments must be blended with mask
        def vectorize(body : ListBuffer[IR_Statement], cond : IR_Expression) = {
          if (body.nonEmpty) {
            ctx.pushScope()

            // statements in body: special handling for assignments
            body foreach {
              case assign @ IR_Assignment(lhsSca, rhsSca, op) =>
                ctx.addStmt(IR_Comment(assign.prettyprint()))

                // introduce copy for each (potentially) updated lhs
                ctx.addStmt(IR_Comment("Copy: " + lhsSca.prettyprint()))
                val (copyName, _) = ctx.getName(IR_VariableAccess("copyVec", SIMD_RealDatatype))
                val copy = IR_VariableAccess(copyName, SIMD_RealDatatype)
                vectorizeStmt(IR_Assignment(copy, lhsSca, op), ctx)

                vectorizeStmt(IR_Assignment(lhsSca, SIMD_Blendv(copy, rhsSca, mask), op), ctx)
              case _ : IR_IfCondition                         =>
                throw new VectorizationException("Cannot deal with nested conditions")
              case stmt : IR_Statement                        =>
                ctx.addStmt(IR_Comment(stmt.prettyprint()))
                vectorizeStmt(stmt, ctx)
              case _ =>
            }

            val newBodyVec = ctx.popScope()
            ctx.addStmt(IR_IfCondition(cond, newBodyVec)) // check if mask is non-zero, if yes then execute body
          }
        }

        def extractBitsMask(expr : IR_Expression) = IR_BitwiseAnd(expr, IR_Native(s"0b${List.fill(Platform.simd_vectorSize)("1").mkString}"))
        ctx.addStmt(IR_Comment("-- True branch --"))
        vectorize(trueBody, extractBitsMask(SIMD_MoveMask(mask)) > 0) // check if not all zeros
        ctx.addStmt(IR_Comment("-- False branch --"))
        vectorize(falseBody, extractBitsMask(IR_BitwiseNot(SIMD_MoveMask(mask))) > 0) // check if not all ones

        ctx.addStmt(IR_Scope(ctx.popScope()))

      case _ => throw new VectorizationException("cannot deal with " + stmt.getClass + "; " + stmt.prettyprint())
    }
  }

  private def vectorizeExpr(expr : IR_Expression, ctx : LoopCtx) : IR_Expression = {
    expr match {
      // TODO: do not vectorize if base is not aligned?
      case IR_ArrayAccess(base, index, alignedBase) =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp) {
          val inds : HashMap[IR_Expression, Long] = IR_SimplifyExpression.extractIntegralSum(index)
          val const : Option[Long] = inds.remove(IR_SimplifyExpression.constName)
          var access1 : Boolean = true
          var mayAligned : Boolean = true
          for (ind <- inds) ind match {
            case (IR_VariableAccess(name, IR_IntegerDatatype), value) =>
              if (name == ctx.itName) {
                if (value != 1L || ctx.incr != 1L)
                  throw new VectorizationException("no linear memory access;  loop increment: " + ctx.incr + "  index: " + index.prettyprint())
                access1 = false
              }

            case (IR_Division(divd, IR_IntegerConstant(divs)), 1L) if (ctx.incr == divs) =>
              val summands : HashMap[IR_Expression, Long] = IR_SimplifyExpression.extractIntegralSum(divd)
              val divdCst = summands.remove(IR_SimplifyExpression.constName).getOrElse(0L)
              for (s <- summands) s match {
                case (IR_VariableAccess(name, IR_IntegerDatatype), coeff) =>
                  if (name == ctx.itName) {
                    if (coeff != 1L)
                      throw new VectorizationException("no linear memory access;  loop increment: " + ctx.incr + "  index: " + index.prettyprint())
                    access1 = false
                    if (ctx.divResidue >= 0)
                      mayAligned &= (divdCst - ctx.divResidue) % divs == 0
                  }
                case (e, _)                                               =>
                  if (containsVarAcc(e, ctx.itName))
                    throw new VectorizationException("no linear memory access;  " + index.prettyprint())
              }

            case (e, _) =>
              if (containsVarAcc(e, ctx.itName))
                throw new VectorizationException("no linear memory access;  " + index.prettyprint())
          }
          if (ctx.ignIncr) // that means we basically only compute a scalar value
            access1 = true

          val vs = Platform.simd_vectorSize
          val aligned : Boolean = alignedBase && mayAligned && (const.getOrElse(0L) - ctx.getAlignedResidue()) % vs == 0
          base match {
            // ---- special handling of loop-carried cse variables ----
            case _ : IR_IV_LoopCarriedCSBuffer if access1 => //if(access1 && ctx.isStore() && !ctx.isLoad()) =>
              ctx.addStmtPreLoop(IR_VariableDeclaration(SIMD_RealDatatype, vecTmp, SIMD_Scalar2Vector(expr)), expr)
              ctx.addStmtPostLoop(IR_Assignment(expr, SIMD_ExtractScalar(IR_VariableAccess(vecTmp, SIMD_RealDatatype), vs - 1)))
            // ------------------------------------------------------
            case _ if ctx.isLoad() && !ctx.isStore() =>
              val init = Some(createLoadExpression(expr, base, inds, const.getOrElse(0L), access1, aligned, alignedBase, ctx))
              ctx.addStmt(new IR_VariableDeclaration(SIMD_RealDatatype, vecTmp, init))
            case _ if !ctx.isLoad() && ctx.isStore() =>
              ctx.addStmt(new IR_VariableDeclaration(SIMD_RealDatatype, vecTmp, None))
            case _                                   =>
              Logger.error("Only expected 'load XOR store', when vectorizing an ArrayAccess")
          }
          ctx.setAlignAndAccess1(vecTmp, aligned, access1)
        }
        val (aligned : Boolean, access1 : Boolean) = ctx.getAlignAndAccess1(vecTmp)
        if (ctx.isStore()) {
          // ---- special handling of loop-carried cse variables ----
          if (!base.isInstanceOf[IR_IV_LoopCarriedCSBuffer] || !access1) { // if we have an access to a single LCS buffer, we must not do anything special here, just skip all sanity checks
            // ------------------------------------------------------
            if (access1)
              throw new VectorizationException("parallel store to a single memory location")
            if (!aligned && !alignedBase && Knowledge.simd_avoidUnaligned)
              throw new VectorizationException("cannot vectorize store: array is not aligned, but unaligned accesses should be avoided")
            if (ctx.storesTmp != null)
              Logger.debug("[vect] Error? More than one store in a single statement?!")
            ctx.storesTmp = SIMD_Store(IR_AddressOf(expr), IR_VariableAccess(vecTmp, SIMD_RealDatatype), aligned)
          }
        }
        // ---- special handling of loop-carried cse variables ----
        base match {
          case _ : IR_IV_LoopCarriedCSBuffer if access1 && ctx.isLoad() && !ctx.isStore() && !ctx.toFinish_LCSE.contains(vecTmp) =>
            val init = SIMD_ConcShift(IR_VariableAccess(vecTmp, SIMD_RealDatatype), null, Platform.simd_vectorSize - 1)
            ctx.toFinish_LCSE(vecTmp) = init
            ctx.addStmt(IR_Assignment(IR_VariableAccess(vecTmp, SIMD_RealDatatype), init, "="))
          case _                                                                                                                 => // nothing to do
        }
        // --------------------------------------------------------
        IR_VariableAccess(vecTmp, SIMD_RealDatatype)

      case IR_VariableAccess(name, dType) =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp) {
          val decl = IR_VariableDeclaration(SIMD_RealDatatype, vecTmp)
          if (ctx.isLoad())
            decl.initialValue = Some(SIMD_Scalar2Vector(IR_VariableAccess(name, dType)))
          if (name == ctx.itName) {
            if (ctx.isStore()) throw new VectorizationException("iteration variable is modified inside the loop body...")
            if (!ctx.ignIncr)
              decl.initialValue = Some(SIMD_Addition(decl.initialValue.get, ctx.getIncrVector()))
            ctx.addStmt(decl)
          } else {
            ctx.addStmtPreLoop(decl, expr)
          }
        }
        IR_VariableAccess(vecTmp, SIMD_RealDatatype)

      case IR_StringLiteral("omp_get_thread_num()") =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp) {
          val decl = IR_VariableDeclaration(SIMD_RealDatatype, vecTmp, SIMD_Scalar2Vector(expr))
          ctx.addStmtPreLoop(decl, expr)
        }
        IR_VariableAccess(vecTmp, SIMD_RealDatatype)

      case IR_RealConstant(value) =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp)
          ctx.addStmtPreLoop(IR_VariableDeclaration(SIMD_RealDatatype, vecTmp, SIMD_Scalar2Vector(IR_RealConstant(value))), expr)
        IR_VariableAccess(vecTmp, SIMD_RealDatatype)

      case IR_IntegerConstant(value) => // TODO: ensure type safety
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp)
          ctx.addStmtPreLoop(IR_VariableDeclaration(SIMD_RealDatatype, vecTmp, SIMD_Scalar2Vector(IR_RealConstant(value))), expr)
        IR_VariableAccess(vecTmp, SIMD_RealDatatype)

      case IR_Negative(nExpr) =>
        SIMD_Negate(vectorizeExpr(nExpr, ctx))

      case IR_Addition(sums) =>
        if (sums.isEmpty)
          Logger.error("empty sum not allowed")
        val (muls, other) = sums.partition(_.isInstanceOf[IR_Multiplication])
        val mulsIt = muls.iterator
        val vecSumds = new Queue[IR_Expression]()
        vecSumds.enqueue(other.view.map { x => vectorizeExpr(x, ctx) } : _*)
        if (vecSumds.isEmpty)
          vecSumds += vectorizeExpr(mulsIt.next(), ctx)
        while (mulsIt.hasNext) {
          val simdMul = vectorizeExpr(mulsIt.next(), ctx).asInstanceOf[SIMD_Multiplication]
          vecSumds.enqueue(SIMD_MultiplyAdd(simdMul.left, simdMul.right, vecSumds.dequeue()))
        }
        while (vecSumds.length > 1)
          vecSumds.enqueue(SIMD_Addition(vecSumds.dequeue(), vecSumds.dequeue()))
        vecSumds.dequeue()

      case IR_Subtraction(left, right) =>
        SIMD_Subtraction(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))

      case IR_Multiplication(facs) =>
        if (facs.isEmpty)
          Logger.error("empty product not allowed")
        val exprs = new Queue[IR_Expression]()
        exprs.enqueue(facs.view.map { x => vectorizeExpr(x, ctx) } : _*)
        while (exprs.length > 1)
          exprs.enqueue(SIMD_Multiplication(exprs.dequeue(), exprs.dequeue()))
        exprs.dequeue()

      case IR_Division(left, right) =>
        SIMD_Division(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))

      case IR_Minimum(args) =>
        if (args.isEmpty)
          Logger.error("empty minimum not allowed")
        val exprs = new Queue[IR_Expression]()
        exprs.enqueue(args.view.map { x => vectorizeExpr(x, ctx) } : _*)
        while (exprs.length > 1)
          exprs.enqueue(SIMD_Minimum(exprs.dequeue(), exprs.dequeue()))
        exprs.dequeue()

      case IR_Maximum(args) =>
        if (args.isEmpty)
          Logger.error("empty minimum not allowed")
        val exprs = new Queue[IR_Expression]()
        exprs.enqueue(args.view.map { x => vectorizeExpr(x, ctx) } : _*)
        while (exprs.length > 1)
          exprs.enqueue(SIMD_Maximum(exprs.dequeue(), exprs.dequeue()))
        exprs.dequeue()

      // logical comparison
      case IR_EqEq(left, right) =>
        SIMD_EqEq(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))
      case IR_Neq(left, right) =>
        SIMD_Neq(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))
      case IR_Lower(left, right) =>
        SIMD_Lower(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))
      case IR_Greater(left, right) =>
        SIMD_Greater(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))
      case IR_LowerEqual(left, right) =>
        SIMD_LowerEqual(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))
      case IR_GreaterEqual(left, right) =>
        SIMD_GreaterEqual(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))

      // logical operations -> bitwise ops
      case IR_AndAnd(left, right) =>
        SIMD_BitwiseAnd(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))
      case IR_OrOr(left, right) =>
        SIMD_BitwiseOr(vectorizeExpr(left, ctx), vectorizeExpr(right, ctx))

      // simd blend
      case blendv @ SIMD_Blendv(a, b, _) =>
        blendv.a = vectorizeExpr(a, ctx)
        blendv.b = vectorizeExpr(b, ctx)
        blendv

      // TODO: datatypes of function accesses relevant?
      case IR_FunctionCall(function, args) if SIMD_MathFunctions.isAllowed(function.name) =>
        IR_FunctionCall(SIMD_MathFunctions.addUsage(function.name), args.map { arg => vectorizeExpr(arg, ctx) })

      case IR_Power(base, exp) if SIMD_MathFunctions.isAllowed("pow") =>
        IR_FunctionCall(SIMD_MathFunctions.addUsage("pow"), ListBuffer(vectorizeExpr(base, ctx), vectorizeExpr(exp, ctx)))

      case _ : IR_MemberAccess |
           _ : IR_IV_FragmentPositionBegin |
           _ : IR_IV_FragmentPositionEnd |
           _ : IR_IV_FragmentPosition =>
        val (vecTmp : String, njuTmp : Boolean) = ctx.getName(expr)
        if (njuTmp)
          ctx.addStmtPreLoop(IR_VariableDeclaration(SIMD_RealDatatype, vecTmp, SIMD_Scalar2Vector(expr)), expr)
        IR_VariableAccess(vecTmp, SIMD_RealDatatype)

      case _ =>
        throw new VectorizationException("cannot deal with " + expr.getClass + "; " + expr.prettyprint())
    }
  }

  private def createLoadExpression(oldExpr : IR_Expression, base : IR_Expression,
      index : HashMap[IR_Expression, Long], indexConst : Long,
      access1 : Boolean, aligned : Boolean, alignedBase : Boolean, ctx : LoopCtx) : IR_Expression = {

    if (access1)
      SIMD_Scalar2Vector(oldExpr)
    else if (aligned || !Knowledge.simd_avoidUnaligned)
      SIMD_Load(IR_AddressOf(oldExpr), aligned)
    else if (!alignedBase)
      throw new VectorizationException("cannot vectorize load: array is not aligned, but unaligned accesses should be avoided")
    else {
      // avoid unaligned load
      val vs : Long = Platform.simd_vectorSize
      val lowerConst : Long = indexConst - ((indexConst - ctx.getAlignedResidue()) % vs + vs) % vs
      index(IR_SimplifyExpression.constName) = lowerConst
      val lowerExpr = vectorizeExpr(IR_ArrayAccess(base, IR_SimplifyExpression.recreateExprFromIntSum(index), true), ctx).asInstanceOf[IR_VariableAccess]
      index(IR_SimplifyExpression.constName) = lowerConst + vs
      val upperExpr = vectorizeExpr(IR_ArrayAccess(base, IR_SimplifyExpression.recreateExprFromIntSum(index), true), ctx).asInstanceOf[IR_VariableAccess]
      SIMD_ConcShift(lowerExpr, upperExpr, (indexConst - lowerConst).toInt)
    }
  }
}
