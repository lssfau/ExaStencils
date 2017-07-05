package exastencils.baseExt.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.parallelization.ir._

// FIXME: refactor
object IR_LoopOverDimensions {
  def apply(numDimensions : Int, indices : IR_ExpressionIndexRange, body : IR_Statement, stepSize : IR_ExpressionIndex) =
    new IR_LoopOverDimensions(numDimensions, indices, ListBuffer[IR_Statement](body), stepSize)
  def apply(numDimensions : Int, indices : IR_ExpressionIndexRange, body : IR_Statement) =
    new IR_LoopOverDimensions(numDimensions, indices, ListBuffer[IR_Statement](body))

  def defIt(numDims : Int) = IR_ExpressionIndex((0 until numDims).map(dim => defItForDim(dim) : IR_Expression).toArray)
  def defItForDim(dim : Int) = IR_FieldIteratorAccess(dim)

  val threadIdxName : String = "threadIdx"

  // object ReplaceOffsetIndicesWithMin extends QuietDefaultStrategy("Replace OffsetIndex nodes with minimum values") {
  //   this += new Transformation("SearchAndReplace", {
  //     case OffsetIndex(xStartOffMin, _, xStart, _) => xStart + xStartOffMin
  //   })
  // }
  // object ReplaceOffsetIndicesWithMax extends QuietDefaultStrategy("Replace OffsetIndex nodes with maximum values") {
  //   this += new Transformation("SearchAndReplace", {
  //     case OffsetIndex(_, xEndOffMax, xEnd, _) => xEnd + xEndOffMax
  //   })
  // }

  // def evalMinIndex(index : Expression) : Long = {
  //   val wrappedIndex = ExpressionStatement(Duplicate(index))
  //   ReplaceOffsetIndicesWithMin.applyStandalone(wrappedIndex)
  //   return SimplifyExpression.evalIntegral(wrappedIndex.expression)
  // }

  def evalMinIndex(startIndex : IR_ExpressionIndex, numDimensions : Int, printWarnings : Boolean = false) : Array[Long] = {
    (0 until numDimensions).map(dim =>
      try {
        IR_SimplifyExpression.evalIntegralExtrema(startIndex(dim))._1
      } catch {
        case _ : EvaluationException =>
          if (printWarnings) Logger.warn(s"Start index for dimension $dim (${ startIndex(dim) }) could not be evaluated")
          0
      }).toArray
  }

  // def evalMaxIndex(index : Expression) : Long = {
  //   val wrappedIndex = ExpressionStatement(Duplicate(index))
  //   ReplaceOffsetIndicesWithMax.applyStandalone(wrappedIndex)
  //   return SimplifyExpression.evalIntegral(wrappedIndex.expression)
  // }

  def evalMaxIndex(endIndex : IR_ExpressionIndex, numDimensions : Int, printWarnings : Boolean = false) : Array[Long] = {
    (0 until numDimensions).map(dim =>
      try {
        IR_SimplifyExpression.evalIntegralExtrema(endIndex(dim))._2
      } catch {
        case _ : EvaluationException =>
          if (printWarnings) Logger.warn(s"End index for dimension $dim (${ endIndex(dim) }) could not be evaluated")
          0
      }).toArray
  }
}

case class IR_LoopOverDimensions(
    var numDimensions : Int,
    var indices : IR_ExpressionIndexRange,
    var body : ListBuffer[IR_Statement],
    var stepSize : IR_ExpressionIndex = null, // actual default set in constructor
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo(),
    var condition : Option[IR_Expression] = None,
    var genOMPThreadLoop : Boolean = false) extends IR_Statement with IR_SpecialExpandable with IR_HasParallelizationInfo {

  import IR_LoopOverDimensions._

  val parDims : Set[Int] = Set(0 until numDimensions : _*)
  var isVectorizable : Boolean = false
  // specifies that this loop can be vectorized even if the innermost dimension is not parallel (if it is, this flag can be ignored)
  val at1stIt : Array[(ListBuffer[IR_Statement], ListBuffer[(String, Any)])] = Array.fill(numDimensions)((new ListBuffer[IR_Statement](), new ListBuffer[(String, Any)]()))
  var lcCSEApplied : Boolean = false

  var tileSize = ListBuffer(Knowledge.poly_tileSize_x, Knowledge.poly_tileSize_y, Knowledge.poly_tileSize_z, Knowledge.poly_tileSize_w).take(numDimensions)

  if (stepSize == null)
    stepSize = IR_ExpressionIndex(Array.fill(numDimensions)(1))

  def maxIterationCount() : Array[Long] = {
    var start : Array[Long] = null
    var end : Array[Long] = null

    indices match {
      case indexRange : IR_ExpressionIndexRange =>
        indexRange.begin match {
          case startIndex : IR_ExpressionIndex => start = evalMinIndex(startIndex, numDimensions, false)
          case _                               => Logger.warn("Loop index range begin is not a MultiIndex")
        }
        indexRange.end match {
          case endIndex : IR_ExpressionIndex => end = evalMaxIndex(endIndex, numDimensions, false)
          case _                             => Logger.warn("Loop index range end is not a MultiIndex")
        }
      case _                                    => Logger.warn("Loop indices are not of type IndexRange")
    }

    if (null == start && null != end) {
      Logger.warn("Could determine loop index range end but not begin; assume begin is 0")
      end
    } else if (null != start && null != end)
      (0 until numDimensions).view.map(dim => end(dim) - start(dim)).toArray
    else
      null
  }

  def parallelizationIsReasonable : Boolean = {
    val maxItCount = maxIterationCount()
    if (maxItCount == null)
      return true // cannot determine iteration count, default is no change in parallelizability, i.e. true

    var totalNumPoints : Long = 1
    for (i <- maxItCount)
      totalNumPoints *= i
    totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads
  }

  def explParLoop = lcCSEApplied && parallelization.potentiallyParallel &&
    Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverDimensions &&
    parallelizationIsReasonable && parDims.isEmpty

  def createOMPThreadsWrapper(body : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = {
    if (explParLoop) {
      val begin = IR_VariableDeclaration(IR_IntegerDatatype, threadIdxName, IR_IntegerConstant(0))
      val end = IR_Lower(IR_VariableAccess(threadIdxName, IR_IntegerDatatype), IR_IntegerConstant(Knowledge.omp_numThreads))
      val inc = IR_ExpressionStatement(IR_PreIncrement(IR_VariableAccess(threadIdxName, IR_IntegerDatatype)))
      val loop = new IR_ForLoop(begin, end, inc, body, IR_ParallelizationInfo.PotentiallyParallel())
      ListBuffer(loop)
    } else {
      body
    }
  }

  def create1stItConds() : ListBuffer[IR_Statement] = {
    val inds = if (explParLoop) ompIndices else indices

    var conds : ListBuffer[IR_Statement] = new ListBuffer()
    // add conditions for first iteration
    for (d <- 0 until numDimensions)
      if (at1stIt(d)._1.nonEmpty) {
        val cond = IR_IfCondition(IR_EqEq(IR_FieldIteratorAccess(d), Duplicate(inds.begin(d))), at1stIt(d)._1)
        for ((annotId, value) <- at1stIt(d)._2)
          cond.annotate(annotId, value)
        conds += cond
      }

    conds
  }

  lazy val areOmpIndicesAffine : Boolean = {
    val outer = numDimensions - 1
    def oldBegin = Duplicate(indices.begin(outer))
    def oldEnd = Duplicate(indices.end(outer))
    def inc = Duplicate(stepSize(outer))
    IR_SimplifyExpression.simplifyIntegralExpr(oldEnd - oldBegin + inc).isInstanceOf[IR_IntegerConstant]
  }

  lazy val ompIndices : IR_ExpressionIndexRange = {
    val nju = Duplicate(indices)
    // update outermost loop according to: begin --> begin + (((end-start+inc-1)/inc * threadIdx) / nrThreads) * inc and end is start for threadIdx+1
    val outer = numDimensions - 1
    def oldBegin = Duplicate(indices.begin(outer))
    def oldEnd = Duplicate(indices.end(outer))
    def inc = Duplicate(stepSize(outer))
    def thrId = IR_VariableAccess(threadIdxName, IR_IntegerDatatype)
    val njuBegin = oldBegin + (((oldEnd - oldBegin + inc - 1) * thrId) / Knowledge.omp_numThreads) * inc
    val njuEnd = oldBegin + (((oldEnd - oldBegin + inc - 1) * (thrId + 1)) / Knowledge.omp_numThreads) * inc
    nju.begin(outer) = IR_SimplifyExpression.simplifyIntegralExpr(njuBegin)
    nju.end(outer) = IR_SimplifyExpression.simplifyIntegralExpr(njuEnd)
    nju
  }

  def expandSpecial() : ListBuffer[IR_Statement] = {
    def parallelizable(d : Int) = parallelization.potentiallyParallel && parDims.contains(d)
    def parallelize(d : Int) = parallelizable(d) && Knowledge.omp_parallelizeLoopOverDimensions && parallelizationIsReasonable

    // TODO: check interaction between at1stIt and condition (see also: TODO in polyhedron.Extractor.enterLoop)
    var wrappedBody : ListBuffer[IR_Statement] = body
    create1stItConds() ++=: wrappedBody // prepend to wrappedBody

    // add internal condition (e.g. RB)
    if (condition.isDefined)
      wrappedBody = ListBuffer[IR_Statement](IR_IfCondition(condition.get, wrappedBody))

    var anyPar : Boolean = false
    val outerPar = if (parDims.isEmpty) -1 else parDims.max
    val inds = if (explParLoop) ompIndices else indices
    // compile loop(s)
    for (d <- 0 until numDimensions) {
      def it = IR_FieldIteratorAccess(d)
      val decl = IR_VariableDeclaration(IR_FieldIteratorAccess(d), inds.begin(d))
      val cond = IR_Lower(it, inds.end(d))
      val incr = IR_Assignment(it, stepSize(d), "+=")
      val loop = new IR_ForLoop(decl, cond, incr, wrappedBody, Duplicate(parallelization))
      if (parallelize(d) && d == outerPar) {
        // FIXME: set depth for inner dimensions?
        // FIXME: set depth depending on parallelizability of inner dimensions?
        loop.parallelization.collapseDepth = numDimensions
      }

      // set optimization hints
      loop.parallelization.isInnermost = d == 0
      loop.parallelization.potentiallyParallel = parallelizable(d)
      loop.parallelization.isVectorizable = isVectorizable

      wrappedBody = ListBuffer[IR_Statement](loop)
    }

    wrappedBody = createOMPThreadsWrapper(wrappedBody)

    wrappedBody
  }
}

/// IR_ResolveLoopOverDimensions

object IR_ResolveLoopOverDimensions extends DefaultStrategy("Resolve LoopOverDimensions nodes") {
  this += new Transformation("Resolve", {
    case loop : IR_LoopOverDimensions => loop.expandSpecial()
  })
}
