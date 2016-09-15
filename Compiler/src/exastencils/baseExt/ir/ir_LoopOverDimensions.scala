package exastencils.baseExt.ir

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger.Logger
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.optimization.OptimizationHint
import exastencils.prettyprinting.PpStream
import exastencils.strategies.ReplaceStringConstantsStrategy
import exastencils.util._

// FIXME: refactor
object IR_LoopOverDimensions {
  def apply(numDimensions : Int, indices : IndexRange, body : IR_Statement, stepSize : IR_ExpressionIndex, reduction : Option[Reduction], condition : Option[IR_Expression]) =
    new IR_LoopOverDimensions(numDimensions, indices, ListBuffer[IR_Statement](body), stepSize, reduction, condition)
  def apply(numDimensions : Int, indices : IndexRange, body : IR_Statement, stepSize : IR_ExpressionIndex, reduction : Option[Reduction]) =
    new IR_LoopOverDimensions(numDimensions, indices, ListBuffer[IR_Statement](body), stepSize, reduction)
  def apply(numDimensions : Int, indices : IndexRange, body : IR_Statement, stepSize : IR_ExpressionIndex) =
    new IR_LoopOverDimensions(numDimensions, indices, ListBuffer[IR_Statement](body), stepSize)
  def apply(numDimensions : Int, indices : IndexRange, body : IR_Statement) =
    new IR_LoopOverDimensions(numDimensions, indices, ListBuffer[IR_Statement](body))

  def defIt(numDims : Int) = IR_ExpressionIndex((0 until numDims).map(dim => defItForDim(dim) : IR_Expression).toArray)
  def defItForDim(dim : Int) = IR_VariableAccess(dimToString(dim), Some(IR_IntegerDatatype))

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
        SimplifyExpression.evalIntegralExtrema(startIndex(dim))._1
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
        SimplifyExpression.evalIntegralExtrema(endIndex(dim))._2
      } catch {
        case _ : EvaluationException =>
          if (printWarnings) Logger.warn(s"End index for dimension $dim (${ endIndex(dim) }) could not be evaluated")
          0
      }).toArray
  }
}

case class IR_LoopOverDimensions(var numDimensions : Int,
    var indices : IndexRange,
    var body : ListBuffer[IR_Statement],
    var stepSize : IR_ExpressionIndex = null, // actual default set in constructor
    var reduction : Option[Reduction] = None,
    var condition : Option[IR_Expression] = None,
    var genOMPThreadLoop : Boolean = false) extends IR_Statement {

  import IR_LoopOverDimensions._

  val parDims : Set[Int] = Set(0 until numDimensions : _*)
  var isVectorizable : Boolean = false
  // specifies that this loop can be vectorized even if the innermost dimension is not parallel (if it is, this flag can be ignored)
  val at1stIt : Array[(ListBuffer[IR_Statement], ListBuffer[(String, Any)])] = Array.fill(numDimensions)((new ListBuffer[IR_Statement](), new ListBuffer[(String, Any)]()))
  var lcCSEApplied : Boolean = false

  if (stepSize == null)
    stepSize = IR_ExpressionIndex(Array.fill(numDimensions)(1))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverDimensions\n"

  def maxIterationCount() : Array[Long] = {
    var start : Array[Long] = null
    var end : Array[Long] = null

    indices match {
      case indexRange : IndexRange =>
        indexRange.begin match {
          case startIndex : IR_ExpressionIndex => start = evalMinIndex(startIndex, numDimensions, false)
          case _                               => Logger.warn("Loop index range begin is not a MultiIndex")
        }
        indexRange.end match {
          case endIndex : IR_ExpressionIndex => end = evalMaxIndex(endIndex, numDimensions, false)
          case _                             => Logger.warn("Loop index range end is not a MultiIndex")
        }
      case _                       => Logger.warn("Loop indices are not of type IndexRange")
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
    (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
  }

  def explParLoop = lcCSEApplied && this.isInstanceOf[OMP_PotentiallyParallel] &&
    Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverDimensions &&
    parallelizationIsReasonable && parDims.isEmpty

  def createOMPThreadsWrapper(body : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = {
    if (explParLoop) {
      val begin = new VariableDeclarationStatement(IR_IntegerDatatype, threadIdxName, IR_IntegerConstant(0))
      val end = IR_LowerExpression(IR_VariableAccess(threadIdxName, IR_IntegerDatatype), IR_IntegerConstant(Knowledge.omp_numThreads))
      val inc = IR_ExpressionStatement(IR_PreIncrementExpression(IR_VariableAccess(threadIdxName, IR_IntegerDatatype)))
      ListBuffer(new IR_ForLoop(begin, end, inc, body) with OMP_PotentiallyParallel)
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
        val cond = IR_IfCondition(IR_EqEqExpression(IR_VariableAccess(dimToString(d), IR_IntegerDatatype), Duplicate(inds.begin(d))), at1stIt(d)._1)
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
    SimplifyExpression.simplifyIntegralExpr(oldEnd - oldBegin + inc).isInstanceOf[IR_IntegerConstant]
  }

  lazy val ompIndices : IndexRange = {
    val nju = Duplicate(indices)
    // update outermost loop according to: begin --> begin + (((end-start+inc-1)/inc * threadIdx) / nrThreads) * inc and end is start for threadIdx+1
    val outer = numDimensions - 1
    def oldBegin = Duplicate(indices.begin(outer))
    def oldEnd = Duplicate(indices.end(outer))
    def inc = Duplicate(stepSize(outer))
    def thrId = IR_VariableAccess(threadIdxName, IR_IntegerDatatype)
    val njuBegin = oldBegin + (((oldEnd - oldBegin + inc - 1) * thrId) / Knowledge.omp_numThreads) * inc
    val njuEnd = oldBegin + (((oldEnd - oldBegin + inc - 1) * (thrId + 1)) / Knowledge.omp_numThreads) * inc
    nju.begin(outer) = SimplifyExpression.simplifyIntegralExpr(njuBegin)
    nju.end(outer) = SimplifyExpression.simplifyIntegralExpr(njuEnd)
    nju
  }

  def expandSpecial : ListBuffer[IR_Statement] = {
    def parallelizable(d : Int) = this.isInstanceOf[OMP_PotentiallyParallel] && parDims.contains(d)
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
      def it = IR_VariableAccess(dimToString(d), Some(IR_IntegerDatatype))
      val decl = VariableDeclarationStatement(IR_IntegerDatatype, dimToString(d), Some(inds.begin(d)))
      val cond = IR_LowerExpression(it, inds.end(d))
      val incr = IR_Assignment(it, stepSize(d), "+=")
      val compiledLoop : IR_ForLoop with OptimizationHint =
        if (parallelize(d) && d == outerPar) {
          anyPar = true
          val omp = new IR_ForLoop(decl, cond, incr, wrappedBody, reduction) with OptimizationHint with OMP_PotentiallyParallel
          omp.collapse = numDimensions
          omp
        } else
          new IR_ForLoop(decl, cond, incr, wrappedBody, reduction) with OptimizationHint
      wrappedBody = ListBuffer[IR_Statement](compiledLoop)
      // set optimization hints
      compiledLoop.isInnermost = d == 0
      compiledLoop.isParallel = parallelizable(d)
      compiledLoop.isVectorizable = isVectorizable
    }

    wrappedBody = createOMPThreadsWrapper(wrappedBody)

    var retStmts : ListBuffer[IR_Statement] = null

    // resolve omp reduction if necessary
    val resolveOmpReduction = (
      anyPar
        && Knowledge.omp_enabled
        && Platform.omp_version < 3.1
        && reduction.isDefined
        && ("min" == reduction.get.op || "max" == reduction.get.op))
    if (!resolveOmpReduction) {
      retStmts = wrappedBody
    } else {
      // resolve max reductions
      val redOp = reduction.get.op
      val redExpName = reduction.get.target.name
      def redExp = IR_VariableAccess(redExpName, None)
      val redExpLocalName = redExpName + "_red"
      def redExpLocal = IR_VariableAccess(redExpLocalName, None)

      // FIXME: this assumes real data types -> data type should be determined according to redExp
      val decl = VariableDeclarationStatement(IR_ArrayDatatype(IR_RealDatatype, Knowledge.omp_numThreads), redExpLocalName, None)
      val init = (0 until Knowledge.omp_numThreads).map(fragIdx => IR_Assignment(ArrayAccess(redExpLocal, fragIdx), redExp))
      val redOperands = ListBuffer[IR_Expression](redExp) ++= (0 until Knowledge.omp_numThreads).map(fragIdx => ArrayAccess(redExpLocal, fragIdx) : IR_Expression)
      val red = IR_Assignment(redExp, if ("min" == redOp) IR_MinimumExpression(redOperands) else IR_MaximumExpression(redOperands))

      ReplaceStringConstantsStrategy.toReplace = redExp.prettyprint
      ReplaceStringConstantsStrategy.replacement = ArrayAccess(redExpLocal, IR_VariableAccess("omp_tid", Some(IR_IntegerDatatype)))
      ReplaceStringConstantsStrategy.applyStandalone(body)
      body.prepend(VariableDeclarationStatement(IR_IntegerDatatype, "omp_tid", Some("omp_get_thread_num()")))

      retStmts = ListBuffer(IR_Scope(decl +=: init ++=: wrappedBody += red))
    }

    retStmts
  }
}
