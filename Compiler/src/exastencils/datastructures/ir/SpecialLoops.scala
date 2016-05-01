package exastencils.datastructures.ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.communication._
import exastencils.core._
import exastencils.core.collectors.StackCollector
import exastencils.data._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.mpi._
import exastencils.omp._
import exastencils.optimization._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.strategies._
import exastencils.util._

case class RegionSpecification(var region : String, var dir : Array[Int], var onlyOnBoundary : Boolean) {}
case class ContractionSpecification(var posExt : Array[Int], var negExt : Array[Int])

case class ContractingLoop(var number : Int, var iterator : Option[Expression], var statements : ListBuffer[Statement],
    var spec : ContractionSpecification) extends Statement { // FIXME: iterator is not used?!
  // TODO: validate spec
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ContractingLoop\n"

  // IMPORTANT: must match and extend all possible bounds for LoopOverDimensions inside a ContractingLoop
  private def extendBoundsBegin(expr : Expression, extent : Int) : Expression = {
    expr match {
      case e if Knowledge.experimental_useStefanOffsets => e // don't do anything here
      case IntegerConstant(i) =>
        IntegerConstant(i - extent)
      case oInd @ OffsetIndex(0, 1, _, ArrayAccess(_ : iv.IterationOffsetBegin, _, _)) =>
        oInd.maxOffset += extent
        oInd.index = SimplifyExpression.simplifyIntegralExpr(oInd.index - extent)
        oInd.offset = SimplifyExpression.simplifyIntegralExpr(oInd.offset * (extent + 1))
        oInd
    }
  }

  // IMPORTANT: must match and extend all possible bounds for LoopOverDimensions inside a ContractingLoop
  private def extendBoundsEnd(expr : Expression, extent : Int) : Expression = {
    expr match {
      case e if Knowledge.experimental_useStefanOffsets => e // don't do anything here
      case IntegerConstant(i) =>
        IntegerConstant(i + extent)
      case oInd @ OffsetIndex(-1, 0, _, ArrayAccess(_ : iv.IterationOffsetEnd, _, _)) =>
        oInd.minOffset -= extent
        oInd.index = SimplifyExpression.simplifyIntegralExpr(oInd.index + extent)
        oInd.offset = SimplifyExpression.simplifyIntegralExpr(oInd.offset * (extent + 1))
        oInd
    }
  }

  private type FieldKey = (String, Int)
  private def FieldKey(field : Field) : FieldKey = {
    return (field.identifier, field.level)
  }

  private def updateSlots(stmts : ListBuffer[Statement], fieldOffset : HashMap[FieldKey, Int]) : Unit = {
    object AdaptFieldSlots extends QuietDefaultStrategy("Adapt field slots") {
      this += new Transformation("now", {
        case fs @ FieldSelection(field, level, SlotAccess(slot, offset), _, _) =>
          fs.slot = new SlotAccess(slot, offset + fieldOffset.getOrElse(FieldKey(field), 0))
          fs
      })
    }
    AdaptFieldSlots.applyStandalone(stmts)
  }

  private def processLoopOverDimensions(l : LoopOverDimensions, extent : Int, fieldOffset : HashMap[FieldKey, Int]) : LoopOverDimensions = {
    val nju : LoopOverDimensions = Duplicate(l)
    for (dim <- 0 until nju.numDimensions) {
      nju.indices.begin(dim) = extendBoundsBegin(nju.indices.begin(dim), extent * spec.negExt(dim))
      nju.indices.end(dim) = extendBoundsEnd(nju.indices.end(dim), extent * spec.posExt(dim))
    }
    updateSlots(nju.body, fieldOffset)
    return nju
  }

  def expandSpecial : Output[NodeList] = {
    val res = new ListBuffer[Statement]()
    val fieldOffset = new HashMap[FieldKey, Int]()
    val fields = new HashMap[FieldKey, Field]()
    var condStmt : ConditionStatement = null
    for (i <- 1 to number)
      for (stmt <- statements)
        stmt match {
          case AdvanceSlotStatement(iv.CurrentSlot(field, fragment)) =>
            val fKey = FieldKey(field)
            fieldOffset(fKey) = fieldOffset.getOrElse(fKey, 0) + 1
            fields(fKey) = field

          case cStmt @ ConditionStatement(cond, ListBuffer(l : LoopOverDimensions), ListBuffer()) =>
            val nju = processLoopOverDimensions(l, (number - i), fieldOffset)
            if (condStmt == null || cond != condStmt.condition) {
              condStmt = Duplicate(cStmt)
              condStmt.trueBody.clear()
              res += condStmt
            }
            condStmt.trueBody += nju

          case l : LoopOverDimensions =>
            res += processLoopOverDimensions(l, (number - i), fieldOffset)
        }

    for ((fKey, offset) <- fieldOffset) {
      val field = fields(fKey)
      res += AssignmentStatement(iv.CurrentSlot(field), (iv.CurrentSlot(field) + offset) Mod field.numSlots)
    }

    return res
  }
}

case class LoopOverPoints(var field : Field,
    var region : Option[RegionSpecification],
    var seq : Boolean, // FIXME: seq HACK
    var startOffset : MultiIndex,
    var endOffset : MultiIndex,
    var increment : MultiIndex,
    var body : ListBuffer[Statement],
    var preComms : ListBuffer[CommunicateStatement] = ListBuffer(),
    var postComms : ListBuffer[CommunicateStatement] = ListBuffer(),
    var reduction : Option[Reduction] = None,
    var condition : Option[Expression] = None) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverPoints\n"

  def expandSpecial(collector : StackCollector) : Output[StatementList] = {
    val insideFragLoop = collector.stack.map({ case loop : LoopOverFragments => true; case _ => false }).reduce((left, right) => left || right)
    val innerLoop =
      if (Knowledge.experimental_splitLoopsForAsyncComm)
        LoopOverPointsInOneFragment(field.domain.index, field, region, seq, startOffset, endOffset, increment, body, preComms, postComms, reduction, condition)
      else
        LoopOverPointsInOneFragment(field.domain.index, field, region, seq, startOffset, endOffset, increment, body, ListBuffer(), ListBuffer(), reduction, condition)

    var stmts = ListBuffer[Statement]()
    stmts += innerLoop

    if (!insideFragLoop)
      if (seq)
        stmts = ListBuffer(new LoopOverFragments(stmts, reduction))
      else
        stmts = ListBuffer(new LoopOverFragments(stmts, reduction) with OMP_PotentiallyParallel)

    if (Knowledge.experimental_splitLoopsForAsyncComm)
      stmts
    else {
      if (!preComms.isEmpty) Logger.warn("Found precomm")
      preComms ++ stmts ++ postComms
    }
  }
}

case class LoopOverPointsInOneFragment(var domain : Int,
    var field : Field,
    var region : Option[RegionSpecification],
    var seq : Boolean, // FIXME: seq HACK
    var startOffset : MultiIndex,
    var endOffset : MultiIndex,
    var increment : MultiIndex,
    var body : ListBuffer[Statement],
    var preComms : ListBuffer[CommunicateStatement] = ListBuffer(),
    var postComms : ListBuffer[CommunicateStatement] = ListBuffer(),
    var reduction : Option[Reduction] = None,
    var condition : Option[Expression] = None) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverPointsInOneFragment\n"

  def numDims = field.fieldLayout.numDimsGrid

  def expandSpecial : Output[StatementList] = {
    var start = new MultiIndex(Array.fill(numDims)(0))
    var stop = new MultiIndex(Array.fill(numDims)(0))
    if (region.isDefined) {
      // case where a special region is to be traversed
      val regionCode = region.get.region.toUpperCase().charAt(0)

      start = new MultiIndex((0 until numDims).view.map({
        case dim if region.get.dir(dim) == 0 => field.fieldLayout.idxById(regionCode + "LB", dim) - field.referenceOffset(dim) + startOffset(dim)
        case dim if region.get.dir(dim) < 0  => field.fieldLayout.idxById(regionCode + "LB", dim) - field.referenceOffset(dim) + startOffset(dim)
        case dim if region.get.dir(dim) > 0  => field.fieldLayout.idxById(regionCode + "RB", dim) - field.referenceOffset(dim) + startOffset(dim)
      }).toArray[Expression])

      stop = new MultiIndex((0 until numDims).view.map({
        case dim if region.get.dir(dim) == 0 => field.fieldLayout.idxById(regionCode + "RE", dim) - field.referenceOffset(dim) - endOffset(dim)
        case dim if region.get.dir(dim) < 0  => field.fieldLayout.idxById(regionCode + "LE", dim) - field.referenceOffset(dim) - endOffset(dim)
        case dim if region.get.dir(dim) > 0  => field.fieldLayout.idxById(regionCode + "RE", dim) - field.referenceOffset(dim) - endOffset(dim)
      }).toArray[Expression])
    } else {
      // basic case -> just eliminate 'real' boundaries
      for (dim <- 0 until numDims) {
        field.fieldLayout.discretization match {
          case discr if "node" == discr
            || ("face_x" == discr && 0 == dim)
            || ("face_y" == discr && 1 == dim)
            || ("face_z" == discr && 2 == dim) =>
            if (Knowledge.experimental_useStefanOffsets) {
              start(dim) = field.fieldLayout.idxById("IB", dim) - field.referenceOffset(dim) + startOffset(dim)
              stop(dim) = field.fieldLayout.idxById("IE", dim) - field.referenceOffset(dim) - endOffset(dim)
            } else if (Knowledge.experimental_disableIterationOffsets) {
              start(dim) = field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
              stop(dim) = field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim)
            } else {
              // FIXME
              //              if (Knowledge.experimental_genVariableFieldSizes) {
              //                start(dim) = OffsetIndex(0, 1, ArrayAccess(iv.IndexFromField(field.identifier, field.level, "DLB"), dim) - field.referenceOffset(dim) + startOffset(dim), ArrayAccess(iv.IterationOffsetBegin(field.domain.index), dim))
              //                stop(dim) = OffsetIndex(-1, 0, ArrayAccess(iv.IndexFromField(field.identifier, field.level, "DRE"), dim) - field.referenceOffset(dim) - endOffset(dim), ArrayAccess(iv.IterationOffsetEnd(field.domain.index), dim))
              //              } else {
              val numDupLeft = field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft
              val numDupRight = field.fieldLayout.layoutsPerDim(dim).numDupLayersRight

              if (numDupLeft > 0)
                start(dim) = OffsetIndex(0, numDupLeft, field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim), numDupLeft * ArrayAccess(iv.IterationOffsetBegin(field.domain.index), dim))
              else
                start(dim) = field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
              if (numDupRight > 0)
                stop(dim) = OffsetIndex(-numDupRight, 0, field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim), numDupRight * ArrayAccess(iv.IterationOffsetEnd(field.domain.index), dim))
              else
                stop(dim) = field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim)
              //              }
            }
          case discr if "cell" == discr
            || ("face_x" == discr && 0 != dim)
            || ("face_y" == discr && 1 != dim)
            || ("face_z" == discr && 2 != dim) =>
            start(dim) = field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
            stop(dim) = field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim)
        }
      }
    }

    val indexRange = IndexRange(start, stop)
    SimplifyStrategy.doUntilDoneStandalone(indexRange)

    // fix iteration space for reduction operations if required
    if (Knowledge.experimental_trimBoundsForReductionLoops && reduction.isDefined && !region.isDefined) {
      if (!condition.isDefined) condition = Some(BooleanConstant(true))
      for (dim <- 0 until numDims)
        if (field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft > 0)
          /*if ("node" == field.fieldLayout.discretization
          || ("face_x" == field.fieldLayout.discretization && 0 == dim)
          || ("face_y" == field.fieldLayout.discretization && 1 == dim)
          || ("face_z" == field.fieldLayout.discretization && 2 == dim))*/
          condition = Some(AndAndExpression(condition.get, GreaterEqualExpression(VariableAccess(dimToString(dim), Some(IntegerDatatype)), field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft)))
    }

    var loop : LoopOverDimensions = (
      if (seq)
        new LoopOverDimensions(numDims, indexRange, body, increment, reduction, condition)
      else {
        val ret = new LoopOverDimensions(numDims, indexRange, body, increment, reduction, condition) with OMP_PotentiallyParallel with PolyhedronAccessible
        ret.optLevel = (
          if (Knowledge.maxLevel - field.level < Knowledge.poly_numFinestLevels)
            Knowledge.poly_optLevel_fine
          else
            Knowledge.poly_optLevel_coarse)
        ret
      })

    var stmts = ListBuffer[Statement]()

    if (Knowledge.experimental_splitLoopsForAsyncComm && !preComms.isEmpty && !postComms.isEmpty) {
      Logger.warn("Found unsupported case of a loop with pre and post communication statements - post communication statements will be ignored")
    }

    if (Knowledge.experimental_splitLoopsForAsyncComm && !preComms.isEmpty) {
      if (region.isDefined) Logger.warn("Found region loop with communication step")

      // gather occuring field access offsets - will determine bounds of inner and outer loop
      GatherFieldAccessOffsets.accesses.clear
      GatherFieldAccessOffsets.applyStandalone(Scope(body))

      // check dimensionality of involved fields
      val numDims = field.fieldLayout.numDimsGrid
      for (cs <- preComms)
        if (numDims != cs.field.fieldLayout.numDimsGrid)
          Logger.warn(s"Found communicate statement on field ${cs.field.codeName} which is incompatible with dimensionality $numDims")

      // loop iterator
      val loopIt = LoopOverDimensions.defIt(numDims)

      // init default bounds ...
      var lowerBounds : IndexedSeq[Expression] = (0 until numDims).map(dim => {
        val minWidth = (if (0 == dim) Knowledge.experimental_splitLoops_minInnerWidth else 0)
        start(dim) + minWidth
      })
      var upperBounds : IndexedSeq[Expression] = (0 until numDims).map(dim => {
        val minWidth = (if (0 == dim) Knowledge.experimental_splitLoops_minInnerWidth else 0)
        stop(dim) - minWidth
      })

      // ... and compile actual loop condition bounds
      // Concept:
      //  determine maximum offset to the left for potentially required reads
      //  shift loop start by this (negated) offset plus 1
      //  optionally enforce a certain number of elements for vectorization/ optimized cache line usage
      for (cs <- preComms) {
        val newLowerBounds = (0 until numDims).map(dim => {
          var ret : Expression = new MinimumExpression(GatherFieldAccessOffsets.accesses.getOrElse(cs.field.codeName, ListBuffer()).map(_(dim)))
          ret = 1 - ret
          SimplifyStrategy.doUntilDoneStandalone(ExpressionStatement(ret))
          start(dim) + ret
        })

        lowerBounds = (lowerBounds, newLowerBounds).zipped.map(new MaximumExpression(_, _))
      }

      for (cs <- preComms) {
        val newUpperBounds = (0 until numDims).map(dim => {
          var ret : Expression = new MaximumExpression(GatherFieldAccessOffsets.accesses.getOrElse(cs.field.codeName, ListBuffer()).map(_(dim)))
          ret = 1 + ret
          SimplifyStrategy.doUntilDoneStandalone(ExpressionStatement(ret))
          stop(dim) - ret
        })

        upperBounds = (upperBounds, newUpperBounds).zipped.map(new MinimumExpression(_, _))
      }

      // start communication
      stmts ++= preComms.filter(comm => "begin" == comm.op)
      stmts ++= preComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "begin"; newComm })

      // innerRegion
      var coreLoop = Duplicate(loop)
      coreLoop.condition = Some(AndAndExpression(coreLoop.condition.getOrElse(BooleanConstant(true)),
        (0 until field.fieldLayout.numDimsGrid).map(dim =>
          AndAndExpression(GreaterEqualExpression(loopIt(dim), lowerBounds(dim)), LowerExpression(loopIt(dim), upperBounds(dim))) : Expression).reduce(AndAndExpression(_, _))))

      stmts += coreLoop

      // finish communication
      stmts ++= preComms.filter(comm => "finish" == comm.op)
      stmts ++= preComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "finish"; newComm })

      // outerRegion
      var boundaryLoop = Duplicate(loop)
      boundaryLoop.condition = Some(OrOrExpression(boundaryLoop.condition.getOrElse(BooleanConstant(false)),
        (0 until field.fieldLayout.numDimsGrid).map(dim =>
          OrOrExpression(LowerExpression(loopIt(dim), lowerBounds(dim)), GreaterEqualExpression(loopIt(dim), upperBounds(dim))) : Expression).reduce(OrOrExpression(_, _))))
      stmts += boundaryLoop
    } else if (Knowledge.experimental_splitLoopsForAsyncComm && !postComms.isEmpty) {
      if (region.isDefined) Logger.warn("Found region loop with communication step")

      // check dimensionality of involved fields
      val numDims = field.fieldLayout.numDimsGrid
      for (cs <- postComms)
        if (numDims != cs.field.fieldLayout.numDimsGrid)
          Logger.warn(s"Found communicate statement on field ${cs.field.codeName} which is incompatible with dimensionality $numDims")

      // loop iterator
      val loopIt = LoopOverDimensions.defIt(numDims)

      // init default bounds ...
      var lowerBounds : IndexedSeq[Expression] = (0 until numDims).map(dim => {
        val minWidth = (if (0 == dim) Knowledge.experimental_splitLoops_minInnerWidth else 0)
        start(dim) + minWidth
      })
      var upperBounds : IndexedSeq[Expression] = (0 until numDims).map(dim => {
        val minWidth = (if (0 == dim) Knowledge.experimental_splitLoops_minInnerWidth else 0)
        stop(dim) - minWidth
      })

      // ... and compile actual loop condition bounds
      // Concept:
      //  enforce that elements to be synchronized are in outer region
      //  optionally enforce a certain number of elements for vectorization/ optimized cache line usage
      for (cs <- postComms) {
        val newLowerBounds = (0 until numDims).map(dim => {
          val ret : Expression = cs.field.fieldLayout.layoutsPerDim(dim).numGhostLayersRight + cs.field.fieldLayout.layoutsPerDim(dim).numDupLayersRight
          cs.field.fieldLayout.idxById("DLB", dim) - cs.field.referenceOffset(dim) + ret
        })

        lowerBounds = (lowerBounds, newLowerBounds).zipped.map(new MaximumExpression(_, _))
      }

      for (cs <- postComms) {
        val newUpperBounds = (0 until numDims).map(dim => {
          val ret : Expression = cs.field.fieldLayout.layoutsPerDim(dim).numGhostLayersLeft + cs.field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft
          cs.field.fieldLayout.idxById("DRE", dim) - cs.field.referenceOffset(dim) - ret
        })

        upperBounds = (upperBounds, newUpperBounds).zipped.map(new MinimumExpression(_, _))
      }

      // outerRegion
      var boundaryLoop = Duplicate(loop)
      boundaryLoop.condition = Some(OrOrExpression(boundaryLoop.condition.getOrElse(BooleanConstant(false)),
        (0 until field.fieldLayout.numDimsGrid).map(dim =>
          OrOrExpression(LowerExpression(loopIt(dim), lowerBounds(dim)), GreaterEqualExpression(loopIt(dim), upperBounds(dim))) : Expression).reduce(OrOrExpression(_, _))))
      stmts += boundaryLoop

      // start communication
      stmts ++= postComms.filter(comm => "begin" == comm.op)
      stmts ++= postComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "begin"; newComm })

      // innerRegion
      var coreLoop = Duplicate(loop)
      coreLoop.condition = Some(AndAndExpression(coreLoop.condition.getOrElse(BooleanConstant(true)),
        (0 until field.fieldLayout.numDimsGrid).map(dim =>
          AndAndExpression(GreaterEqualExpression(loopIt(dim), lowerBounds(dim)), LowerExpression(loopIt(dim), upperBounds(dim))) : Expression).reduce(AndAndExpression(_, _))))

      stmts += coreLoop

      // finish communication
      stmts ++= postComms.filter(comm => "finish" == comm.op)
      stmts ++= postComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "finish"; newComm })
    } else {
      stmts += loop
    }

    if (region.isDefined) {
      if (region.get.onlyOnBoundary) {
        val neighIndex = Fragment.getNeigh(region.get.dir).index
        stmts = ListBuffer[Statement](new ConditionStatement(NegationExpression(iv.NeighborIsValid(domain, neighIndex)), stmts))
      }
    }

    if (domain >= 0) {
      stmts = ListBuffer[Statement](new ConditionStatement(iv.IsValidForSubdomain(domain), stmts))
    }

    stmts
  }
}

object LoopOverDimensions {
  def defIt(numDims : Int) = {
    new MultiIndex((0 until numDims).map(dim => defItForDim(dim) : Expression).toArray)
  }
  def defItForDim(dim : Int) = {
    new VariableAccess(dimToString(dim), Some(IntegerDatatype))
  }

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

  def evalMinIndex(startIndex : MultiIndex, numDimensions : Int, printWarnings : Boolean = false) : Array[Long] = {
    (0 until numDimensions).map(dim =>
      try {
        SimplifyExpression.evalIntegralExtrema(startIndex(dim))._1
      } catch {
        case _ : EvaluationException =>
          if (printWarnings) Logger.warn(s"Start index for dimension $dim (${startIndex(dim)}) could not be evaluated")
          0
      }).toArray
  }

  // def evalMaxIndex(index : Expression) : Long = {
  //   val wrappedIndex = ExpressionStatement(Duplicate(index))
  //   ReplaceOffsetIndicesWithMax.applyStandalone(wrappedIndex)
  //   return SimplifyExpression.evalIntegral(wrappedIndex.expression)
  // }

  def evalMaxIndex(endIndex : MultiIndex, numDimensions : Int, printWarnings : Boolean = false) : Array[Long] = {
    (0 until numDimensions).map(dim =>
      try {
        SimplifyExpression.evalIntegralExtrema(endIndex(dim))._2
      } catch {
        case _ : EvaluationException =>
          if (printWarnings) Logger.warn(s"End index for dimension $dim (${endIndex(dim)}) could not be evaluated")
          0
      }).toArray
  }
}

case class LoopOverDimensions(var numDimensions : Int,
    var indices : IndexRange,
    var body : ListBuffer[Statement],
    var stepSize : MultiIndex = null, // to be overwritten afterwards
    var reduction : Option[Reduction] = None,
    var condition : Option[Expression] = None) extends Statement {
  def this(numDimensions : Int, indices : IndexRange, body : Statement, stepSize : MultiIndex, reduction : Option[Reduction], condition : Option[Expression]) = this(numDimensions, indices, ListBuffer[Statement](body), stepSize, reduction, condition)
  def this(numDimensions : Int, indices : IndexRange, body : Statement, stepSize : MultiIndex, reduction : Option[Reduction]) = this(numDimensions, indices, ListBuffer[Statement](body), stepSize, reduction)
  def this(numDimensions : Int, indices : IndexRange, body : Statement, stepSize : MultiIndex) = this(numDimensions, indices, ListBuffer[Statement](body), stepSize)
  def this(numDimensions : Int, indices : IndexRange, body : Statement) = this(numDimensions, indices, ListBuffer[Statement](body))

  import LoopOverDimensions._

  if (stepSize == null) stepSize = new MultiIndex(Array.fill(numDimensions)(1))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverDimensions\n"

  def maxIterationCount() : Array[Long] = {
    var start : Array[Long] = null
    var end : Array[Long] = null

    indices match {
      case indexRange : IndexRange =>
        indexRange.begin match {
          case startIndex : MultiIndex => start = evalMinIndex(startIndex, numDimensions, false)
          case _                       => Logger.warn("Loop index range begin is not a MultiIndex")
        }
        indexRange.end match {
          case endIndex : MultiIndex => end = evalMaxIndex(endIndex, numDimensions, false)
          case _                     => Logger.warn("Loop index range end is not a MultiIndex")
        }
      case _ => Logger.warn("Loop indices are not of type IndexRange")
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
    return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
  }

  def expandSpecial : ListBuffer[Statement] = {
    val parallelizable = Knowledge.omp_parallelizeLoopOverDimensions && (this match { case _ : OMP_PotentiallyParallel => true; case _ => false })
    val parallelize = parallelizable && parallelizationIsReasonable
    val resolveOmpReduction = (
      parallelize
      && Knowledge.omp_enabled
      && Platform.omp_version < 3.1
      && reduction.isDefined
      && ("min" == reduction.get.op || "max" == reduction.get.op))

    // add internal condition (e.g. RB)
    var wrappedBody : ListBuffer[Statement] = (
      if (condition.isDefined)
        ListBuffer[Statement](new ConditionStatement(condition.get, body))
      else
        body)

    // compile loop(s)
    var compiledLoop : ForLoopStatement with OptimizationHint = null
    for (d <- 0 until numDimensions) {
      def it = VariableAccess(dimToString(d), Some(IntegerDatatype))
      val decl = VariableDeclarationStatement(IntegerDatatype, dimToString(d), Some(indices.begin(d)))
      val cond = LowerExpression(it, indices.end(d))
      val incr = AssignmentStatement(it, stepSize(d), "+=")
      if (parallelize && d == numDimensions - 1) {
        val omp = new ForLoopStatement(decl, cond, incr, wrappedBody, reduction) with OptimizationHint with OMP_PotentiallyParallel
        omp.collapse = numDimensions
        compiledLoop = omp
      } else {
        compiledLoop = new ForLoopStatement(decl, cond, incr, wrappedBody, reduction) with OptimizationHint
        wrappedBody = ListBuffer[Statement](compiledLoop)
      }
      // set optimization hints
      compiledLoop.isInnermost = d == 0
      compiledLoop.isParallel = parallelizable
    }

    var retStmts = ListBuffer[Statement]()

    // resolve omp reduction if necessary
    if (!resolveOmpReduction) {
      retStmts += compiledLoop
    } else {
      // resolve max reductions
      val redOp = reduction.get.op
      val redExpName = reduction.get.target.name
      def redExp = VariableAccess(redExpName, None)
      val redExpLocalName = redExpName + "_red"
      def redExpLocal = VariableAccess(redExpLocalName, None)

      // FIXME: this assumes real data types -> data type should be determined according to redExp
      val decl = VariableDeclarationStatement(ArrayDatatype(RealDatatype, Knowledge.omp_numThreads), redExpLocalName, None)
      val init = (0 until Knowledge.omp_numThreads).map(fragIdx => AssignmentStatement(ArrayAccess(redExpLocal, fragIdx), redExp))
      val redOperands = ListBuffer[Expression](redExp) ++ (0 until Knowledge.omp_numThreads).map(fragIdx => ArrayAccess(redExpLocal, fragIdx) : Expression)
      val red = AssignmentStatement(redExp, if ("min" == redOp) MinimumExpression(redOperands) else MaximumExpression(redOperands))

      ReplaceStringConstantsStrategy.toReplace = redExp.prettyprint
      ReplaceStringConstantsStrategy.replacement = ArrayAccess(redExpLocal, VariableAccess("omp_tid", Some(IntegerDatatype)))
      ReplaceStringConstantsStrategy.applyStandalone(body)
      body.prepend(VariableDeclarationStatement(IntegerDatatype, "omp_tid", Some("omp_get_thread_num()")))

      retStmts += Scope(ListBuffer[Statement](decl)
        ++ init
        ++ ListBuffer[Statement](compiledLoop, red))
    }

    retStmts
  }
}

object LoopOverFragments { def defIt = "fragmentIdx" }

case class LoopOverFragments(var body : ListBuffer[Statement], var reduction : Option[Reduction] = None) extends Statement with Expandable {
  import LoopOverFragments._

  def this(body : Statement, reduction : Option[Reduction]) = this(ListBuffer(body), reduction)
  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverFragments\n"

  def generateBasicLoop(parallelize : Boolean) = {
    val loop = if (parallelize)
      new ForLoopStatement(
        VariableDeclarationStatement(IntegerDatatype, defIt, Some(0)),
        LowerExpression(defIt, Knowledge.domain_numFragmentsPerBlock),
        PreIncrementExpression(defIt),
        body,
        reduction) with OMP_PotentiallyParallel
    else
      new ForLoopStatement(
        VariableDeclarationStatement(IntegerDatatype, defIt, Some(0)),
        LowerExpression(defIt, Knowledge.domain_numFragmentsPerBlock),
        PreIncrementExpression(defIt),
        body,
        reduction)
    loop.annotate("numLoopIterations", Knowledge.domain_numFragmentsPerBlock)
    loop
  }

  override def expand() : Output[StatementList] = {
    var statements = new ListBuffer[Statement]

    if (Knowledge.experimental_resolveUnreqFragmentLoops && Knowledge.domain_numFragmentsPerBlock <= 1) {
      // eliminate fragment loops in case of only one fragment per block
      statements = ListBuffer(Scope(body))

      // replace references to old loop iterator
      ReplaceStringConstantsStrategy.toReplace = defIt
      ReplaceStringConstantsStrategy.replacement = IntegerConstant(0)
      ReplaceStringConstantsStrategy.applyStandalone(statements)
    } else {
      val parallelize = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && this.isInstanceOf[OMP_PotentiallyParallel]
      val resolveOmpReduction = (
        parallelize
        && Platform.omp_version < 3.1
        && reduction.isDefined
        && ("min" == reduction.get.op || "max" == reduction.get.op))

      // basic loop

      if (!resolveOmpReduction) {
        statements += generateBasicLoop(parallelize)
      } else {
        // resolve max reductions
        val redOp = reduction.get.op
        val redExpName = reduction.get.target.name
        def redExp = VariableAccess(redExpName, None)
        val redExpLocalName = redExpName + "_red"
        def redExpLocal = VariableAccess(redExpLocalName, None)

        // FIXME: this assumes real data types -> data type should be determined according to redExp
        val decl = VariableDeclarationStatement(ArrayDatatype(RealDatatype, Knowledge.omp_numThreads), redExpLocalName, None)
        val init = (0 until Knowledge.omp_numThreads).map(fragIdx => AssignmentStatement(ArrayAccess(redExpLocal, fragIdx), redExp))
        val redOperands = ListBuffer[Expression](redExp) ++ (0 until Knowledge.omp_numThreads).map(fragIdx => ArrayAccess(redExpLocal, fragIdx) : Expression)
        val red = AssignmentStatement(redExp, if ("min" == redOp) MinimumExpression(redOperands) else MaximumExpression(redOperands))

        ReplaceStringConstantsStrategy.toReplace = redExp.prettyprint
        ReplaceStringConstantsStrategy.replacement = ArrayAccess(redExpLocal, VariableAccess("omp_tid", Some(IntegerDatatype)))
        ReplaceStringConstantsStrategy.applyStandalone(body)
        body.prepend(VariableDeclarationStatement(IntegerDatatype, "omp_tid", Some("omp_get_thread_num()")))

        statements += Scope(ListBuffer[Statement](decl)
          ++ init
          ++ ListBuffer[Statement](generateBasicLoop(parallelize), red))
      }
    }

    if (Knowledge.mpi_enabled && reduction.isDefined) {
      statements += new MPI_Allreduce(AddressofExpression(reduction.get.target), RealDatatype, 1, reduction.get.op) // FIXME: get dt and cnt from reduction
    }

    statements
  }
}

object LoopOverDomains { def defIt = "domainIdx" }

case class LoopOverDomains(var body : ListBuffer[Statement]) extends Statement with Expandable {
  import LoopOverDomains._

  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverDomains\n"

  override def expand() : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(IntegerDatatype, defIt, Some(0)),
      LowerExpression(defIt, DomainCollection.domains.size),
      PreIncrementExpression(defIt),
      body)
  }
}

object LoopOverFields { def defIt = "fieldIdx" }

case class LoopOverFields(var body : ListBuffer[Statement]) extends Statement with Expandable {
  import LoopOverFields._

  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverFields\n"

  override def expand() : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(IntegerDatatype, defIt, Some(0)),
      LowerExpression(defIt, FieldCollection.fields.size),
      PreIncrementExpression(defIt),
      body)
  }
}

object LoopOverLevels { def defIt = "levelIdx" }

case class LoopOverLevels(var body : ListBuffer[Statement]) extends Statement with Expandable {
  import LoopOverLevels._

  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverLevels\n"

  override def expand() : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(IntegerDatatype, defIt, Some(Knowledge.minLevel)),
      LowerExpression(defIt, Knowledge.maxLevel + 1),
      PreIncrementExpression(defIt),
      body)
  }
}

object LoopOverNeighbors { def defIt = "neighborIdx" }

case class LoopOverNeighbors(var body : ListBuffer[Statement]) extends Statement with Expandable {
  import LoopOverNeighbors._

  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverNeighbors\n"

  override def expand() : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(IntegerDatatype, defIt, Some(0)),
      LowerExpression(defIt, Fragment.neighbors.size),
      PreIncrementExpression(defIt),
      body)
  }
}
