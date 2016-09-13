package exastencils.datastructures.ir

import scala.collection.mutable.{ HashMap, ListBuffer, Set }

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.communication._
import exastencils.core._
import exastencils.core.collectors.StackCollector
import exastencils.data._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
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

case class RegionSpecification(var region : String, var dir : IR_ConstIndex, var onlyOnBoundary : Boolean) {}

case class ContractionSpecification(var posExt : IR_ConstIndex, var negExt : IR_ConstIndex)

case class ContractingLoop(var number : Int, var iterator : Option[IR_Expression], var statements : ListBuffer[IR_Statement],
    var spec : ContractionSpecification) extends IR_Statement {
  // FIXME: iterator is not used?!
  // TODO: validate spec
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ContractingLoop\n"

  // IMPORTANT: must match and extend all possible bounds for LoopOverDimensions inside a ContractingLoop
  private def extendBoundsBegin(expr : IR_Expression, extent : Int) : IR_Expression = {
    expr match {
      case e if Knowledge.experimental_useStefanOffsets =>
        e // don't do anything here

      case IR_IntegerConstant(i) =>
        IR_IntegerConstant(i - extent)

      case bOff @ BoundedExpression(_, _, ArrayAccess(_ : iv.IterationOffsetBegin, _, _)) =>
        (bOff * (extent + 1)) - extent

      case add : IR_AdditionExpression =>
        add.summands.transform {
          case bOff @ BoundedExpression(_, _, ArrayAccess(_ : iv.IterationOffsetBegin, _, _)) =>
            bOff * (extent + 1)
          case x                                                                              =>
            x
        }
        add.summands += IR_IntegerConstant(-extent)
        SimplifyExpression.simplifyIntegralExpr(add)

      // case oInd @ OffsetIndex(0, 1, _, ArrayAccess(_ : iv.IterationOffsetBegin, _, _)) =>
      //   oInd.maxOffset += extent
      //   oInd.index = SimplifyExpression.simplifyIntegralExpr(oInd.index - extent)
      //   oInd.offset = SimplifyExpression.simplifyIntegralExpr(oInd.offset * (extent + 1))
      //   oInd
    }
  }

  // IMPORTANT: must match and extend all possible bounds for LoopOverDimensions inside a ContractingLoop
  private def extendBoundsEnd(expr : IR_Expression, extent : Int) : IR_Expression = {
    expr match {
      case e if Knowledge.experimental_useStefanOffsets =>
        e // don't do anything here

      case IR_IntegerConstant(i) =>
        IR_IntegerConstant(i + extent)

      case bOff @ BoundedExpression(_, _, ArrayAccess(_ : iv.IterationOffsetEnd, _, _)) =>
        (bOff * (extent + 1)) + extent

      case add : IR_AdditionExpression =>
        add.summands.transform {
          case bOff @ BoundedExpression(_, _, ArrayAccess(_ : iv.IterationOffsetEnd, _, _)) =>
            bOff * (extent + 1)
          case x                                                                            =>
            x
        }
        add.summands += IR_IntegerConstant(extent)
        SimplifyExpression.simplifyIntegralExpr(add)

      // case oInd @ OffsetIndex(-1, 0, _, ArrayAccess(_ : iv.IterationOffsetEnd, _, _)) =>
      //   oInd.minOffset -= extent
      //   oInd.index = SimplifyExpression.simplifyIntegralExpr(oInd.index + extent)
      //   oInd.offset = SimplifyExpression.simplifyIntegralExpr(oInd.offset * (extent + 1))
      //   oInd
    }
  }

  private type FieldKey = (String, Int)
  private def FieldKey(field : Field) : FieldKey = {
    return (field.identifier, field.level)
  }

  private def updateSlots(stmts : ListBuffer[IR_Statement], fieldOffset : HashMap[FieldKey, Int]) : Unit = {
    object AdaptFieldSlots extends QuietDefaultStrategy("Adapt field slots") {
      this += new Transformation("now", {
        case fs @ FieldSelection(field, level, SlotAccess(slot, offset), _, _) =>
          fs.slot = new SlotAccess(slot, offset + fieldOffset.getOrElse(FieldKey(field), 0))
          fs
      })
    }
    AdaptFieldSlots.applyStandalone(stmts)
  }

  def processLoopOverDimensions(l : LoopOverDimensions, extent : Int, fieldOffset : HashMap[FieldKey, Int]) : LoopOverDimensions = {
    val nju : LoopOverDimensions = Duplicate(l)
    for (dim <- 0 until nju.numDimensions) {
      nju.indices.begin(dim) = extendBoundsBegin(nju.indices.begin(dim), extent * spec.negExt(dim))
      nju.indices.end(dim) = extendBoundsEnd(nju.indices.end(dim), extent * spec.posExt(dim))
    }
    updateSlots(nju.body, fieldOffset)
    return nju
  }

  def expandSpecial : Output[NodeList] = {
    val res = new ListBuffer[IR_Statement]()
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

          case cStmt @ ConditionStatement(cond, trueBody : ListBuffer[IR_Statement], ListBuffer()) =>
            val bodyWithoutComments = trueBody.filterNot(x => x.isInstanceOf[CommentStatement])
            bodyWithoutComments match {
              case ListBuffer(l : LoopOverDimensions) =>
                val nju = processLoopOverDimensions(l, (number - i), fieldOffset)
                if (condStmt == null || cond != condStmt.condition) {
                  condStmt = Duplicate(cStmt)
                  condStmt.trueBody.clear()
                  res += condStmt
                }
                condStmt.trueBody += nju
              case _                                  =>
            }

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
    var startOffset : IR_ExpressionIndex,
    var endOffset : IR_ExpressionIndex,
    var increment : IR_ExpressionIndex,
    var body : ListBuffer[IR_Statement],
    var preComms : ListBuffer[CommunicateStatement] = ListBuffer(),
    var postComms : ListBuffer[CommunicateStatement] = ListBuffer(),
    var reduction : Option[Reduction] = None,
    var condition : Option[IR_Expression] = None) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverPoints\n"

  def expandSpecial(collector : StackCollector) : Output[StatementList] = {
    val insideFragLoop = collector.stack.map({ case loop : LoopOverFragments => true; case _ => false }).reduce((left, right) => left || right)
    val innerLoop =
      if (Knowledge.experimental_splitLoopsForAsyncComm)
        LoopOverPointsInOneFragment(field.domain.index, field, region, seq, startOffset, endOffset, increment, body, preComms, postComms, reduction, condition)
      else
        LoopOverPointsInOneFragment(field.domain.index, field, region, seq, startOffset, endOffset, increment, body, ListBuffer(), ListBuffer(), reduction, condition)

    var stmts = ListBuffer[IR_Statement]()
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
    var startOffset : IR_ExpressionIndex,
    var endOffset : IR_ExpressionIndex,
    var increment : IR_ExpressionIndex,
    var body : ListBuffer[IR_Statement],
    var preComms : ListBuffer[CommunicateStatement] = ListBuffer(),
    var postComms : ListBuffer[CommunicateStatement] = ListBuffer(),
    var reduction : Option[Reduction] = None,
    var condition : Option[IR_Expression] = None) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverPointsInOneFragment\n"

  def numDims = field.fieldLayout.numDimsGrid

  def expandSpecial : Output[StatementList] = {
    var start = IR_ExpressionIndex(Array.fill(numDims)(0))
    var stop = IR_ExpressionIndex(Array.fill(numDims)(0))
    if (region.isDefined) {
      // case where a special region is to be traversed
      val regionCode = region.get.region.toUpperCase().charAt(0)

      start = IR_ExpressionIndex((0 until numDims).view.map({
        case dim if region.get.dir(dim) == 0 => field.fieldLayout.idxById(regionCode + "LB", dim) - field.referenceOffset(dim) + startOffset(dim)
        case dim if region.get.dir(dim) < 0  => field.fieldLayout.idxById(regionCode + "LB", dim) - field.referenceOffset(dim) + startOffset(dim)
        case dim if region.get.dir(dim) > 0  => field.fieldLayout.idxById(regionCode + "RB", dim) - field.referenceOffset(dim) + startOffset(dim)
      }).toArray[IR_Expression])

      stop = IR_ExpressionIndex((0 until numDims).view.map({
        case dim if region.get.dir(dim) == 0 => field.fieldLayout.idxById(regionCode + "RE", dim) - field.referenceOffset(dim) - endOffset(dim)
        case dim if region.get.dir(dim) < 0  => field.fieldLayout.idxById(regionCode + "LE", dim) - field.referenceOffset(dim) - endOffset(dim)
        case dim if region.get.dir(dim) > 0  => field.fieldLayout.idxById(regionCode + "RE", dim) - field.referenceOffset(dim) - endOffset(dim)
      }).toArray[IR_Expression])
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
                start(dim) = IR_AdditionExpression(field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim), numDupLeft * BoundedExpression(0, 1, ArrayAccess(iv.IterationOffsetBegin(field.domain.index), dim)))
              // start(dim) = OffsetIndex(0, numDupLeft, field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim), numDupLeft * ArrayAccess(iv.IterationOffsetBegin(field.domain.index), dim))
              else
                start(dim) = field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
              if (numDupRight > 0)
                stop(dim) = IR_AdditionExpression(field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim), numDupRight * BoundedExpression(-1, 0, ArrayAccess(iv.IterationOffsetEnd(field.domain.index), dim)))
              // stop(dim) = OffsetIndex(-numDupRight, 0, field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim), numDupRight * ArrayAccess(iv.IterationOffsetEnd(field.domain.index), dim))
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
      if (!condition.isDefined) condition = Some(IR_BooleanConstant(true))
      for (dim <- 0 until numDims)
        if (field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft > 0)
        /*if ("node" == field.fieldLayout.discretization
        || ("face_x" == field.fieldLayout.discretization && 0 == dim)
        || ("face_y" == field.fieldLayout.discretization && 1 == dim)
        || ("face_z" == field.fieldLayout.discretization && 2 == dim))*/
          condition = Some(IR_AndAndExpression(condition.get, IR_GreaterEqualExpression(VariableAccess(dimToString(dim), Some(IR_IntegerDatatype)), field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft)))
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

    var stmts = ListBuffer[IR_Statement]()

    if (Knowledge.experimental_splitLoopsForAsyncComm && !preComms.isEmpty && !postComms.isEmpty) {
      Logger.warn("Found unsupported case of a loop with pre and post communication statements - post communication statements will be ignored")
    }

    if (Knowledge.experimental_splitLoopsForAsyncComm && !preComms.isEmpty) {
      if (region.isDefined) Logger.warn("Found region loop with communication step")

      // gather occuring field access offsets - will determine bounds of inner and outer loop
      GatherFieldAccessOffsets.accesses.clear
      GatherFieldAccessOffsets.applyStandalone(IR_Scope(body))

      // check dimensionality of involved fields
      val numDims = field.fieldLayout.numDimsGrid
      for (cs <- preComms)
        if (numDims != cs.field.fieldLayout.numDimsGrid)
          Logger.warn(s"Found communicate statement on field ${ cs.field.codeName } which is incompatible with dimensionality $numDims")

      // loop iterator
      val loopIt = LoopOverDimensions.defIt(numDims)

      // init default bounds ...
      var lowerBounds : IndexedSeq[IR_Expression] = (0 until numDims).map(dim => {
        val minWidth = (if (0 == dim) Knowledge.experimental_splitLoops_minInnerWidth else 0)
        start(dim) + minWidth
      })
      var upperBounds : IndexedSeq[IR_Expression] = (0 until numDims).map(dim => {
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
          var ret : IR_Expression = new IR_MinimumExpression(GatherFieldAccessOffsets.accesses.getOrElse(cs.field.codeName, ListBuffer()).map(_ (dim)))
          ret = 1 - ret
          SimplifyStrategy.doUntilDoneStandalone(IR_ExpressionStatement(ret))
          start(dim) + ret
        })

        lowerBounds = (lowerBounds, newLowerBounds).zipped.map(new IR_MaximumExpression(_, _))
      }

      for (cs <- preComms) {
        val newUpperBounds = (0 until numDims).map(dim => {
          var ret : IR_Expression = new IR_MaximumExpression(GatherFieldAccessOffsets.accesses.getOrElse(cs.field.codeName, ListBuffer()).map(_ (dim)))
          ret = 1 + ret
          SimplifyStrategy.doUntilDoneStandalone(IR_ExpressionStatement(ret))
          stop(dim) - ret
        })

        upperBounds = (upperBounds, newUpperBounds).zipped.map(new IR_MinimumExpression(_, _))
      }

      // start communication
      stmts ++= preComms.filter(comm => "begin" == comm.op)
      stmts ++= preComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "begin"; newComm })

      // innerRegion
      var coreLoop = Duplicate(loop)
      coreLoop.condition = Some(IR_AndAndExpression(coreLoop.condition.getOrElse(IR_BooleanConstant(true)),
        (0 until field.fieldLayout.numDimsGrid).map(dim =>
          IR_AndAndExpression(IR_GreaterEqualExpression(loopIt(dim), lowerBounds(dim)), IR_LowerExpression(loopIt(dim), upperBounds(dim))) : IR_Expression).reduce(IR_AndAndExpression(_, _))))

      stmts += coreLoop

      // finish communication
      stmts ++= preComms.filter(comm => "finish" == comm.op)
      stmts ++= preComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "finish"; newComm })

      // outerRegion
      var boundaryLoop = Duplicate(loop)
      boundaryLoop.condition = Some(IR_OrOrExpression(boundaryLoop.condition.getOrElse(IR_BooleanConstant(false)),
        (0 until field.fieldLayout.numDimsGrid).map(dim =>
          IR_OrOrExpression(IR_LowerExpression(loopIt(dim), lowerBounds(dim)), IR_GreaterEqualExpression(loopIt(dim), upperBounds(dim))) : IR_Expression).reduce(IR_OrOrExpression(_, _))))
      stmts += boundaryLoop
    } else if (Knowledge.experimental_splitLoopsForAsyncComm && !postComms.isEmpty) {
      if (region.isDefined) Logger.warn("Found region loop with communication step")

      // check dimensionality of involved fields
      val numDims = field.fieldLayout.numDimsGrid
      for (cs <- postComms)
        if (numDims != cs.field.fieldLayout.numDimsGrid)
          Logger.warn(s"Found communicate statement on field ${ cs.field.codeName } which is incompatible with dimensionality $numDims")

      // loop iterator
      val loopIt = LoopOverDimensions.defIt(numDims)

      // init default bounds ...
      var lowerBounds : IndexedSeq[IR_Expression] = (0 until numDims).map(dim => {
        val minWidth = (if (0 == dim) Knowledge.experimental_splitLoops_minInnerWidth else 0)
        start(dim) + minWidth
      })
      var upperBounds : IndexedSeq[IR_Expression] = (0 until numDims).map(dim => {
        val minWidth = (if (0 == dim) Knowledge.experimental_splitLoops_minInnerWidth else 0)
        stop(dim) - minWidth
      })

      // ... and compile actual loop condition bounds
      // Concept:
      //  enforce that elements to be synchronized are in outer region
      //  optionally enforce a certain number of elements for vectorization/ optimized cache line usage
      for (cs <- postComms) {
        val newLowerBounds = (0 until numDims).map(dim => {
          val ret : IR_Expression = cs.field.fieldLayout.layoutsPerDim(dim).numGhostLayersRight + cs.field.fieldLayout.layoutsPerDim(dim).numDupLayersRight
          cs.field.fieldLayout.idxById("DLB", dim) - cs.field.referenceOffset(dim) + ret
        })

        lowerBounds = (lowerBounds, newLowerBounds).zipped.map(new IR_MaximumExpression(_, _))
      }

      for (cs <- postComms) {
        val newUpperBounds = (0 until numDims).map(dim => {
          val ret : IR_Expression = cs.field.fieldLayout.layoutsPerDim(dim).numGhostLayersLeft + cs.field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft
          cs.field.fieldLayout.idxById("DRE", dim) - cs.field.referenceOffset(dim) - ret
        })

        upperBounds = (upperBounds, newUpperBounds).zipped.map(new IR_MinimumExpression(_, _))
      }

      // outerRegion
      var boundaryLoop = Duplicate(loop)
      boundaryLoop.condition = Some(IR_OrOrExpression(boundaryLoop.condition.getOrElse(IR_BooleanConstant(false)),
        (0 until field.fieldLayout.numDimsGrid).map(dim =>
          IR_OrOrExpression(IR_LowerExpression(loopIt(dim), lowerBounds(dim)), IR_GreaterEqualExpression(loopIt(dim), upperBounds(dim))) : IR_Expression).reduce(IR_OrOrExpression(_, _))))
      stmts += boundaryLoop

      // start communication
      stmts ++= postComms.filter(comm => "begin" == comm.op)
      stmts ++= postComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "begin"; newComm })

      // innerRegion
      var coreLoop = Duplicate(loop)
      coreLoop.condition = Some(IR_AndAndExpression(coreLoop.condition.getOrElse(IR_BooleanConstant(true)),
        (0 until field.fieldLayout.numDimsGrid).map(dim =>
          IR_AndAndExpression(IR_GreaterEqualExpression(loopIt(dim), lowerBounds(dim)), IR_LowerExpression(loopIt(dim), upperBounds(dim))) : IR_Expression).reduce(IR_AndAndExpression(_, _))))

      stmts += coreLoop

      // finish communication
      stmts ++= postComms.filter(comm => "finish" == comm.op)
      stmts ++= postComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "finish"; newComm })
    } else {
      stmts += loop
    }

    if (region.isDefined) {
      if (region.get.onlyOnBoundary) {
        val neighIndex = Fragment.getNeigh(region.get.dir.indices).index
        stmts = ListBuffer[IR_Statement](new ConditionStatement(IR_NegationExpression(iv.NeighborIsValid(domain, neighIndex)), stmts))
      }
    }

    if (domain >= 0) {
      stmts = ListBuffer[IR_Statement](new ConditionStatement(iv.IsValidForSubdomain(domain), stmts))
    }

    stmts
  }
}

object LoopOverDimensions {
  def defIt(numDims : Int) = {
    IR_ExpressionIndex((0 until numDims).map(dim => defItForDim(dim) : IR_Expression).toArray)
  }
  def defItForDim(dim : Int) = {
    new VariableAccess(dimToString(dim), Some(IR_IntegerDatatype))
  }
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

case class LoopOverDimensions(var numDimensions : Int,
    var indices : IndexRange,
    var body : ListBuffer[IR_Statement],
    var stepSize : IR_ExpressionIndex = null, // acutal default set in constructor
    var reduction : Option[Reduction] = None,
    var condition : Option[IR_Expression] = None,
    var genOMPThreadLoop : Boolean = false) extends IR_Statement {
  def this(numDimensions : Int, indices : IndexRange, body : IR_Statement, stepSize : IR_ExpressionIndex, reduction : Option[Reduction], condition : Option[IR_Expression]) = this(numDimensions, indices, ListBuffer[IR_Statement](body), stepSize, reduction, condition)
  def this(numDimensions : Int, indices : IndexRange, body : IR_Statement, stepSize : IR_ExpressionIndex, reduction : Option[Reduction]) = this(numDimensions, indices, ListBuffer[IR_Statement](body), stepSize, reduction)
  def this(numDimensions : Int, indices : IndexRange, body : IR_Statement, stepSize : IR_ExpressionIndex) = this(numDimensions, indices, ListBuffer[IR_Statement](body), stepSize)
  def this(numDimensions : Int, indices : IndexRange, body : IR_Statement) = this(numDimensions, indices, ListBuffer[IR_Statement](body))

  import LoopOverDimensions._

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
    return (totalNumPoints > Knowledge.omp_minWorkItemsPerThread * Knowledge.omp_numThreads)
  }

  def explParLoop = lcCSEApplied && this.isInstanceOf[OMP_PotentiallyParallel] &&
    Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverDimensions &&
    parallelizationIsReasonable && parDims.isEmpty

  def createOMPThreadsWrapper(body : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = {
    if (explParLoop) {
      val begin = new VariableDeclarationStatement(IR_IntegerDatatype, threadIdxName, IR_IntegerConstant(0))
      val end = new IR_LowerExpression(new VariableAccess(threadIdxName, IR_IntegerDatatype), IR_IntegerConstant(Knowledge.omp_numThreads))
      val inc = new IR_ExpressionStatement(new IR_PreIncrementExpression(new VariableAccess(threadIdxName, IR_IntegerDatatype)))
      return ListBuffer(new ForLoopStatement(begin, end, inc, body) with OMP_PotentiallyParallel)

    } else
      return body
  }

  def create1stItConds() : ListBuffer[IR_Statement] = {
    val inds = if (explParLoop) ompIndices else indices

    var conds : ListBuffer[IR_Statement] = new ListBuffer()
    // add conditions for first iteration
    for (d <- 0 until numDimensions)
      if (!at1stIt(d)._1.isEmpty) {
        val cond = new ConditionStatement(new IR_EqEqExpression(new VariableAccess(dimToString(d), IR_IntegerDatatype), Duplicate(inds.begin(d))), at1stIt(d)._1)
        for ((annotId, value) <- at1stIt(d)._2)
          cond.annotate(annotId, value)
        conds += cond
      }

    return conds
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
    def thrId = new VariableAccess(threadIdxName, IR_IntegerDatatype)
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
      wrappedBody = ListBuffer[IR_Statement](new ConditionStatement(condition.get, wrappedBody))

    var anyPar : Boolean = false
    val outerPar = if (parDims.isEmpty) -1 else parDims.max
    val inds = if (explParLoop) ompIndices else indices
    // compile loop(s)
    for (d <- 0 until numDimensions) {
      def it = VariableAccess(dimToString(d), Some(IR_IntegerDatatype))
      val decl = VariableDeclarationStatement(IR_IntegerDatatype, dimToString(d), Some(inds.begin(d)))
      val cond = IR_LowerExpression(it, inds.end(d))
      val incr = AssignmentStatement(it, stepSize(d), "+=")
      val compiledLoop : ForLoopStatement with OptimizationHint =
        if (parallelize(d) && d == outerPar) {
          anyPar = true
          val omp = new ForLoopStatement(decl, cond, incr, wrappedBody, reduction) with OptimizationHint with OMP_PotentiallyParallel
          omp.collapse = numDimensions
          omp
        } else
          new ForLoopStatement(decl, cond, incr, wrappedBody, reduction) with OptimizationHint
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
      def redExp = VariableAccess(redExpName, None)
      val redExpLocalName = redExpName + "_red"
      def redExpLocal = VariableAccess(redExpLocalName, None)

      // FIXME: this assumes real data types -> data type should be determined according to redExp
      val decl = VariableDeclarationStatement(IR_ArrayDatatype(IR_RealDatatype, Knowledge.omp_numThreads), redExpLocalName, None)
      val init = (0 until Knowledge.omp_numThreads).map(fragIdx => AssignmentStatement(ArrayAccess(redExpLocal, fragIdx), redExp))
      val redOperands = ListBuffer[IR_Expression](redExp) ++= (0 until Knowledge.omp_numThreads).map(fragIdx => ArrayAccess(redExpLocal, fragIdx) : IR_Expression)
      val red = AssignmentStatement(redExp, if ("min" == redOp) IR_MinimumExpression(redOperands) else IR_MaximumExpression(redOperands))

      ReplaceStringConstantsStrategy.toReplace = redExp.prettyprint
      ReplaceStringConstantsStrategy.replacement = ArrayAccess(redExpLocal, VariableAccess("omp_tid", Some(IR_IntegerDatatype)))
      ReplaceStringConstantsStrategy.applyStandalone(body)
      body.prepend(VariableDeclarationStatement(IR_IntegerDatatype, "omp_tid", Some("omp_get_thread_num()")))

      retStmts = ListBuffer(IR_Scope(decl +=: init ++=: wrappedBody += red))
    }

    retStmts
  }
}

object LoopOverFragments {def defIt = "fragmentIdx" }

case class LoopOverFragments(var body : ListBuffer[IR_Statement], var reduction : Option[Reduction] = None) extends IR_Statement with Expandable {

  import LoopOverFragments._

  def this(body : IR_Statement, reduction : Option[Reduction]) = this(ListBuffer(body), reduction)
  def this(body : IR_Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverFragments\n"

  def generateBasicLoop(parallelize : Boolean) = {
    val loop = if (parallelize)
      new ForLoopStatement(
        VariableDeclarationStatement(IR_IntegerDatatype, defIt, Some(0)),
        IR_LowerExpression(defIt, Knowledge.domain_numFragmentsPerBlock),
        IR_PreIncrementExpression(defIt),
        body,
        reduction) with OMP_PotentiallyParallel
    else
      new ForLoopStatement(
        VariableDeclarationStatement(IR_IntegerDatatype, defIt, Some(0)),
        IR_LowerExpression(defIt, Knowledge.domain_numFragmentsPerBlock),
        IR_PreIncrementExpression(defIt),
        body,
        reduction)
    loop.annotate("numLoopIterations", Knowledge.domain_numFragmentsPerBlock)
    loop
  }

  override def expand() : Output[StatementList] = {
    var statements = new ListBuffer[IR_Statement]

    if (Knowledge.experimental_resolveUnreqFragmentLoops && Knowledge.domain_numFragmentsPerBlock <= 1) {
      // eliminate fragment loops in case of only one fragment per block
      statements = ListBuffer(IR_Scope(body))

      // replace references to old loop iterator
      ReplaceStringConstantsStrategy.toReplace = defIt
      ReplaceStringConstantsStrategy.replacement = IR_IntegerConstant(0)
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
        val decl = VariableDeclarationStatement(IR_ArrayDatatype(IR_RealDatatype, Knowledge.omp_numThreads), redExpLocalName, None)
        val init = (0 until Knowledge.omp_numThreads).map(fragIdx => AssignmentStatement(ArrayAccess(redExpLocal, fragIdx), redExp))
        val redOperands = ListBuffer[IR_Expression](redExp) ++ (0 until Knowledge.omp_numThreads).map(fragIdx => ArrayAccess(redExpLocal, fragIdx) : IR_Expression)
        val red = AssignmentStatement(redExp, if ("min" == redOp) IR_MinimumExpression(redOperands) else IR_MaximumExpression(redOperands))

        ReplaceStringConstantsStrategy.toReplace = redExp.prettyprint
        ReplaceStringConstantsStrategy.replacement = ArrayAccess(redExpLocal, VariableAccess("omp_tid", Some(IR_IntegerDatatype)))
        ReplaceStringConstantsStrategy.applyStandalone(body)
        body.prepend(VariableDeclarationStatement(IR_IntegerDatatype, "omp_tid", Some("omp_get_thread_num()")))

        statements += IR_Scope(ListBuffer[IR_Statement](decl)
          ++ init
          ++ ListBuffer[IR_Statement](generateBasicLoop(parallelize), red))
      }
    }

    if (Knowledge.mpi_enabled && reduction.isDefined) {
      statements += new MPI_Allreduce(IR_AddressofExpression(reduction.get.target), IR_RealDatatype, 1, reduction.get.op) // FIXME: get dt and cnt from reduction
    }

    statements
  }
}

object LoopOverDomains {def defIt = "domainIdx" }

case class LoopOverDomains(var body : ListBuffer[IR_Statement]) extends IR_Statement with Expandable {

  import LoopOverDomains._

  def this(body : IR_Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverDomains\n"

  override def expand() : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(IR_IntegerDatatype, defIt, Some(0)),
      IR_LowerExpression(defIt, DomainCollection.domains.size),
      IR_PreIncrementExpression(defIt),
      body)
  }
}

object LoopOverFields {def defIt = "fieldIdx" }

case class LoopOverFields(var body : ListBuffer[IR_Statement]) extends IR_Statement with Expandable {

  import LoopOverFields._

  def this(body : IR_Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverFields\n"

  override def expand() : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(IR_IntegerDatatype, defIt, Some(0)),
      IR_LowerExpression(defIt, FieldCollection.fields.size),
      IR_PreIncrementExpression(defIt),
      body)
  }
}

object LoopOverLevels {def defIt = "levelIdx" }

case class LoopOverLevels(var body : ListBuffer[IR_Statement]) extends IR_Statement with Expandable {

  import LoopOverLevels._

  def this(body : IR_Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverLevels\n"

  override def expand() : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(IR_IntegerDatatype, defIt, Some(Knowledge.minLevel)),
      IR_LowerExpression(defIt, Knowledge.maxLevel + 1),
      IR_PreIncrementExpression(defIt),
      body)
  }
}

object LoopOverNeighbors {def defIt = "neighborIdx" }

case class LoopOverNeighbors(var body : ListBuffer[IR_Statement]) extends IR_Statement with Expandable {

  import LoopOverNeighbors._

  def this(body : IR_Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverNeighbors\n"

  override def expand() : Output[ForLoopStatement] = {
    new ForLoopStatement(
      VariableDeclarationStatement(IR_IntegerDatatype, defIt, Some(0)),
      IR_LowerExpression(defIt, Fragment.neighbors.size),
      IR_PreIncrementExpression(defIt),
      body)
  }
}
