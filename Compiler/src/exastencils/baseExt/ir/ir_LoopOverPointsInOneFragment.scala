package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.communication.ir.IR_Communicate
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.deprecated.ir.IR_DimToString
import exastencils.domain.ir._
import exastencils.field.ir.IR_Field
import exastencils.knowledge.Fragment
import exastencils.logger.Logger
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.polyhedron.PolyhedronAccessible
import exastencils.prettyprinting.PpStream
import exastencils.strategies._

// FIXME: refactor: extract functionality, reduce complexity
case class IR_LoopOverPointsInOneFragment(var domain : Int,
    var field : IR_Field,
    var region : Option[IR_RegionSpecification],
    var startOffset : IR_ExpressionIndex,
    var endOffset : IR_ExpressionIndex,
    var increment : IR_ExpressionIndex,
    var body : ListBuffer[IR_Statement],
    var preComms : ListBuffer[IR_Communicate] = ListBuffer(),
    var postComms : ListBuffer[IR_Communicate] = ListBuffer(),
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo(),
    var condition : Option[IR_Expression] = None) extends IR_Statement {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

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
                start(dim) = IR_AdditionExpression(field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim), numDupLeft * IR_BoundedScalar(0, 1, IR_IV_IterationOffsetBegin(dim, field.domain.index)))
              // start(dim) = OffsetIndex(0, numDupLeft, field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim), numDupLeft * ArrayAccess(iv.IterationOffsetBegin(field.domain.index), dim))
              else
                start(dim) = field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
              if (numDupRight > 0)
                stop(dim) = IR_AdditionExpression(field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim), numDupRight * IR_BoundedScalar(-1, 0, IR_IV_IterationOffsetEnd(dim, field.domain.index)))
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

    val indexRange = IR_ExpressionIndexRange(start, stop)
    SimplifyStrategy.doUntilDoneStandalone(indexRange)

    // fix iteration space for reduction operations if required
    if (Knowledge.experimental_trimBoundsForReductionLoops && parallelization.reduction.isDefined && !region.isDefined) {
      if (!condition.isDefined) condition = Some(IR_BooleanConstant(true))
      for (dim <- 0 until numDims)
        if (field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft > 0)
        /*if ("node" == field.fieldLayout.discretization
        || ("face_x" == field.fieldLayout.discretization && 0 == dim)
        || ("face_y" == field.fieldLayout.discretization && 1 == dim)
        || ("face_z" == field.fieldLayout.discretization && 2 == dim))*/
          condition = Some(IR_AndAndExpression(condition.get, IR_GreaterEqualExpression(IR_VariableAccess(IR_DimToString(dim), IR_IntegerDatatype), field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft)))
    }

    var loop : IR_LoopOverDimensions = {
      if (!parallelization.potentiallyParallel)
        IR_LoopOverDimensions(numDims, indexRange, body, increment, parallelization, condition)
      else {
        val ret = new IR_LoopOverDimensions(numDims, indexRange, body, increment, parallelization, condition) with PolyhedronAccessible
        ret.optLevel =
          if (Knowledge.maxLevel - field.level < Knowledge.poly_numFinestLevels)
            Knowledge.poly_optLevel_fine
          else
            Knowledge.poly_optLevel_coarse
        ret
      }
    }

    var stmts = ListBuffer[IR_Statement]()

    if (Knowledge.experimental_splitLoopsForAsyncComm && preComms.nonEmpty && postComms.nonEmpty) {
      Logger.warn("Found unsupported case of a loop with pre and post communication statements - post communication statements will be ignored")
    }

    if (Knowledge.experimental_splitLoopsForAsyncComm && preComms.nonEmpty) {
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
      val loopIt = IR_LoopOverDimensions.defIt(numDims)

      // init default bounds ...
      var lowerBounds : IndexedSeq[IR_Expression] = (0 until numDims).map(dim => {
        val minWidth = if (0 == dim) Knowledge.experimental_splitLoops_minInnerWidth else 0
        start(dim) + minWidth
      })
      var upperBounds : IndexedSeq[IR_Expression] = (0 until numDims).map(dim => {
        val minWidth = if (0 == dim) Knowledge.experimental_splitLoops_minInnerWidth else 0
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

        lowerBounds = (lowerBounds, newLowerBounds).zipped.map(IR_MaximumExpression(_, _))
      }

      for (cs <- preComms) {
        val newUpperBounds = (0 until numDims).map(dim => {
          var ret : IR_Expression = new IR_MaximumExpression(GatherFieldAccessOffsets.accesses.getOrElse(cs.field.codeName, ListBuffer()).map(_ (dim)))
          ret = 1 + ret
          SimplifyStrategy.doUntilDoneStandalone(IR_ExpressionStatement(ret))
          stop(dim) - ret
        })

        upperBounds = (upperBounds, newUpperBounds).zipped.map(IR_MinimumExpression(_, _))
      }

      // start communication
      stmts ++= preComms.filter(comm => "begin" == comm.op)
      stmts ++= preComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "begin"; newComm })

      // innerRegion
      var coreLoop = Duplicate(loop)
      coreLoop.condition = Some(IR_AndAndExpression(coreLoop.condition.getOrElse(IR_BooleanConstant(true)),
        (0 until field.fieldLayout.numDimsGrid).map(dim =>
          IR_AndAndExpression(IR_GreaterEqualExpression(loopIt(dim), lowerBounds(dim)), IR_LowerExpression(loopIt(dim), upperBounds(dim))) : IR_Expression).reduce(IR_AndAndExpression)))

      stmts += coreLoop

      // finish communication
      stmts ++= preComms.filter(comm => "finish" == comm.op)
      stmts ++= preComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "finish"; newComm })

      // outerRegion
      var boundaryLoop = Duplicate(loop)
      boundaryLoop.condition = Some(IR_OrOrExpression(boundaryLoop.condition.getOrElse(IR_BooleanConstant(false)),
        (0 until field.fieldLayout.numDimsGrid).map(dim =>
          IR_OrOrExpression(IR_LowerExpression(loopIt(dim), lowerBounds(dim)), IR_GreaterEqualExpression(loopIt(dim), upperBounds(dim))) : IR_Expression).reduce(IR_OrOrExpression)))
      stmts += boundaryLoop
    } else if (Knowledge.experimental_splitLoopsForAsyncComm && postComms.nonEmpty) {
      if (region.isDefined) Logger.warn("Found region loop with communication step")

      // check dimensionality of involved fields
      val numDims = field.fieldLayout.numDimsGrid
      for (cs <- postComms)
        if (numDims != cs.field.fieldLayout.numDimsGrid)
          Logger.warn(s"Found communicate statement on field ${ cs.field.codeName } which is incompatible with dimensionality $numDims")

      // loop iterator
      val loopIt = IR_LoopOverDimensions.defIt(numDims)

      // init default bounds ...
      var lowerBounds : IndexedSeq[IR_Expression] = (0 until numDims).map(dim => {
        val minWidth = if (0 == dim) Knowledge.experimental_splitLoops_minInnerWidth else 0
        start(dim) + minWidth
      })
      var upperBounds : IndexedSeq[IR_Expression] = (0 until numDims).map(dim => {
        val minWidth = if (0 == dim) Knowledge.experimental_splitLoops_minInnerWidth else 0
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

        lowerBounds = (lowerBounds, newLowerBounds).zipped.map(IR_MaximumExpression(_, _))
      }

      for (cs <- postComms) {
        val newUpperBounds = (0 until numDims).map(dim => {
          val ret : IR_Expression = cs.field.fieldLayout.layoutsPerDim(dim).numGhostLayersLeft + cs.field.fieldLayout.layoutsPerDim(dim).numDupLayersLeft
          cs.field.fieldLayout.idxById("DRE", dim) - cs.field.referenceOffset(dim) - ret
        })

        upperBounds = (upperBounds, newUpperBounds).zipped.map(IR_MinimumExpression(_, _))
      }

      // outerRegion
      var boundaryLoop = Duplicate(loop)
      boundaryLoop.condition = Some(IR_OrOrExpression(boundaryLoop.condition.getOrElse(IR_BooleanConstant(false)),
        (0 until field.fieldLayout.numDimsGrid).map(dim =>
          IR_OrOrExpression(IR_LowerExpression(loopIt(dim), lowerBounds(dim)), IR_GreaterEqualExpression(loopIt(dim), upperBounds(dim))) : IR_Expression).reduce(IR_OrOrExpression)))
      stmts += boundaryLoop

      // start communication
      stmts ++= postComms.filter(comm => "begin" == comm.op)
      stmts ++= postComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "begin"; newComm })

      // innerRegion
      var coreLoop = Duplicate(loop)
      coreLoop.condition = Some(IR_AndAndExpression(coreLoop.condition.getOrElse(IR_BooleanConstant(true)),
        (0 until field.fieldLayout.numDimsGrid).map(dim =>
          IR_AndAndExpression(IR_GreaterEqualExpression(loopIt(dim), lowerBounds(dim)), IR_LowerExpression(loopIt(dim), upperBounds(dim))) : IR_Expression).reduce(IR_AndAndExpression)))

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
        stmts = ListBuffer[IR_Statement](IR_IfCondition(IR_NegationExpression(IR_IV_NeighborIsValid(domain, neighIndex)), stmts))
      }
    }

    if (domain >= 0) {
      stmts = ListBuffer[IR_Statement](IR_IfCondition(IR_IV_IsValidForDomain(domain), stmts))
    }

    stmts
  }
}

/// IR_ResolveLoopOverPointsInOneFragment

object IR_ResolveLoopOverPointsInOneFragment extends DefaultStrategy("Resole LoopOverPointsInOneFragment nodes") {
  this += new Transformation("Resolve", {
    case loop : IR_LoopOverPointsInOneFragment => loop.expandSpecial
  })
}
