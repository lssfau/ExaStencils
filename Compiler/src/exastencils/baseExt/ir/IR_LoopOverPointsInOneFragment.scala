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

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.domain.ir._
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.grid.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.parallelization.ir._

// FIXME: refactor: extract functionality, reduce complexity
case class IR_LoopOverPointsInOneFragment(var domain : Int,
    var field : IR_FieldLike,
    var region : Option[IR_RegionSpecification],
    var startOffset : IR_ExpressionIndex,
    var endOffset : IR_ExpressionIndex,
    var increment : IR_ExpressionIndex,
    var body : ListBuffer[IR_Statement],
    var preComms : ListBuffer[IR_Communicate] = ListBuffer(),
    var postComms : ListBuffer[IR_Communicate] = ListBuffer(),
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo(),
    var condition : Option[IR_Expression] = None) extends IR_ScopedStatement with IR_SpecialExpandable with IR_HasParallelizationInfo {

  def numDims = field.layout.numDimsGrid

  def expandSpecial : Output[StatementList] = {
    var start = IR_ExpressionIndex(Array.fill(numDims)(0))
    var stop = IR_ExpressionIndex(Array.fill(numDims)(0))
    if (region.isDefined) {
      // case where a special region is to be traversed
      val regionCode = region.get.region.toUpperCase().charAt(0)

      start = IR_ExpressionIndex((0 until numDims).view.map({
        case dim if region.get.dir(dim) == 0 => field.layout.idxById(regionCode + "LB", dim) - field.referenceOffset(dim) + startOffset(dim)
        case dim if region.get.dir(dim) < 0  => field.layout.idxById(regionCode + "LB", dim) - field.referenceOffset(dim) + startOffset(dim)
        case dim if region.get.dir(dim) > 0  => field.layout.idxById(regionCode + "RB", dim) - field.referenceOffset(dim) + startOffset(dim)
      }).toArray[IR_Expression])

      stop = IR_ExpressionIndex((0 until numDims).view.map({
        case dim if region.get.dir(dim) == 0 => field.layout.idxById(regionCode + "RE", dim) - field.referenceOffset(dim) - endOffset(dim)
        case dim if region.get.dir(dim) < 0  => field.layout.idxById(regionCode + "LE", dim) - field.referenceOffset(dim) - endOffset(dim)
        case dim if region.get.dir(dim) > 0  => field.layout.idxById(regionCode + "RE", dim) - field.referenceOffset(dim) - endOffset(dim)
      }).toArray[IR_Expression])
    } else {
      // basic case -> just eliminate 'real' boundaries
      for (dim <- 0 until numDims) {
        field.layout.localization match {
          case IR_AtNode | IR_AtFaceCenter(`dim`) =>
            if (Knowledge.experimental_useStefanOffsets) {
              start(dim) = field.layout.idxById("IB", dim) - field.referenceOffset(dim) + startOffset(dim)
              stop(dim) = field.layout.idxById("IE", dim) - field.referenceOffset(dim) - endOffset(dim)
            } else if (Knowledge.experimental_disableIterationOffsets) {
              start(dim) = field.layout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
              stop(dim) = field.layout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim)
            } else {
              // FIXME
              //              if (Knowledge.experimental_genVariableFieldSizes) {
              //                start(dim) = OffsetIndex(0, 1, ArrayAccess(iv.IndexFromField(field.identifier, field.level, "DLB"), dim) - field.referenceOffset(dim) + startOffset(dim), ArrayAccess(iv.IterationOffsetBegin(field.domain.index), dim))
              //                stop(dim) = OffsetIndex(-1, 0, ArrayAccess(iv.IndexFromField(field.identifier, field.level, "DRE"), dim) - field.referenceOffset(dim) - endOffset(dim), ArrayAccess(iv.IterationOffsetEnd(field.domain.index), dim))
              //              } else {
              val numDupLeft = field.layout.layoutsPerDim(dim).numDupLayersLeft
              val numDupRight = field.layout.layoutsPerDim(dim).numDupLayersRight

              if (numDupLeft > 0)
                start(dim) = IR_Addition(field.layout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim), numDupLeft * IR_BoundedScalar(0, 1, IR_IV_IterationOffsetBegin(dim, field.domain.index)))
              // start(dim) = OffsetIndex(0, numDupLeft, field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim), numDupLeft * ArrayAccess(iv.IterationOffsetBegin(field.domain.index), dim))
              else
                start(dim) = field.layout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
              if (numDupRight > 0)
                stop(dim) = IR_Addition(field.layout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim), numDupRight * IR_BoundedScalar(-1, 0, IR_IV_IterationOffsetEnd(dim, field.domain.index)))
              // stop(dim) = OffsetIndex(-numDupRight, 0, field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim), numDupRight * ArrayAccess(iv.IterationOffsetEnd(field.domain.index), dim))
              else
                stop(dim) = field.layout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim)
              //              }
            }

          case IR_AtCellCenter | IR_AtFaceCenter(_) =>
            start(dim) = field.layout.idxById("DLB", dim) - field.referenceOffset(dim) + startOffset(dim)
            stop(dim) = field.layout.idxById("DRE", dim) - field.referenceOffset(dim) - endOffset(dim)
        }
      }
    }

    val indexRange = IR_ExpressionIndexRange(start, stop)
    IR_GeneralSimplify.doUntilDoneStandalone(indexRange)

    // fix iteration space for reduction operations if required
    if (Knowledge.experimental_trimBoundsForReductionLoops && parallelization.reduction.isDefined && !region.isDefined) {
      if (!condition.isDefined) condition = Some(IR_BooleanConstant(true))
      for (dim <- 0 until numDims)
        if (field.layout.layoutsPerDim(dim).numDupLayersLeft > 0)
        /*if ("node" == field.fieldLayout.discretization
        || ("face_x" == field.fieldLayout.discretization && 0 == dim)
        || ("face_y" == field.fieldLayout.discretization && 1 == dim)
        || ("face_z" == field.fieldLayout.discretization && 2 == dim))*/
          condition = Some(IR_AndAnd(condition.get, IR_GreaterEqual(IR_FieldIteratorAccess(dim), field.layout.layoutsPerDim(dim).numDupLayersLeft)))
    }

    var loop : IR_LoopOverDimensions = {
      if (!parallelization.potentiallyParallel)
        IR_LoopOverDimensions(numDims, indexRange, body, increment, parallelization, condition)
      else {
        val ret = new IR_LoopOverDimensions(numDims, indexRange, body, increment, parallelization, condition)
        ret.polyOptLevel =
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

    // TODO: move to communication/ir/IR_SplitLoopsForCommunication
    if (Knowledge.experimental_splitLoopsForAsyncComm && preComms.nonEmpty) {
      if (region.isDefined) Logger.warn("Found region loop with communication step")

      // gather occurring field access offsets - will determine bounds of inner and outer loop
      IR_GatherFieldAccessOffsets.accesses.clear
      IR_GatherFieldAccessOffsets.applyStandalone(IR_Scope(body))

      // check dimensionality of involved fields
      val numDims = field.layout.numDimsGrid
      for (cs <- preComms)
        if (numDims != cs.field.layout.numDimsGrid)
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
          var ret : IR_Expression = new IR_Minimum(IR_GatherFieldAccessOffsets.accesses.getOrElse(cs.field.codeName, ListBuffer()).map(_ (dim)))
          ret = 1 - ret
          ret = IR_GeneralSimplifyWrapper.process(ret)
          start(dim) + ret
        })

        lowerBounds = (lowerBounds, newLowerBounds).zipped.map(IR_Maximum(_, _))
      }

      for (cs <- preComms) {
        val newUpperBounds = (0 until numDims).map(dim => {
          var ret : IR_Expression = new IR_Maximum(IR_GatherFieldAccessOffsets.accesses.getOrElse(cs.field.codeName, ListBuffer()).map(_ (dim)))
          ret = 1 + ret
          ret = IR_GeneralSimplifyWrapper.process(ret)
          stop(dim) - ret
        })

        upperBounds = (upperBounds, newUpperBounds).zipped.map(IR_Minimum(_, _))
      }

      // start communication
      stmts ++= preComms.filter(comm => "begin" == comm.op)
      stmts ++= preComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "begin"; newComm })

      // innerRegion
      var coreLoop = Duplicate(loop)
      coreLoop.condition = Some(IR_AndAnd(coreLoop.condition.getOrElse(IR_BooleanConstant(true)),
        (0 until field.layout.numDimsGrid).map(dim =>
          IR_AndAnd(IR_GreaterEqual(loopIt(dim), lowerBounds(dim)), IR_Lower(loopIt(dim), upperBounds(dim))) : IR_Expression).reduce(IR_AndAnd)))

      stmts += coreLoop

      // finish communication
      stmts ++= preComms.filter(comm => "finish" == comm.op)
      stmts ++= preComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "finish"; newComm })

      // outerRegion
      var boundaryLoop = Duplicate(loop)
      boundaryLoop.condition = Some(IR_OrOr(boundaryLoop.condition.getOrElse(IR_BooleanConstant(false)),
        (0 until field.layout.numDimsGrid).map(dim =>
          IR_OrOr(IR_Lower(loopIt(dim), lowerBounds(dim)), IR_GreaterEqual(loopIt(dim), upperBounds(dim))) : IR_Expression).reduce(IR_OrOr)))
      stmts += boundaryLoop
    } else if (Knowledge.experimental_splitLoopsForAsyncComm && postComms.nonEmpty) {
      if (region.isDefined) Logger.warn("Found region loop with communication step")

      // check dimensionality of involved fields
      val numDims = field.layout.numDimsGrid
      for (cs <- postComms)
        if (numDims != cs.field.layout.numDimsGrid)
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
          val ret : IR_Expression = cs.field.layout.layoutsPerDim(dim).numGhostLayersRight + cs.field.layout.layoutsPerDim(dim).numDupLayersRight
          cs.field.layout.idxById("DLB", dim) - cs.field.referenceOffset(dim) + ret
        })

        lowerBounds = (lowerBounds, newLowerBounds).zipped.map(IR_Maximum(_, _))
      }

      for (cs <- postComms) {
        val newUpperBounds = (0 until numDims).map(dim => {
          val ret : IR_Expression = cs.field.layout.layoutsPerDim(dim).numGhostLayersLeft + cs.field.layout.layoutsPerDim(dim).numDupLayersLeft
          cs.field.layout.idxById("DRE", dim) - cs.field.referenceOffset(dim) - ret
        })

        upperBounds = (upperBounds, newUpperBounds).zipped.map(IR_Minimum(_, _))
      }

      // outerRegion
      var boundaryLoop = Duplicate(loop)
      boundaryLoop.condition = Some(IR_OrOr(boundaryLoop.condition.getOrElse(IR_BooleanConstant(false)),
        (0 until field.layout.numDimsGrid).map(dim =>
          IR_OrOr(IR_Lower(loopIt(dim), lowerBounds(dim)), IR_GreaterEqual(loopIt(dim), upperBounds(dim))) : IR_Expression).reduce(IR_OrOr)))
      stmts += boundaryLoop

      // start communication
      stmts ++= postComms.filter(comm => "begin" == comm.op)
      stmts ++= postComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "begin"; newComm })

      // innerRegion
      var coreLoop = Duplicate(loop)
      coreLoop.condition = Some(IR_AndAnd(coreLoop.condition.getOrElse(IR_BooleanConstant(true)),
        (0 until field.layout.numDimsGrid).map(dim =>
          IR_AndAnd(IR_GreaterEqual(loopIt(dim), lowerBounds(dim)), IR_Lower(loopIt(dim), upperBounds(dim))) : IR_Expression).reduce(IR_AndAnd)))

      stmts += coreLoop

      // finish communication
      stmts ++= postComms.filter(comm => "finish" == comm.op)
      stmts ++= postComms.filter(comm => "both" == comm.op).map(comm => { val newComm = Duplicate(comm); newComm.op = "finish"; newComm })
    } else {
      stmts += loop
    }

    if (region.isDefined) {
      if (region.get.onlyOnBoundary) {
        val neighIndex = DefaultNeighbors.getNeigh(region.get.dir.indices).index
        val indexOfRefinedNeighbor : Option[IR_Expression] = None

        stmts = ListBuffer[IR_Statement](IR_IfCondition(IR_Negation(IR_IV_NeighborIsValid(domain, neighIndex, indexOfRefinedNeighbor)), stmts))
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
