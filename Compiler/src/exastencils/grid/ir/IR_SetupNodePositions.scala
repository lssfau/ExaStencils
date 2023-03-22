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

package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.{ IR_Native, _ }
import exastencils.baseExt.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_MathFunctionReference

/// IR_SetupNodePositions

object IR_SetupNodePositions {
  // FIXME IR_Communicate where applicable

  def for_AA_Uniform(level : Int, dim : Int) : ListBuffer[IR_Statement] = {
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to global domain")

    val domain = IR_DomainCollection.getByIdentifier("global").get
    val numDims = domain.numDims

    val numCellsPerFrag = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim)
    val numCellsTotal = numCellsPerFrag * Knowledge.domain_rect_numFragsTotalAsVec(dim)

    // fix grid width to match domain size
    val domainBounds = domain.asInstanceOf[IR_DomainFromAABB].aabb
    val cellWidth = (domainBounds.upper(dim) - domainBounds.lower(dim)) / numCellsTotal

    // look up field and compile access to base element
    val field = IR_VF_NodePositionPerDim.find(level, dim).associatedField
    val baseIndex = IR_LoopOverDimensions.defIt(numDims)
    val baseAccess = IR_FieldAccess(field, 0, baseIndex)

    // fix the inner iterator -> used for zone checks
    def innerIt =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_LoopOverDimensions.defItForDim(dim)
      else
        IR_VariableAccess(s"global_i$dim", IR_IntegerDatatype)

    val innerItDecl =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_NullStatement
      else
        IR_VariableDeclaration(innerIt, IR_LoopOverDimensions.defItForDim(dim) + IR_IV_FragmentIndex(dim) * numCellsPerFrag)

    // compile special boundary handling expressions
    val leftDir = Array.fill(numDims)(0)
    leftDir(dim) = -1
    val leftNeighIndex = DefaultNeighbors.getNeigh(leftDir).index

    val leftGhostIndex = IR_ExpressionIndex(Array.fill(numDims)(0))
    leftGhostIndex(dim) = -2
    val leftGhostAccess = IR_FieldAccess(field, 0, leftGhostIndex)

    val leftBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(field.domain.index, leftNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(IR_GridUtil.offsetAccess(leftGhostAccess, 1, dim),
          2 * IR_GridUtil.offsetAccess(leftGhostAccess, 2, dim) - IR_GridUtil.offsetAccess(leftGhostAccess, 3, dim)),
        IR_Assignment(Duplicate(leftGhostAccess),
          2 * IR_GridUtil.offsetAccess(leftGhostAccess, 1, dim) - IR_GridUtil.offsetAccess(leftGhostAccess, 2, dim))))

    val rightDir = Array.fill(numDims)(0)
    rightDir(dim) = 1
    val rightNeighIndex = DefaultNeighbors.getNeigh(rightDir).index

    val rightGhostIndex = IR_ExpressionIndex(Array.fill(numDims)(0))
    rightGhostIndex(dim) = numCellsPerFrag + 2
    val rightGhostAccess = IR_FieldAccess(field, 0, rightGhostIndex)

    val rightBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(field.domain.index, rightNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(IR_GridUtil.offsetAccess(rightGhostAccess, -1, dim),
          2 * IR_GridUtil.offsetAccess(rightGhostAccess, -2, dim) - IR_GridUtil.offsetAccess(rightGhostAccess, -3, dim)),
        IR_Assignment(Duplicate(rightGhostAccess),
          2 * IR_GridUtil.offsetAccess(rightGhostAccess, -1, dim) - IR_GridUtil.offsetAccess(rightGhostAccess, -2, dim))))

    // compile final loop
    val innerLoop = IR_LoopOverPoints(field, None,
      IR_GridUtil.offsetIndex(IR_ExpressionIndex(Array.fill(numDims)(0)), -2, dim),
      IR_GridUtil.offsetIndex(IR_ExpressionIndex(Array.fill(numDims)(0)), -2, dim),
      IR_ExpressionIndex(Array.fill(numDims)(1)),
      ListBuffer[IR_Statement](
        innerItDecl,
        IR_Assignment(Duplicate(baseAccess),
          domainBounds.lower(dim) + innerIt * cellWidth))
    )
    innerLoop.parallelization.potentiallyParallel = false

    ListBuffer[IR_Statement](
      IR_LoopOverFragments(ListBuffer[IR_Statement](
        innerLoop,
        leftBoundaryUpdate,
        rightBoundaryUpdate)))
  }

  def for_AA_LinearFct(level : Int, dim : Int) : ListBuffer[IR_Statement] = {
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to global domain")

    val domain = IR_DomainCollection.getByIdentifier("global").get
    val numDims = domain.numDims

    val numCellsPerFrag = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim)
    val numCellsTotal = numCellsPerFrag * Knowledge.domain_rect_numFragsTotalAsVec(dim)

    // zone parameters
    val xf = numCellsTotal / 4 - 1 // end of the first (left-most) zone
    val xs = (numCellsTotal / 4) * 3 // start of the last (right-most) zone

    // total size = alphaCoeff * alpha + betaCoeff * beta
    val lastPointAlphaCoeff = -0.5 * xf * xf - 0.5 * xf + xf * numCellsTotal - 0.5 * numCellsTotal * numCellsTotal + 0.5 * numCellsTotal + numCellsTotal * xs - 0.5 * xs * xs - 0.5 * xs
    val lastPointBetaCoeff = numCellsTotal
    // size of the first interval = alphaCoeff * alpha + betaCoeff * beta

    // fix alpha to match domain size
    val domainBounds = domain.asInstanceOf[IR_DomainFromAABB].aabb

    // simple approach: alpha and beta are equal -> results in very small volumes and aspect ratios if the number of points is high
    //    val alpha = domainSize / (lastPointAlphaCoeff + lastPointBetaCoeff)
    //    val beta = alpha

    // better approach: fix the ratio between smallest and largest cell width to 8
    val factor = (numCellsTotal / 4) / 8.0
    val alpha = IR_SimplifyExpression.evalFloating((domainBounds.upper(dim) - domainBounds.lower(dim)) / (lastPointAlphaCoeff + lastPointBetaCoeff * factor))
    val beta = IR_SimplifyExpression.evalFloating(factor * alpha)

    //Logger.warn(s"Using alpha $alpha and beta $beta")

    // look up field and compile access to base element
    val field = IR_VF_NodePositionPerDim.find(level, dim).associatedField

    def baseIndex = IR_LoopOverDimensions.defIt(numDims)

    def baseAccess = IR_FieldAccess(field, 0, baseIndex)

    // fix the inner iterator -> used for zone checks
    def innerIt =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_LoopOverDimensions.defItForDim(dim)
      else
        IR_VariableAccess(s"global_i$dim", IR_IntegerDatatype)

    val innerItDecl =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_NullStatement
      else
        IR_VariableDeclaration(innerIt, IR_LoopOverDimensions.defItForDim(dim) + IR_IV_FragmentIndex(dim) * numCellsPerFrag)

    // compile special boundary handling expressions
    val leftDir = Array.fill(numDims)(0)
    leftDir(dim) = -1
    val leftNeighIndex = DefaultNeighbors.getNeigh(leftDir).index

    val leftGhostIndex = IR_ExpressionIndex(Array.fill(numDims)(0))
    leftGhostIndex(dim) = -2

    def leftGhostAccess = IR_FieldAccess(field, 0, leftGhostIndex)

    val leftBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(field.domain.index, leftNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(IR_GridUtil.offsetAccess(leftGhostAccess, 1, dim),
          2 * IR_GridUtil.offsetAccess(leftGhostAccess, 2, dim) - IR_GridUtil.offsetAccess(leftGhostAccess, 3, dim)),
        IR_Assignment(Duplicate(leftGhostAccess),
          2 * IR_GridUtil.offsetAccess(leftGhostAccess, 1, dim) - IR_GridUtil.offsetAccess(leftGhostAccess, 2, dim))))

    val rightDir = Array.fill(numDims)(0)
    rightDir(dim) = 1
    val rightNeighIndex = DefaultNeighbors.getNeigh(rightDir).index

    val rightGhostIndex = IR_ExpressionIndex(Array.fill(numDims)(0))
    rightGhostIndex(dim) = numCellsPerFrag + 2

    def rightGhostAccess = IR_FieldAccess(field, 0, rightGhostIndex)

    val rightBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(field.domain.index, rightNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(IR_GridUtil.offsetAccess(rightGhostAccess, -1, dim),
          2 * IR_GridUtil.offsetAccess(rightGhostAccess, -2, dim) - IR_GridUtil.offsetAccess(rightGhostAccess, -3, dim)),
        IR_Assignment(Duplicate(rightGhostAccess),
          2 * IR_GridUtil.offsetAccess(rightGhostAccess, -1, dim) - IR_GridUtil.offsetAccess(rightGhostAccess, -2, dim))))

    // compile final loop
    val innerLoop = IR_LoopOverPoints(field, None,
      IR_GridUtil.offsetIndex(IR_ExpressionIndex(Array.fill(numDims)(0)), -2, dim),
      IR_GridUtil.offsetIndex(IR_ExpressionIndex(Array.fill(numDims)(0)), -2, dim),
      IR_ExpressionIndex(Array.fill(numDims)(1)),
      ListBuffer[IR_Statement](
        innerItDecl,
        IR_IfCondition(IR_LowerEqual(innerIt, xf + 1),
          IR_Assignment(Duplicate(baseAccess),
            domainBounds.lower(dim) + 0.5 * alpha * innerIt * innerIt + (beta - 0.5 * alpha) * innerIt),
          IR_IfCondition(IR_LowerEqual(innerIt, xs + 1),
            IR_Assignment(Duplicate(baseAccess),
              domainBounds.lower(dim) - 0.5 * alpha * (xf * xf + xf) + (beta + alpha * xf) * innerIt),
            IR_Assignment(Duplicate(baseAccess),
              domainBounds.lower(dim) - 0.5 * alpha * innerIt * innerIt
                + (alpha * xf + alpha * xs + 0.5 * alpha + beta) * innerIt
                - 0.5 * alpha * (xf * xf + xf + xs * xs + xs))))))
    innerLoop.parallelization.potentiallyParallel = false

    ListBuffer[IR_Statement](
      IR_LoopOverFragments(ListBuffer[IR_Statement](
        innerLoop,
        leftBoundaryUpdate,
        rightBoundaryUpdate)))
  }

  def for_AA_Diego(level : Int, dim : Int) : ListBuffer[IR_Statement] = {
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to global domain")

    val domain = IR_DomainCollection.getByIdentifier("global").get
    val numDims = domain.numDims

    val expo = 1.5
    val numCells = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim) // number of cells per fragment
    val zoneSize = numCells / 4
    val step = 1.0 / zoneSize

    val zoneLength = 0.0095 //* 8 / zoneSize

    val field = IR_VF_NodePositionPerDim.find(level, dim).associatedField
    val baseIndex = IR_LoopOverDimensions.defIt(numDims)
    val baseAccess = IR_FieldAccess(field, 0, baseIndex)

    val innerIt = IR_LoopOverDimensions.defItForDim(dim)

    val leftGhostIndex = IR_ExpressionIndex(Array.fill(numDims)(0))
    leftGhostIndex(dim) = -1
    val leftGhostAccess = IR_FieldAccess(field, 0, leftGhostIndex)
    val rightGhostIndex = IR_ExpressionIndex(Array.fill(numDims)(0))
    rightGhostIndex(dim) = numCells + 1
    val rightGhostAccess = IR_FieldAccess(field, 0, rightGhostIndex)

    // TODO: fix loop offsets -> no duplicate layers - don't generate iterationOffset loop bounds

    val innerLoop = IR_LoopOverPoints(field, None,
      IR_GridUtil.offsetIndex(IR_ExpressionIndex(Array.fill(numDims)(0)), -1, dim),
      IR_GridUtil.offsetIndex(IR_ExpressionIndex(Array.fill(numDims)(0)), -1, dim),
      IR_ExpressionIndex(Array.fill(numDims)(1)),
      ListBuffer[IR_Statement](
        IR_IfCondition(IR_LowerEqual(innerIt, 0),
          IR_Assignment(Duplicate(baseAccess), 0.0),
          IR_IfCondition(IR_LowerEqual(innerIt, 1 * zoneSize),
            IR_Assignment(Duplicate(baseAccess), IR_GridUtil.offsetAccess(baseAccess, -1 * innerIt + 0 * zoneSize, dim)
              + zoneLength * IR_FunctionCall(IR_MathFunctionReference.pow, ListBuffer[IR_Expression](step * (IR_LoopOverDimensions.defItForDim(dim) - 0.0 * zoneSize), expo))),
            IR_IfCondition(IR_LowerEqual(innerIt, 2 * zoneSize),
              IR_Assignment(Duplicate(baseAccess), IR_GridUtil.offsetAccess(baseAccess, -1 * innerIt + 1 * zoneSize, dim)
                + zoneLength * step * (IR_LoopOverDimensions.defItForDim(dim) - 1.0 * zoneSize)),
              IR_IfCondition(IR_LowerEqual(innerIt, 3 * zoneSize),
                IR_Assignment(Duplicate(baseAccess), IR_GridUtil.offsetAccess(baseAccess, -1 * innerIt + 2 * zoneSize, dim)
                  + zoneLength * step * (IR_LoopOverDimensions.defItForDim(dim) - 2.0 * zoneSize)),
                IR_IfCondition(IR_LowerEqual(innerIt, 4 * zoneSize),
                  IR_Assignment(Duplicate(baseAccess), IR_GridUtil.offsetAccess(baseAccess, -1 * innerIt + 3 * zoneSize, dim)
                    + zoneLength * (1.0 - IR_FunctionCall(IR_MathFunctionReference.pow, ListBuffer[IR_Expression](1.0 - step * (IR_LoopOverDimensions.defItForDim(dim) - 3.0 * zoneSize), expo)))),
                  IR_Assignment(Duplicate(baseAccess), IR_GridUtil.offsetAccess(baseAccess, -1, dim)))))))))
    innerLoop.parallelization.potentiallyParallel = false

    ListBuffer(
      innerLoop,
      IR_Assignment(Duplicate(leftGhostAccess),
        2 * IR_GridUtil.offsetAccess(leftGhostAccess, 1, dim) - IR_GridUtil.offsetAccess(leftGhostAccess, 2, dim)),
      IR_Assignment(Duplicate(rightGhostAccess),
        2 * IR_GridUtil.offsetAccess(rightGhostAccess, -1, dim) - IR_GridUtil.offsetAccess(rightGhostAccess, -2, dim)))
  }

  def for_AA_Diego2(level : Int, dim : Int) : ListBuffer[IR_Statement] = {
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to global domain")

    val domain = IR_DomainCollection.getByIdentifier("global").get
    val numDims = domain.numDims

    // virtually the same as setupNodePos_Diego but with only three zones -> replicates the new test cases
    val expo = 1.5
    val numCells = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim) // number of cells per fragment
    val zoneSize_1 : Int = numCells / 3
    val zoneSize_3 : Int = numCells / 3
    val zoneSize_2 : Int = numCells - (zoneSize_1 + zoneSize_3)
    val step_1 = 1.0 / zoneSize_1
    val step_2 = 1.0 / zoneSize_2
    val step_3 = 1.0 / zoneSize_3

    val zoneLength_1 = 0.012 // * 8 / zoneSize_1
    val zoneLength_2 = 0.014 // * 8 / zoneSize_2
    val zoneLength_3 = 0.012 // * 8 / zoneSize_3

    val field = IR_VF_NodePositionPerDim.find(level, dim).associatedField
    val baseIndex = IR_LoopOverDimensions.defIt(numDims)
    val baseAccess = IR_FieldAccess(field, 0, baseIndex)

    val innerIt = IR_LoopOverDimensions.defItForDim(dim)

    val leftGhostIndex = IR_ExpressionIndex(Array.fill(numDims)(0))
    leftGhostIndex(dim) = -1
    val leftGhostAccess = IR_FieldAccess(field, 0, leftGhostIndex)
    val rightGhostIndex = IR_ExpressionIndex(Array.fill(numDims)(0))
    rightGhostIndex(dim) = numCells + 1
    val rightGhostAccess = IR_FieldAccess(field, 0, rightGhostIndex)

    // TODO: fix loop offsets -> no duplicate layers - don't generate iterationOffset loop bounds

    val innerLoop = IR_LoopOverPoints(field, None,
      IR_GridUtil.offsetIndex(IR_ExpressionIndex(Array.fill(numDims)(0)), -1, dim),
      IR_GridUtil.offsetIndex(IR_ExpressionIndex(Array.fill(numDims)(0)), -1, dim),
      IR_ExpressionIndex(Array.fill(numDims)(1)),
      ListBuffer[IR_Statement](
        IR_IfCondition(IR_LowerEqual(innerIt, 0),
          IR_Assignment(Duplicate(baseAccess), 0.0),
          IR_IfCondition(IR_LowerEqual(innerIt, zoneSize_1),
            IR_Assignment(Duplicate(baseAccess), IR_GridUtil.offsetAccess(baseAccess, -1 * innerIt, dim)
              + zoneLength_1 * IR_FunctionCall(IR_MathFunctionReference.pow, ListBuffer[IR_Expression](step_1 * IR_LoopOverDimensions.defItForDim(dim), expo))),
            IR_IfCondition(IR_LowerEqual(innerIt, zoneSize_1 + zoneSize_2),
              IR_Assignment(Duplicate(baseAccess), IR_GridUtil.offsetAccess(baseAccess, -1 * innerIt + zoneSize_1, dim)
                + zoneLength_2 * step_2 * (IR_LoopOverDimensions.defItForDim(dim) - zoneSize_1)),
              IR_IfCondition(IR_LowerEqual(innerIt, innerIt + (zoneSize_1 + zoneSize_2 + zoneSize_3)),
                IR_Assignment(Duplicate(baseAccess), IR_GridUtil.offsetAccess(baseAccess, -1 * innerIt + (zoneSize_1 + zoneSize_2), dim)
                  + zoneLength_3 * step_3 * (IR_LoopOverDimensions.defItForDim(dim) - (zoneSize_1 + zoneSize_2))),
                IR_Assignment(Duplicate(baseAccess), IR_GridUtil.offsetAccess(baseAccess, -1, dim))))))))
    innerLoop.parallelization.potentiallyParallel = false

    ListBuffer(
      innerLoop,
      IR_Assignment(Duplicate(leftGhostAccess),
        2 * IR_GridUtil.offsetAccess(leftGhostAccess, 1, dim) - IR_GridUtil.offsetAccess(leftGhostAccess, 2, dim)),
      IR_Assignment(Duplicate(rightGhostAccess),
        2 * IR_GridUtil.offsetAccess(rightGhostAccess, -1, dim) - IR_GridUtil.offsetAccess(rightGhostAccess, -2, dim)))
  }

  def for_nonAA_Uniform(level : Int) : ListBuffer[IR_Statement] = {
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to global domain")

    val domain = IR_DomainCollection.getByIdentifier("global").get
    val numDims = domain.numDims

    val stmts = ListBuffer[IR_Statement]()

    for (dim <- 0 until numDims) {
      val numCellsPerFrag = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim)
      val numCellsTotal = numCellsPerFrag * Knowledge.domain_rect_numFragsTotalAsVec(dim)

      // fix grid width to match domain size
      val domainBounds = domain.asInstanceOf[IR_DomainFromAABB].aabb
      val cellWidth = IR_SimplifyExpression.evalFloating((domainBounds.upper(dim) - domainBounds.lower(dim)) / numCellsTotal)

      // look up field and compile access to base element
      val field = IR_VF_NodePositionAsVec.find(level).associatedField
      val baseIndex = IR_LoopOverDimensions.defIt(numDims)
      baseIndex.indices ++= Array[IR_Expression](dim, 0)
      val baseAccess = IR_FieldAccess(field, 0, baseIndex)

      // fix the inner iterator -> used for zone checks
      def innerIt =
        if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
          IR_LoopOverDimensions.defItForDim(dim)
        else
          IR_VariableAccess(s"global_i$dim", IR_IntegerDatatype)

      val innerItDecl =
        if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
          IR_NullStatement
        else
          IR_VariableDeclaration(innerIt, IR_LoopOverDimensions.defItForDim(dim) + IR_IV_FragmentIndex(dim) * numCellsPerFrag)

      // compile final loop
      val innerLoop = IR_LoopOverPoints(field, None,
        IR_ExpressionIndex(Array.fill(numDims)(-2)),
        IR_ExpressionIndex(Array.fill(numDims)(-2)),
        IR_ExpressionIndex(Array.fill(numDims)(1)),
        ListBuffer[IR_Statement](
          innerItDecl,
          IR_Assignment(Duplicate(baseAccess),
            domainBounds.lower(dim) + innerIt * cellWidth))
      )
      innerLoop.parallelization.potentiallyParallel = false

      stmts += innerLoop
    }

    stmts
  }

  def for_nonAA_Random(level : Int) : ListBuffer[IR_Statement] = {
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to global domain")

    val domain = IR_DomainCollection.getByIdentifier("global").get
    val numDims = domain.numDims

    val stmts = ListBuffer[IR_Statement]()

    // init with uniform first
    stmts ++= for_nonAA_Uniform(level)

    // add randn function
    if (level == Knowledge.maxLevel) {
      val dim = 0 // FIXME: which dim?
      val numCellsTotal = ((1 << level) * Knowledge.domain_fragmentLengthAsVec(dim)) * Knowledge.domain_rect_numFragsTotalAsVec(dim)
      val domainBounds = domain.asInstanceOf[IR_DomainFromAABB].aabb
      val defGridWidth = IR_SimplifyExpression.evalFloating((domainBounds.upper(dim) - domainBounds.lower(dim)) / numCellsTotal)

      Settings.additionalIncludes = (Settings.additionalIncludes :+ "functional" :+ "random").distinct

      stmts += IR_Native(s"std::default_random_engine generator(${ if (Knowledge.mpi_enabled) MPI_IV_MpiRank.prettyprint() else 0 })")
      stmts += IR_Native(s"static std::uniform_real_distribution <double> distribution(${ -Knowledge.experimental_grid_randomMaxOffset * defGridWidth }, ${ Knowledge.experimental_grid_randomMaxOffset * defGridWidth })")
      stmts += IR_Native(s"static auto randn = std::bind (distribution, generator)")
    }

    // apply modification of positions
    // look up field and compile access to base element
    val field = IR_VF_NodePositionAsVec.find(level).associatedField

    def baseAccess(f : IR_Field, dim : Int) = {
      val baseIndex = IR_LoopOverDimensions.defIt(numDims)
      baseIndex.indices ++= Array[IR_Expression](dim, 0)
      IR_FieldAccess(f, 0, baseIndex)
    }

    if (level == Knowledge.maxLevel) {
      // on finest level: add random offset
      var fieldLoopStmts = ListBuffer[IR_Statement]()
      for (dim <- 0 until numDims)
        fieldLoopStmts += IR_Assignment(baseAccess(field, dim), IR_FunctionCall("randn"), "+=")

      // create fused field loop with updates for all dims
      val innerLoop = IR_LoopOverPoints(field, fieldLoopStmts)
      innerLoop.parallelization.potentiallyParallel = false
      stmts += innerLoop

      // communication
      stmts += IR_Communicate(field, 0, "both", ListBuffer(IR_CommunicateTarget("all", None, None)), None, "")
    } else {
      val finerField = IR_VF_NodePositionAsVec.find(level + 1).associatedField

      // on all levels but the finest: inject positions from finer level
      var fieldLoopStmts = ListBuffer[IR_Statement]()
      for (dim <- 0 until numDims)
        fieldLoopStmts += IR_Assignment(baseAccess(field, dim), baseAccess(finerField, dim))

      stmts += IR_LoopOverPoints(field, fieldLoopStmts)
    }

    stmts
  }

  def for_AA_restrictFromFiner(level : Int, dim : Int) = {
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to global domain")

    val domain = IR_DomainCollection.getByIdentifier("global").get
    val numDims = domain.numDims

    val coarseLvl = level
    val fineLvl = coarseLvl + 1

    val coarseCellsPerFrag = (1 << coarseLvl) * Knowledge.domain_fragmentLengthAsVec(dim)

    // look up field and compile access to base element
    val coarseField = IR_VF_NodePositionPerDim.find(coarseLvl, dim).associatedField
    val coarseIndex = IR_LoopOverDimensions.defIt(numDims)
    val coarseAccess = IR_FieldAccess(coarseField, 0, coarseIndex)
    val fineField = IR_VF_NodePositionPerDim.find(fineLvl, dim).associatedField
    val fineIndex = IR_LoopOverDimensions.defIt(numDims)
    val fineAccess = IR_FieldAccess(fineField, 0, fineIndex)

    // compile special boundary handling expressions
    val leftDir = Array.fill(numDims)(0)
    leftDir(dim) = -1
    val leftNeighIndex = DefaultNeighbors.getNeigh(leftDir).index

    val leftGhostIndex = IR_ExpressionIndex(Array.fill(numDims)(0))
    leftGhostIndex(dim) = -2
    val leftGhostAccess = IR_FieldAccess(coarseField, 0, leftGhostIndex)

    val leftBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(coarseField.domain.index, leftNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(IR_GridUtil.offsetAccess(leftGhostAccess, 1, dim),
          2 * IR_GridUtil.offsetAccess(leftGhostAccess, 2, dim) - IR_GridUtil.offsetAccess(leftGhostAccess, 3, dim)),
        IR_Assignment(Duplicate(leftGhostAccess),
          2 * IR_GridUtil.offsetAccess(leftGhostAccess, 1, dim) - IR_GridUtil.offsetAccess(leftGhostAccess, 2, dim))))

    val rightDir = Array.fill(numDims)(0)
    rightDir(dim) = 1
    val rightNeighIndex = DefaultNeighbors.getNeigh(rightDir).index

    val rightGhostIndex = IR_ExpressionIndex(Array.fill(numDims)(0))
    rightGhostIndex(dim) = coarseCellsPerFrag + 2
    val rightGhostAccess = IR_FieldAccess(coarseField, 0, rightGhostIndex)

    val rightBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(coarseField.domain.index, rightNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(IR_GridUtil.offsetAccess(rightGhostAccess, -1, dim),
          2 * IR_GridUtil.offsetAccess(rightGhostAccess, -2, dim) - IR_GridUtil.offsetAccess(rightGhostAccess, -3, dim)),
        IR_Assignment(Duplicate(rightGhostAccess),
          2 * IR_GridUtil.offsetAccess(rightGhostAccess, -1, dim) - IR_GridUtil.offsetAccess(rightGhostAccess, -2, dim))))

    // compile final loop
    val offset = -1

    //val offset = -2
    def innerIt = IR_LoopOverDimensions.defItForDim(dim)

    val innerLoop = IR_LoopOverPoints(coarseField, None,
      IR_GridUtil.offsetIndex(IR_ExpressionIndex(Array.fill(numDims)(0)), offset, dim),
      IR_GridUtil.offsetIndex(IR_ExpressionIndex(Array.fill(numDims)(0)), offset, dim),
      IR_ExpressionIndex(Array.fill(numDims)(1)),
      ListBuffer[IR_Statement](
        // simple injection strategy
        IR_Assignment(Duplicate(coarseAccess), Duplicate(fineAccess))))

    innerLoop.parallelization.potentiallyParallel = false

    ListBuffer[IR_Statement](
      IR_LoopOverFragments(ListBuffer[IR_Statement](
        innerLoop,
        leftBoundaryUpdate,
        rightBoundaryUpdate)),
      IR_Communicate(coarseField, 0, "both", ListBuffer(IR_CommunicateTarget("ghost", None, None)), None, ""))
  }
}
