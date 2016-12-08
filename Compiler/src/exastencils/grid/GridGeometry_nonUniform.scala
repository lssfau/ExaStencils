package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.boundary.l4.L4_NoBC
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir._
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.deprecated.domain._
import exastencils.deprecated.ir._
import exastencils.domain._
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.field.l4._
import exastencils.logger._

/// GridGeometry_nonUniform

trait GridGeometry_nonUniform extends GridGeometry {
  // direct accesses
  override def nodePosition(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = IR_FieldCollection.getByIdentifierLevExp(s"node_pos_${ IR_DimToString(dim) }", level).get
    IR_FieldAccess(IR_FieldSelection(field, field.level, 0, arrayIndex), GridUtil.projectIdx(index, dim))
  }

  // compound accesses
  override def cellCenter(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) = {
    0.5 * (nodePosition(level, GridUtil.offsetIndex(index, 1, dim), arrayIndex, dim) + nodePosition(level, Duplicate(index), arrayIndex, dim))
  }

  override def cellWidth(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) = {
    nodePosition(level, GridUtil.offsetIndex(index, 1, dim), arrayIndex, dim) - nodePosition(level, Duplicate(index), arrayIndex, dim)
  }

  // injection of  missing l4 information for virtual fields and generation of setup code
  override def initL4() = {
    for (lvl <- Knowledge.levels) {
      ExaRootNode.l4_root.nodes += L4_FieldLayoutDecl(
        L4_LeveledIdentifier("DefNodeLineLayout_x", L4_SingleLevel(lvl)),
        L4_RealDatatype, "Edge_Node".toLowerCase(),
        ListBuffer(
          L4_FieldLayoutOption("ghostLayers", L4_ConstIndex(2, 0, 0), true),
          L4_FieldLayoutOption("duplicateLayers", L4_ConstIndex(1, 0, 0), true),
          L4_FieldLayoutOption("innerPoints", L4_ConstIndex((1 << lvl) * Knowledge.domain_fragmentLength_x - 1, 1, 1), false)))
      ExaRootNode.l4_root.nodes += L4_FieldLayoutDecl(
        L4_LeveledIdentifier("DefNodeLineLayout_y", L4_SingleLevel(lvl)),
        L4_RealDatatype, "Edge_Node".toLowerCase(),
        ListBuffer(
          L4_FieldLayoutOption("ghostLayers", L4_ConstIndex(0, 2, 0), true),
          L4_FieldLayoutOption("duplicateLayers", L4_ConstIndex(0, 1, 0), true),
          L4_FieldLayoutOption("innerPoints", L4_ConstIndex(1, (1 << lvl) * Knowledge.domain_fragmentLength_y - 1, 1), false)))
      ExaRootNode.l4_root.nodes += L4_FieldLayoutDecl(
        L4_LeveledIdentifier("DefNodeLineLayout_z", L4_SingleLevel(lvl)),
        L4_RealDatatype, "Edge_Node".toLowerCase(),
        ListBuffer(
          L4_FieldLayoutOption("ghostLayers", L4_ConstIndex(0, 0, 2), true),
          L4_FieldLayoutOption("duplicateLayers", L4_ConstIndex(0, 0, 1), true),
          L4_FieldLayoutOption("innerPoints", L4_ConstIndex(1, 1, (1 << lvl) * Knowledge.domain_fragmentLength_z - 1), false)))
    }

    ExaRootNode.l4_root.nodes += L4_FieldDecl(
      L4_LeveledIdentifier("node_pos_x", L4_AllLevels), "global", "DefNodeLineLayout_x", L4_NoBC, 1, 0)
    if (Knowledge.dimensionality > 1)
      ExaRootNode.l4_root.nodes += L4_FieldDecl(
        L4_LeveledIdentifier("node_pos_y", L4_AllLevels), "global", "DefNodeLineLayout_y", L4_NoBC, 1, 0)
    if (Knowledge.dimensionality > 2)
      ExaRootNode.l4_root.nodes += L4_FieldDecl(
        L4_LeveledIdentifier("node_pos_z", L4_AllLevels), "global", "DefNodeLineLayout_z", L4_NoBC, 1, 0)
  }

  def setupNodePos_Uniform(dim : Int, level : Int) : ListBuffer[IR_Statement] = {
    val numCellsPerFrag = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim)
    val numCellsTotal = numCellsPerFrag * Knowledge.domain_rect_numFragsTotalAsVec(dim)

    // fix grid width to match domain size
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to the first domain")
    val domainBounds = IR_DomainCollection.objects(0).asInstanceOf[RectangularDomain].shape.shapeData.asInstanceOf[AABB]
    val cellWidth = (domainBounds.upper(dim) - domainBounds.lower(dim)) / numCellsTotal

    // look up field and compile access to base element
    val field = IR_FieldCollection.getByIdentifier(s"node_pos_${ IR_DimToString(dim) }", level).get
    val baseIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim
    val baseAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), baseIndex)

    // fix the inner iterator -> used for zone checks
    def innerIt =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_LoopOverDimensions.defItForDim(dim)
      else
        IR_VariableAccess(s"global_${ IR_DimToString(dim) }", IR_IntegerDatatype)
    val innerItDecl =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_NullStatement
      else
        IR_VariableDeclaration(innerIt, IR_LoopOverDimensions.defItForDim(dim) + IR_IV_FragmentIndex(dim) * numCellsPerFrag)

    // compile special boundary handling expressions
    val leftDir = Array(0, 0, 0)
    leftDir(dim) = -1
    val leftNeighIndex = DefaultNeighbors.getNeigh(leftDir).index

    val leftGhostIndex = IR_ExpressionIndex(0, 0, 0, 0)
    leftGhostIndex(dim) = -2
    val leftGhostAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), leftGhostIndex)

    val leftBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(field.domain.index, leftNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(leftGhostAccess, 1, dim),
          2 * GridUtil.offsetAccess(leftGhostAccess, 2, dim) - GridUtil.offsetAccess(leftGhostAccess, 3, dim)),
        IR_Assignment(Duplicate(leftGhostAccess),
          2 * GridUtil.offsetAccess(leftGhostAccess, 1, dim) - GridUtil.offsetAccess(leftGhostAccess, 2, dim))))

    val rightDir = Array(0, 0, 0)
    rightDir(dim) = 1
    val rightNeighIndex = DefaultNeighbors.getNeigh(rightDir).index

    val rightGhostIndex = IR_ExpressionIndex(0, 0, 0, 0)
    rightGhostIndex(dim) = numCellsPerFrag + 2
    val rightGhostAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), rightGhostIndex)

    val rightBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(field.domain.index, rightNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(rightGhostAccess, -1, dim),
          2 * GridUtil.offsetAccess(rightGhostAccess, -2, dim) - GridUtil.offsetAccess(rightGhostAccess, -3, dim)),
        IR_Assignment(Duplicate(rightGhostAccess),
          2 * GridUtil.offsetAccess(rightGhostAccess, -1, dim) - GridUtil.offsetAccess(rightGhostAccess, -2, dim))))

    // compile final loop
    val innerLoop = IR_LoopOverPoints(field, None,
      GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -2, dim),
      GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -2, dim),
      IR_ExpressionIndex(1, 1, 1),
      ListBuffer[IR_Statement](
        innerItDecl,
        IR_Assignment(Duplicate(baseAccess),
          domainBounds.lower(dim) - innerIt * cellWidth))
    )
    innerLoop.parallelization.potentiallyParallel = false

    ListBuffer[IR_Statement](
      IR_LoopOverFragments(ListBuffer[IR_Statement](
        innerLoop,
        leftBoundaryUpdate,
        rightBoundaryUpdate)))
  }

  def setupNodePos_LinearFct(dim : Int, level : Int) : ListBuffer[IR_Statement] = {
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
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to the first domain")
    val domainBounds = IR_DomainCollection.objects(0).asInstanceOf[RectangularDomain].shape.shapeData.asInstanceOf[AABB]

    // simple approach: alpha and beta are equal -> results in very small volumes and aspect ratios if the number of points is high
    //    val alpha = domainSize / (lastPointAlphaCoeff + lastPointBetaCoeff)
    //    val beta = alpha

    // better approach: fix the ratio between smallest and largest cell width to 8
    val factor = (numCellsTotal / 4) / 8.0
    val alpha = (domainBounds.upper(dim) - domainBounds.lower(dim)) / (lastPointAlphaCoeff + lastPointBetaCoeff * factor)
    val beta = factor * alpha

    //Logger.debug(s"Using alpha $alpha and beta $beta")

    // look up field and compile access to base element
    val field = IR_FieldCollection.getByIdentifier(s"node_pos_${ IR_DimToString(dim) }", level).get
    def baseIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality)
    // TODO: dim
    def baseAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), baseIndex)

    // fix the inner iterator -> used for zone checks
    def innerIt =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_LoopOverDimensions.defItForDim(dim)
      else
        IR_VariableAccess(s"global_${ IR_DimToString(dim) }", IR_IntegerDatatype)
    val innerItDecl =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_NullStatement
      else
        IR_VariableDeclaration(innerIt, IR_LoopOverDimensions.defItForDim(dim) + IR_IV_FragmentIndex(dim) * numCellsPerFrag)

    // compile special boundary handling expressions
    val leftDir = Array(0, 0, 0)
    leftDir(dim) = -1
    val leftNeighIndex = DefaultNeighbors.getNeigh(leftDir).index

    val leftGhostIndex = IR_ExpressionIndex(0, 0, 0, 0)
    leftGhostIndex(dim) = -2
    def leftGhostAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), leftGhostIndex)

    val leftBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(field.domain.index, leftNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(leftGhostAccess, 1, dim),
          2 * GridUtil.offsetAccess(leftGhostAccess, 2, dim) - GridUtil.offsetAccess(leftGhostAccess, 3, dim)),
        IR_Assignment(Duplicate(leftGhostAccess),
          2 * GridUtil.offsetAccess(leftGhostAccess, 1, dim) - GridUtil.offsetAccess(leftGhostAccess, 2, dim))))

    val rightDir = Array(0, 0, 0)
    rightDir(dim) = 1
    val rightNeighIndex = DefaultNeighbors.getNeigh(rightDir).index

    val rightGhostIndex = IR_ExpressionIndex(0, 0, 0, 0)
    rightGhostIndex(dim) = numCellsPerFrag + 2
    def rightGhostAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), rightGhostIndex)

    val rightBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(field.domain.index, rightNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(rightGhostAccess, -1, dim),
          2 * GridUtil.offsetAccess(rightGhostAccess, -2, dim) - GridUtil.offsetAccess(rightGhostAccess, -3, dim)),
        IR_Assignment(Duplicate(rightGhostAccess),
          2 * GridUtil.offsetAccess(rightGhostAccess, -1, dim) - GridUtil.offsetAccess(rightGhostAccess, -2, dim))))

    // compile final loop
    val innerLoop = IR_LoopOverPoints(field, None,
      GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -2, dim),
      GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -2, dim),
      IR_ExpressionIndex(1, 1, 1),
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

  def restrictNodePos(dim : Int, coarseLvl : Int, fineLvl : Int) : ListBuffer[IR_Statement] = {
    val coarseCellsPerFrag = (1 << coarseLvl) * Knowledge.domain_fragmentLengthAsVec(dim)

    // look up field and compile access to base element
    val coarseField = IR_FieldCollection.getByIdentifier(s"node_pos_${ IR_DimToString(dim) }", coarseLvl).get
    val coarseIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim
    val coarseAccess = IR_FieldAccess(IR_FieldSelection(coarseField, coarseLvl, 0), coarseIndex)
    val fineField = IR_FieldCollection.getByIdentifier(s"node_pos_${ IR_DimToString(dim) }", fineLvl).get
    val fineIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim
    val fineAccess = IR_FieldAccess(IR_FieldSelection(fineField, fineLvl, 0), fineIndex)

    // compile special boundary handling expressions
    val leftDir = Array(0, 0, 0)
    leftDir(dim) = -1
    val leftNeighIndex = DefaultNeighbors.getNeigh(leftDir).index

    val leftGhostIndex = IR_ExpressionIndex(0, 0, 0, 0)
    leftGhostIndex(dim) = -2
    val leftGhostAccess = IR_FieldAccess(IR_FieldSelection(coarseField, coarseLvl, 0), leftGhostIndex)

    val leftBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(coarseField.domain.index, leftNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(leftGhostAccess, 1, dim),
          2 * GridUtil.offsetAccess(leftGhostAccess, 2, dim) - GridUtil.offsetAccess(leftGhostAccess, 3, dim)),
        IR_Assignment(Duplicate(leftGhostAccess),
          2 * GridUtil.offsetAccess(leftGhostAccess, 1, dim) - GridUtil.offsetAccess(leftGhostAccess, 2, dim))))

    val rightDir = Array(0, 0, 0)
    rightDir(dim) = 1
    val rightNeighIndex = DefaultNeighbors.getNeigh(rightDir).index

    val rightGhostIndex = IR_ExpressionIndex(0, 0, 0, 0)
    rightGhostIndex(dim) = coarseCellsPerFrag + 2
    val rightGhostAccess = IR_FieldAccess(IR_FieldSelection(coarseField, coarseLvl, 0), rightGhostIndex)

    val rightBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(coarseField.domain.index, rightNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(rightGhostAccess, -1, dim),
          2 * GridUtil.offsetAccess(rightGhostAccess, -2, dim) - GridUtil.offsetAccess(rightGhostAccess, -3, dim)),
        IR_Assignment(Duplicate(rightGhostAccess),
          2 * GridUtil.offsetAccess(rightGhostAccess, -1, dim) - GridUtil.offsetAccess(rightGhostAccess, -2, dim))))

    // compile final loop
    val offset = -1
    //val offset = -2
    def innerIt = IR_LoopOverDimensions.defItForDim(dim)
    val innerLoop = IR_LoopOverPoints(coarseField, None,
      GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), offset, dim),
      GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), offset, dim),
      IR_ExpressionIndex(1, 1, 1),
      ListBuffer[IR_Statement](
        // simple injection strategy
        IR_Assignment(Duplicate(coarseAccess), Duplicate(fineAccess))))

    innerLoop.parallelization.potentiallyParallel = false

    ListBuffer[IR_Statement](
      IR_LoopOverFragments(ListBuffer[IR_Statement](
        innerLoop,
        leftBoundaryUpdate,
        rightBoundaryUpdate)),
      IR_Communicate(IR_FieldSelection(coarseField, coarseLvl, 0), "both", ListBuffer(IR_CommunicateTarget("ghost", None, None)), None))
  }
}

