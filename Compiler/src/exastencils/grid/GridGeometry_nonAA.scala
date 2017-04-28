package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.baseExt.l4.L4_VectorDatatype
import exastencils.boundary.l4.L4_NoBC
import exastencils.communication.DefaultNeighbors
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.deprecated.domain.RectangularDomain
import exastencils.deprecated.ir._
import exastencils.domain.AABB
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.field.l4._
import exastencils.logger.Logger

/// GridGeometry_nonAA

object GridGeometry_nonAA extends GridGeometry {
  // direct accesses
  override def nodePosAsVec(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int]) = {
    val field = IR_FieldCollection.getByIdentifierLevExp("node_pos", level).get
    val newIndex = Duplicate(index)
    IR_FieldAccess(IR_FieldSelection(field, field.level, 0), newIndex)
  }

  // direct accesses
  override def nodePosition(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = IR_FieldCollection.getByIdentifierLevExp("node_pos", level).get
    val newIndex = Duplicate(index)
    // extra index for matrix expression
    newIndex.indices ++= Array[IR_Expression](dim, 0)
    IR_FieldAccess(IR_FieldSelection(field, field.level, 0), newIndex)
  }

  override def cellCenter(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = ???

  override def cellWidth(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = ???

  // injection of  missing l4 information for virtual fields and generation of setup code
  override def initL4() : Unit = {
    val vecType = L4_VectorDatatype(L4_RealDatatype, Knowledge.dimensionality)
    val layoutName = s"NodePosVec${ Knowledge.dimensionality }Data"

    ExaRootNode.l4_root.nodes += L4_FieldLayoutDecl(
      L4_LeveledIdentifier(layoutName, L4_AllLevels),
      vecType, "node",
      ListBuffer(
        L4_FieldLayoutOption("ghostLayers", L4_ConstIndex(Array.fill(Knowledge.dimensionality)(2)), true),
        L4_FieldLayoutOption("duplicateLayers", L4_ConstIndex(Array.fill(Knowledge.dimensionality)(1)), true)))

    ExaRootNode.l4_root.nodes += L4_FieldDecl(
      L4_LeveledIdentifier("node_pos", L4_AllLevels), "global", layoutName, L4_NoBC, 1, 0)
  }

  override def generateInitCode() : ListBuffer[IR_Statement] = {
    Knowledge.grid_spacingModel match {
      case "uniform" =>
        (Knowledge.maxLevel to Knowledge.minLevel by -1).map(level =>
          Knowledge.dimensions.to[ListBuffer].flatMap(dim => setupNodePos_Uniform(dim, level)))
          .reduceLeft(_ ++ _)
//      case "linearFct" =>
//        Logger.warn("LinearFct spacing model is currently not recommended for GridGeometry_nonUniform_nonStaggered_AA since grid point positions won't match across levels")
//        (Knowledge.maxLevel to Knowledge.minLevel by -1).map(level =>
//          Knowledge.dimensions.to[ListBuffer].flatMap(dim => setupNodePos_LinearFct(dim, level)))
//          .reduceLeft(_ ++ _)
    }
  }

  def HACK_numDims = Knowledge.dimensionality // TODO: fix dim

  def setupNodePos_Uniform(dim : Int, level : Int) : ListBuffer[IR_Statement] = {
    val numCellsPerFrag = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim)
    val numCellsTotal = numCellsPerFrag * Knowledge.domain_rect_numFragsTotalAsVec(dim)

    // fix grid width to match domain size
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to the first domain")
    val domainBounds = IR_DomainCollection.objects(0).asInstanceOf[RectangularDomain].shape.shapeData.asInstanceOf[AABB]
    val cellWidth = (domainBounds.upper(dim) - domainBounds.lower(dim)) / numCellsTotal

    // look up field and compile access to base element
    val field = IR_FieldCollection.getByIdentifier(s"node_pos", level).get
    val baseIndex = IR_LoopOverDimensions.defIt(HACK_numDims)
    baseIndex.indices ++= Array[IR_Expression](dim, 0)
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
    val leftDir = Array.fill(HACK_numDims)(0)
    leftDir(dim) = -1
    val leftNeighIndex = DefaultNeighbors.getNeigh(leftDir).index

    val leftGhostIndex = IR_ExpressionIndex(Array.fill(HACK_numDims + 2)(0))
    leftGhostIndex(dim) = -2
    val leftGhostAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), leftGhostIndex)

    val leftBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(field.domain.index, leftNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(leftGhostAccess, 1, dim),
          2 * GridUtil.offsetAccess(leftGhostAccess, 2, dim) - GridUtil.offsetAccess(leftGhostAccess, 3, dim)),
        IR_Assignment(Duplicate(leftGhostAccess),
          2 * GridUtil.offsetAccess(leftGhostAccess, 1, dim) - GridUtil.offsetAccess(leftGhostAccess, 2, dim))))

    val rightDir = Array.fill(HACK_numDims)(0)
    rightDir(dim) = 1
    val rightNeighIndex = DefaultNeighbors.getNeigh(rightDir).index

    val rightGhostIndex = IR_ExpressionIndex(Array.fill(HACK_numDims + 2)(0))
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
      IR_ExpressionIndex(Array.fill(HACK_numDims)(-2)),
      IR_ExpressionIndex(Array.fill(HACK_numDims)(-2)),
      IR_ExpressionIndex(1, 1, 1),
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
}
