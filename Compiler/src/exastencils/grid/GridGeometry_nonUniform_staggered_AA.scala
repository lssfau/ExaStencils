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
import exastencils.deprecated.ir._
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.field.l4._

/// GridGeometry_nonUniform_staggered_AA

object GridGeometry_nonUniform_staggered_AA extends GridGeometry_nonUniform with GridGeometry_staggered {
  // direct accesses
  override def stagCVWidth(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = IR_FieldCollection.getByIdentifierLevExp(s"stag_cv_width_${ IR_DimToString(dim) }", level).get
    IR_FieldAccess(IR_FieldSelection(field, field.level, 0, arrayIndex), GridUtil.projectIdx(index, dim))
  }

  // injection of  missing l4 information for virtual fields and generation of setup code
  override def initL4() = {
    super.initL4() // set basic stuff

    // extend with info required by staggered grid
    ExaRootNode.l4_root.nodes += L4_FieldDecl(
      L4_LeveledIdentifier("stag_cv_width_x", L4_AllLevels), "global", "DefNodeLineLayout_x", L4_NoBC, 1, 0)
    if (Knowledge.dimensionality > 1)
      ExaRootNode.l4_root.nodes += L4_FieldDecl(
        L4_LeveledIdentifier("stag_cv_width_y", L4_AllLevels), "global", "DefNodeLineLayout_y", L4_NoBC, 1, 0)
    if (Knowledge.dimensionality > 2)
      ExaRootNode.l4_root.nodes += L4_FieldDecl(
        L4_LeveledIdentifier("stag_cv_width_z", L4_AllLevels), "global", "DefNodeLineLayout_z", L4_NoBC, 1, 0)
  }

  override def generateInitCode() = {
    /// node_pos        -> nodes of the original grid
    /// o   o   o   o   o
    /// cell_width      -> width of the control volumes of the original grid
    /// |---|   |---|
    /// stag_cv_width   -> width of the staggered control volumes
    /// |-|   |---|   |-|

    Knowledge.grid_spacingModel match {
      case "diego"     =>
        Knowledge.dimensions.to[ListBuffer].flatMap(dim => setupNodePos_Diego(dim, Knowledge.maxLevel)) ++
          Knowledge.dimensions.to[ListBuffer].flatMap(dim => setupStagCVWidth(dim, Knowledge.maxLevel))
      case "diego2"    =>
        Knowledge.dimensions.to[ListBuffer].flatMap(dim => setupNodePos_Diego2(dim, Knowledge.maxLevel)) ++
          Knowledge.dimensions.to[ListBuffer].flatMap(dim => setupStagCVWidth(dim, Knowledge.maxLevel))
      case "linearFct" =>
        val stmts = ListBuffer[IR_Statement]()
        stmts ++= Knowledge.dimensions.to[ListBuffer].flatMap(dim => setupNodePos_LinearFct(dim, Knowledge.maxLevel))
        stmts ++= Knowledge.dimensions.to[ListBuffer].flatMap(dim => setupStagCVWidth(dim, Knowledge.maxLevel))

        if (true)
          for (lvl <- Knowledge.maxLevel - 1 to Knowledge.minLevel by -1) {
            stmts ++= Knowledge.dimensions.to[ListBuffer].flatMap(dim => restrictNodePos(dim, lvl, lvl + 1))
            stmts ++= Knowledge.dimensions.to[ListBuffer].flatMap(dim => setupStagCVWidth(dim, lvl))
          }

        stmts
    }
  }

  def setupNodePos_Diego(dim : Int, level : Int) : ListBuffer[IR_Statement] = {
    val expo = 1.5
    val numCells = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim) // number of cells per fragment
    val zoneSize = numCells / 4
    val step = 1.0 / zoneSize

    val zoneLength = 0.0095 //* 8 / zoneSize

    val field = IR_FieldCollection.getByIdentifier(s"node_pos_${ IR_DimToString(dim) }", level).get
    val baseIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim
    val baseAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), baseIndex)

    val innerIt = IR_LoopOverDimensions.defItForDim(dim)

    val leftGhostIndex = IR_ExpressionIndex(0, 0, 0, 0)
    leftGhostIndex(dim) = -1
    val leftGhostAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), leftGhostIndex)
    val rightGhostIndex = IR_ExpressionIndex(0, 0, 0, 0)
    rightGhostIndex(dim) = numCells + 1
    val rightGhostAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), rightGhostIndex)

    // TODO: fix loop offsets -> no duplicate layers - don't generate iterationOffset loop bounds

    val innerLoop = IR_LoopOverPoints(field, None,
      GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -1, dim),
      GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -1, dim),
      IR_ExpressionIndex(1, 1, 1),
      ListBuffer[IR_Statement](
        IR_IfCondition(IR_LowerEqual(innerIt, 0),
          IR_Assignment(Duplicate(baseAccess), 0.0),
          IR_IfCondition(IR_LowerEqual(innerIt, 1 * zoneSize),
            IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + 0 * zoneSize, dim)
              + zoneLength * IR_FunctionCall("pow", ListBuffer[IR_Expression](step * (IR_LoopOverDimensions.defItForDim(dim) - 0.0 * zoneSize), expo))),
            IR_IfCondition(IR_LowerEqual(innerIt, 2 * zoneSize),
              IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + 1 * zoneSize, dim)
                + zoneLength * step * (IR_LoopOverDimensions.defItForDim(dim) - 1.0 * zoneSize)),
              IR_IfCondition(IR_LowerEqual(innerIt, 3 * zoneSize),
                IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + 2 * zoneSize, dim)
                  + zoneLength * step * (IR_LoopOverDimensions.defItForDim(dim) - 2.0 * zoneSize)),
                IR_IfCondition(IR_LowerEqual(innerIt, 4 * zoneSize),
                  IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + 3 * zoneSize, dim)
                    + zoneLength * (1.0 - IR_FunctionCall("pow", ListBuffer[IR_Expression](1.0 - step * (IR_LoopOverDimensions.defItForDim(dim) - 3.0 * zoneSize), expo)))),
                  IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1, dim)))))))))
    innerLoop.parallelization.potentiallyParallel = false

    ListBuffer(
      innerLoop,
      IR_Assignment(Duplicate(leftGhostAccess),
        2 * GridUtil.offsetAccess(leftGhostAccess, 1, dim) - GridUtil.offsetAccess(leftGhostAccess, 2, dim)),
      IR_Assignment(Duplicate(rightGhostAccess),
        2 * GridUtil.offsetAccess(rightGhostAccess, -1, dim) - GridUtil.offsetAccess(rightGhostAccess, -2, dim)))
  }

  def setupNodePos_Diego2(dim : Int, level : Int) : ListBuffer[IR_Statement] = {
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

    val field = IR_FieldCollection.getByIdentifier(s"node_pos_${ IR_DimToString(dim) }", level).get
    val baseIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim
    val baseAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), baseIndex)

    val innerIt = IR_LoopOverDimensions.defItForDim(dim)

    val leftGhostIndex = IR_ExpressionIndex(0, 0, 0, 0)
    leftGhostIndex(dim) = -1
    val leftGhostAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), leftGhostIndex)
    val rightGhostIndex = IR_ExpressionIndex(0, 0, 0, 0)
    rightGhostIndex(dim) = numCells + 1
    val rightGhostAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), rightGhostIndex)

    // TODO: fix loop offsets -> no duplicate layers - don't generate iterationOffset loop bounds

    val innerLoop = IR_LoopOverPoints(field, None,
      GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -1, dim),
      GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -1, dim),
      IR_ExpressionIndex(1, 1, 1),
      ListBuffer[IR_Statement](
        IR_IfCondition(IR_LowerEqual(innerIt, 0),
          IR_Assignment(Duplicate(baseAccess), 0.0),
          IR_IfCondition(IR_LowerEqual(innerIt, zoneSize_1),
            IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt, dim)
              + zoneLength_1 * IR_FunctionCall("pow", ListBuffer[IR_Expression](step_1 * IR_LoopOverDimensions.defItForDim(dim), expo))),
            IR_IfCondition(IR_LowerEqual(innerIt, zoneSize_1 + zoneSize_2),
              IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + zoneSize_1, dim)
                + zoneLength_2 * step_2 * (IR_LoopOverDimensions.defItForDim(dim) - zoneSize_1)),
              IR_IfCondition(IR_LowerEqual(innerIt, innerIt + (zoneSize_1 + zoneSize_2 + zoneSize_3)),
                IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + (zoneSize_1 + zoneSize_2), dim)
                  + zoneLength_3 * step_3 * (IR_LoopOverDimensions.defItForDim(dim) - (zoneSize_1 + zoneSize_2))),
                IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1, dim))))))))
    innerLoop.parallelization.potentiallyParallel = false

    ListBuffer(
      innerLoop,
      IR_Assignment(Duplicate(leftGhostAccess),
        2 * GridUtil.offsetAccess(leftGhostAccess, 1, dim) - GridUtil.offsetAccess(leftGhostAccess, 2, dim)),
      IR_Assignment(Duplicate(rightGhostAccess),
        2 * GridUtil.offsetAccess(rightGhostAccess, -1, dim) - GridUtil.offsetAccess(rightGhostAccess, -2, dim)))
  }

  def setupStagCVWidth(dim : Int, level : Int) : ListBuffer[IR_Statement] = {
    val numCellsPerFrag = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim)
    val numCellsTotal = numCellsPerFrag * Knowledge.domain_rect_numFragsTotalAsVec(dim)

    // look up field and compile access to base element
    val baseIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim
    val field = IR_FieldCollection.getByIdentifier(s"stag_cv_width_${ IR_DimToString(dim) }", level).get
    val baseAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), Duplicate(baseIndex))
    val npField = IR_FieldCollection.getByIdentifier(s"node_pos_${ IR_DimToString(dim) }", level).get
    val npBaseAccess = IR_FieldAccess(IR_FieldSelection(npField, npField.level, 0), Duplicate(baseIndex))

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
        IR_Assignment(GridUtil.offsetAccess(leftGhostAccess, 1, dim), GridUtil.offsetAccess(leftGhostAccess, 2, dim)),
        IR_Assignment(Duplicate(leftGhostAccess), GridUtil.offsetAccess(leftGhostAccess, 1, dim))))

    val rightDir = Array(0, 0, 0)
    rightDir(dim) = 1
    val rightNeighIndex = DefaultNeighbors.getNeigh(rightDir).index

    val rightGhostIndex = IR_ExpressionIndex(0, 0, 0, 0)
    rightGhostIndex(dim) = numCellsPerFrag + 2
    val rightGhostAccess = IR_FieldAccess(IR_FieldSelection(field, field.level, 0), rightGhostIndex)

    val rightBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(field.domain.index, rightNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(rightGhostAccess, -1, dim), GridUtil.offsetAccess(rightGhostAccess, -2, dim)),
        IR_Assignment(Duplicate(rightGhostAccess), GridUtil.offsetAccess(rightGhostAccess, -1, dim))))

    // compile final loop
    val innerLoop = IR_LoopOverPoints(field, None,
      GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -1, dim),
      GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -1, dim),
      IR_ExpressionIndex(1, 1, 1),
      ListBuffer[IR_Statement](
        innerItDecl,
        IR_IfCondition(IR_EqEq(0, innerIt),
          IR_Assignment(Duplicate(baseAccess),
            0.5 * (Duplicate(npBaseAccess) + GridUtil.offsetAccess(npBaseAccess, 1, dim))
              - Duplicate(npBaseAccess)),
          IR_IfCondition(IR_EqEq(numCellsTotal, innerIt),
            IR_Assignment(Duplicate(baseAccess),
              Duplicate(npBaseAccess)
                - 0.5 * (GridUtil.offsetAccess(npBaseAccess, -1, dim) + Duplicate(npBaseAccess))),
            IR_Assignment(Duplicate(baseAccess),
              0.5 * (Duplicate(npBaseAccess) + GridUtil.offsetAccess(npBaseAccess, 1, dim))
                - 0.5 * (GridUtil.offsetAccess(npBaseAccess, -1, dim) + Duplicate(npBaseAccess)))))))
    innerLoop.parallelization.potentiallyParallel = false

    ListBuffer[IR_Statement](
      IR_LoopOverFragments(ListBuffer[IR_Statement](
        innerLoop,
        leftBoundaryUpdate,
        rightBoundaryUpdate)),
      IR_Communicate(IR_FieldSelection(field, level, 0), "both", ListBuffer(IR_CommunicateTarget("ghost", None, None)), None))
  }
}
