package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.domain.{ l4 => _, _ }
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.l4.{ L4_FieldLayoutOption, _ }
import exastencils.grid.ir.IR_VirtualFieldAccess
import exastencils.knowledge
import exastencils.knowledge.{ l4 => _, _ }
import exastencils.logger._
import exastencils.util._

abstract class GridGeometry() {
  // information always required
  def nodePosition(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression
  def cellCenter(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression

  def cellWidth(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression
  def gridWidth(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = cellWidth(level, index, arrayIndex, dim) // simple alias for most grids

  def cellVolume(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int]) : IR_Expression = {
    var exp : IR_Expression = cellWidth(level, index, arrayIndex, 0)
    for (dim <- 1 until Knowledge.dimensionality)
      exp *= cellWidth(level, index, arrayIndex, dim)
    exp
  }

  def cellCenterToFace(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = { 0.5 * cellWidth(level, index, arrayIndex, dim) }

  // resolution of special function accessing virtual fields
  def resolveGridMemberFunction(name : String) : Option[java.lang.reflect.Method] = {
    this.getClass().getMethods.find(_.getName.toLowerCase() == name.toLowerCase())
  }

  // helper method to map names of special fields to actual member functions implementing the resolving step
  def invokeAccessResolve(virtualField : IR_VirtualFieldAccess) : IR_Expression = {
    var functionName = virtualField.fieldName
    if (functionName.startsWith("vf_")) functionName = functionName.substring(3)
    functionName.substring(functionName.length() - 2) match {
      case "_x" => {
        val method = resolveGridMemberFunction(functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level, virtualField.index, virtualField.arrayIndex, 0 : Integer).asInstanceOf[IR_Expression]
      }
      case "_y" => {
        val method = resolveGridMemberFunction(functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level, virtualField.index, virtualField.arrayIndex, 1 : Integer).asInstanceOf[IR_Expression]
      }
      case "_z" => {
        val method = resolveGridMemberFunction(functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level, virtualField.index, virtualField.arrayIndex, 2 : Integer).asInstanceOf[IR_Expression]
      }
      case _    => {
        val method = resolveGridMemberFunction(functionName)
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level, virtualField.index, virtualField.arrayIndex).asInstanceOf[IR_Expression]
      }
    }
  }

  // injection of  missing l4 information for virtual fields and generation of setup code
  def initL4() : Unit
  def generateInitCode() : ListBuffer[IR_Statement]
}

object GridGeometry {
  def getGeometry = {
    if (Knowledge.grid_isUniform && !Knowledge.grid_isStaggered && Knowledge.grid_isAxisAligned)
      GridGeometry_uniform_nonStaggered_AA
    else if (Knowledge.grid_isUniform && Knowledge.grid_isStaggered && Knowledge.grid_isAxisAligned)
      GridGeometry_uniform_staggered_AA
    else if (!Knowledge.grid_isUniform && !Knowledge.grid_isStaggered && Knowledge.grid_isAxisAligned)
      GridGeometry_nonUniform_nonStaggered_AA
    else if (!Knowledge.grid_isUniform && Knowledge.grid_isStaggered && Knowledge.grid_isAxisAligned)
      GridGeometry_nonUniform_staggered_AA
    else
      Logger.error(s"Trying to get geometry for unsupported configuration of ( uniform : ${ Knowledge.grid_isUniform } ), ( staggered : ${ Knowledge.grid_isStaggered } ), ( axis-aligned : ${ Knowledge.grid_isAxisAligned } )")
  }
}

trait GridGeometry_uniform extends GridGeometry {
  // properties of uniform grids
  override def cellWidth(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = {
    val levelIndex = level.asInstanceOf[IR_IntegerConstant].v.toInt - Knowledge.minLevel
    dim match {
      case 0 => Knowledge.discr_hx(levelIndex)
      case 1 => Knowledge.discr_hy(levelIndex)
      case 2 => Knowledge.discr_hz(levelIndex)
    }
  }

  override def nodePosition(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = {
    //index(dim) * cellWidth(level, index, arrayIndex, dim) + ArrayAccess(iv.PrimitivePositionBegin(), dim)
    index(dim) * cellWidth(level, index, arrayIndex, dim) + IR_MemberAccess(iv.PrimitivePositionBegin(), dimToString(dim)) // FIXME: HACK
  }

  override def cellCenter(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = {
    (index(dim) + 0.5) * cellWidth(level, index, arrayIndex, dim) + IR_ArrayAccess(iv.PrimitivePositionBegin(), dim)
  }
}

trait GridGeometry_nonUniform extends GridGeometry {
  // direct accesses
  override def nodePosition(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = FieldCollection.getFieldByIdentifierLevExp(s"node_pos_${ dimToString(dim) }", level).get
    IR_FieldAccess(FieldSelection(field, field.level, 0, arrayIndex), GridUtil.projectIdx(index, dim))
  }

  // compound accesses
  override def cellCenter(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) = {
    0.5 * (nodePosition(level, GridUtil.offsetIndex(index, 1, dim), arrayIndex, dim) + nodePosition(level, Duplicate(index), arrayIndex, dim))
  }

  override def cellWidth(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) = {
    nodePosition(level, GridUtil.offsetIndex(index, 1, dim), arrayIndex, dim) - nodePosition(level, Duplicate(index), arrayIndex, dim)
  }

  // injection of  missing l4 information for virtual fields and generation of setup code
  override def initL4 = {
    val root = StateManager.root_.asInstanceOf[l4.Root]
    root.otherNodes += L4_FieldLayoutDecl(
      l4.LeveledIdentifier("DefNodeLineLayout_x", l4.AllLevelsSpecification),
      L4_RealDatatype, "Edge_Node".toLowerCase(),
      ListBuffer(
        L4_FieldLayoutOption("ghostLayers", L4_ConstIndex(2, 0, 0), false),
        L4_FieldLayoutOption("duplicateLayers", L4_ConstIndex(1, 0, 0), false),
        L4_FieldLayoutOption("innerPoints", L4_ConstIndex((1 << Knowledge.maxLevel) * Knowledge.domain_fragmentLength_x - 1, 1, 1), false)))
    root.otherNodes += L4_FieldLayoutDecl(
      l4.LeveledIdentifier("DefNodeLineLayout_y", l4.AllLevelsSpecification),
      L4_RealDatatype, "Edge_Node".toLowerCase(),
      ListBuffer(
        L4_FieldLayoutOption("ghostLayers", L4_ConstIndex(0, 2, 0), false),
        L4_FieldLayoutOption("duplicateLayers", L4_ConstIndex(0, 1, 0), false),
        L4_FieldLayoutOption("innerPoints", L4_ConstIndex(1, (1 << Knowledge.maxLevel) * Knowledge.domain_fragmentLength_y - 1, 1), false)))
    root.otherNodes += L4_FieldLayoutDecl(
      l4.LeveledIdentifier("DefNodeLineLayout_z", l4.AllLevelsSpecification),
      L4_RealDatatype, "Edge_Node".toLowerCase(),
      ListBuffer(
        L4_FieldLayoutOption("ghostLayers", L4_ConstIndex(0, 0, 2), false),
        L4_FieldLayoutOption("duplicateLayers", L4_ConstIndex(0, 0, 1), false),
        L4_FieldLayoutOption("innerPoints", L4_ConstIndex(1, 1, (1 << Knowledge.maxLevel) * Knowledge.domain_fragmentLength_z - 1), false)))

    root.otherNodes += L4_FieldDecl(
      l4.LeveledIdentifier("node_pos_x", l4.AllLevelsSpecification), "global", "DefNodeLineLayout_x", None, 1, 0)
    if (Knowledge.dimensionality > 1)
      root.otherNodes += L4_FieldDecl(
        l4.LeveledIdentifier("node_pos_y", l4.AllLevelsSpecification), "global", "DefNodeLineLayout_y", None, 1, 0)
    if (Knowledge.dimensionality > 2)
      root.otherNodes += L4_FieldDecl(
        l4.LeveledIdentifier("node_pos_z", l4.AllLevelsSpecification), "global", "DefNodeLineLayout_z", None, 1, 0)
  }

  def setupNodePos_Uniform(dim : Int, level : Int) : ListBuffer[IR_Statement] = {
    val numCellsPerFrag = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim)
    val numCellsTotal = numCellsPerFrag * Knowledge.domain_rect_numFragsTotalAsVec(dim)

    // fix grid width to match domain size
    if (DomainCollection.domains.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to the first domain")
    val domainBounds = DomainCollection.domains(0).asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]
    val cellWidth = (domainBounds.upper(dim) - domainBounds.lower(dim)) / numCellsTotal

    // look up field and compile access to base element
    val field = FieldCollection.getFieldByIdentifier(s"node_pos_${ dimToString(dim) }", level).get
    val baseIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim
    val baseAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), baseIndex)

    // fix the inner iterator -> used for zone checks
    def innerIt =
    if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
      IR_LoopOverDimensions.defItForDim(dim)
    else
      IR_VariableAccess(s"global_${ dimToString(dim) }", IR_IntegerDatatype)
    val innerItDecl =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_NullStatement
      else
        IR_VariableDeclaration(innerIt.asInstanceOf[IR_VariableAccess], IR_LoopOverDimensions.defItForDim(dim) + IR_ArrayAccess(iv.PrimitiveIndex(), dim) * numCellsPerFrag)

    // compile special boundary handling expressions
    var leftDir = Array(0, 0, 0);
    leftDir(dim) = -1
    val leftNeighIndex = knowledge.Fragment.getNeigh(leftDir).index

    var leftGhostIndex = IR_ExpressionIndex(0, 0, 0, 0);
    leftGhostIndex(dim) = -2
    val leftGhostAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), leftGhostIndex)

    val leftBoundaryUpdate = IR_IfCondition(
      IR_NegationExpression(iv.NeighborIsValid(field.domain.index, leftNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(leftGhostAccess, 1, dim),
          2 * GridUtil.offsetAccess(leftGhostAccess, 2, dim) - GridUtil.offsetAccess(leftGhostAccess, 3, dim)),
        IR_Assignment(Duplicate(leftGhostAccess),
          2 * GridUtil.offsetAccess(leftGhostAccess, 1, dim) - GridUtil.offsetAccess(leftGhostAccess, 2, dim))))

    var rightDir = Array(0, 0, 0);
    rightDir(dim) = 1
    val rightNeighIndex = knowledge.Fragment.getNeigh(rightDir).index

    var rightGhostIndex = IR_ExpressionIndex(0, 0, 0, 0);
    rightGhostIndex(dim) = numCellsPerFrag + 2
    val rightGhostAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), rightGhostIndex)

    val rightBoundaryUpdate = IR_IfCondition(
      IR_NegationExpression(iv.NeighborIsValid(field.domain.index, rightNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(rightGhostAccess, -1, dim),
          2 * GridUtil.offsetAccess(rightGhostAccess, -2, dim) - GridUtil.offsetAccess(rightGhostAccess, -3, dim)),
        IR_Assignment(Duplicate(rightGhostAccess),
          2 * GridUtil.offsetAccess(rightGhostAccess, -1, dim) - GridUtil.offsetAccess(rightGhostAccess, -2, dim))))

    // compile final loop
    ListBuffer[IR_Statement](
      IR_LoopOverFragments(ListBuffer[IR_Statement](
        IR_LoopOverPoints(field, None, true,
          GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -2, dim),
          GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -2, dim),
          IR_ExpressionIndex(1, 1, 1),
          ListBuffer[IR_Statement](
            innerItDecl,
            IR_Assignment(Duplicate(baseAccess),
              domainBounds.lower(dim) - innerIt * cellWidth))),
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
    val firstIntervalAlphaCoeff = 0.5 * xf * xf + 0.5 * xf
    val firstIntervalBetaCoeff = xf + 1

    // fix alpha to match domain size
    if (DomainCollection.domains.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to the first domain")
    val domainBounds = DomainCollection.domains(0).asInstanceOf[RectangularDomain].shape.asInstanceOf[RectangularDomainShape].shapeData.asInstanceOf[AABB]

    // simple approach: alpha and beta are equal -> results in very small volumes and aspect ratios if the number of points is high
    //    val alpha = domainSize / (lastPointAlphaCoeff + lastPointBetaCoeff)
    //    val beta = alpha

    // better approach: fix the ratio between smallest and largest cell width to 8
    val factor = (numCellsTotal / 4) / 8.0
    val alpha = (domainBounds.upper(dim) - domainBounds.lower(dim)) / (lastPointAlphaCoeff + lastPointBetaCoeff * factor)
    val beta = factor * alpha

    //Logger.debug(s"Using alpha $alpha and beta $beta")

    // look up field and compile access to base element
    val field = FieldCollection.getFieldByIdentifier(s"node_pos_${ dimToString(dim) }", level).get
    val baseIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim
    val baseAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), baseIndex)

    // fix the inner iterator -> used for zone checks
    def innerIt =
    if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
      IR_LoopOverDimensions.defItForDim(dim)
    else
      IR_VariableAccess(s"global_${ dimToString(dim) }", IR_IntegerDatatype)
    val innerItDecl =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_NullStatement
      else
        IR_VariableDeclaration(innerIt.asInstanceOf[IR_VariableAccess], IR_LoopOverDimensions.defItForDim(dim) + IR_ArrayAccess(iv.PrimitiveIndex(), dim) * numCellsPerFrag)

    // compile special boundary handling expressions
    var leftDir = Array(0, 0, 0);
    leftDir(dim) = -1
    val leftNeighIndex = knowledge.Fragment.getNeigh(leftDir).index

    var leftGhostIndex = IR_ExpressionIndex(0, 0, 0, 0);
    leftGhostIndex(dim) = -2
    val leftGhostAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), leftGhostIndex)

    val leftBoundaryUpdate = IR_IfCondition(
      IR_NegationExpression(iv.NeighborIsValid(field.domain.index, leftNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(leftGhostAccess, 1, dim),
          2 * GridUtil.offsetAccess(leftGhostAccess, 2, dim) - GridUtil.offsetAccess(leftGhostAccess, 3, dim)),
        IR_Assignment(Duplicate(leftGhostAccess),
          2 * GridUtil.offsetAccess(leftGhostAccess, 1, dim) - GridUtil.offsetAccess(leftGhostAccess, 2, dim))))

    var rightDir = Array(0, 0, 0);
    rightDir(dim) = 1
    val rightNeighIndex = knowledge.Fragment.getNeigh(rightDir).index

    var rightGhostIndex = IR_ExpressionIndex(0, 0, 0, 0);
    rightGhostIndex(dim) = numCellsPerFrag + 2
    val rightGhostAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), rightGhostIndex)

    val rightBoundaryUpdate = IR_IfCondition(
      IR_NegationExpression(iv.NeighborIsValid(field.domain.index, rightNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(rightGhostAccess, -1, dim),
          2 * GridUtil.offsetAccess(rightGhostAccess, -2, dim) - GridUtil.offsetAccess(rightGhostAccess, -3, dim)),
        IR_Assignment(Duplicate(rightGhostAccess),
          2 * GridUtil.offsetAccess(rightGhostAccess, -1, dim) - GridUtil.offsetAccess(rightGhostAccess, -2, dim))))

    // compile final loop
    ListBuffer[IR_Statement](
      IR_LoopOverFragments(ListBuffer[IR_Statement](
        IR_LoopOverPoints(field, None, true,
          GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -2, dim),
          GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -2, dim),
          IR_ExpressionIndex(1, 1, 1),
          ListBuffer[IR_Statement](
            innerItDecl,
            IR_IfCondition(IR_LowerEqualExpression(innerIt, xf + 1),
              IR_Assignment(Duplicate(baseAccess),
                domainBounds.lower(dim) + 0.5 * alpha * innerIt * innerIt + (beta - 0.5 * alpha) * innerIt),
              IR_IfCondition(IR_LowerEqualExpression(innerIt, xs + 1),
                IR_Assignment(Duplicate(baseAccess),
                  domainBounds.lower(dim) - 0.5 * alpha * (xf * xf + xf) + (beta + alpha * xf) * innerIt),
                IR_Assignment(Duplicate(baseAccess),
                  domainBounds.lower(dim) - 0.5 * alpha * innerIt * innerIt
                    + (alpha * xf + alpha * xs + 0.5 * alpha + beta) * innerIt
                    - 0.5 * alpha * (xf * xf + xf + xs * xs + xs)))))),
        leftBoundaryUpdate,
        rightBoundaryUpdate)))
  }
}

trait GridGeometry_staggered extends GridGeometry {
  // additional information introduced by the staggered property
  def stagCVWidth(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression // depends on uniform property

  // compound accesses
  def staggeredCellVolume(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], stagDim : Int) = {
    var exp : IR_Expression = (
      if (0 == stagDim)
        stagCVWidth(level, index, arrayIndex, 0)
      else
        cellWidth(level, index, arrayIndex, 0))
    for (dim <- 1 until Knowledge.dimensionality)
      if (dim == stagDim)
        exp *= stagCVWidth(level, index, arrayIndex, dim)
      else
        exp *= cellWidth(level, index, arrayIndex, dim)
    exp
  }

  def xStagCellVolume(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int]) : IR_Expression = staggeredCellVolume(level, index, arrayIndex, 0)
  def yStagCellVolume(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int]) : IR_Expression = staggeredCellVolume(level, index, arrayIndex, 1)
  def zStagCellVolume(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int]) : IR_Expression = staggeredCellVolume(level, index, arrayIndex, 2)
}

object GridGeometry_uniform_nonStaggered_AA extends GridGeometry_uniform {
  // nothing else to do here since everything can be pre-computed/ inlined
  override def initL4() = {}
  override def generateInitCode() = ListBuffer()
}

object GridGeometry_uniform_staggered_AA extends GridGeometry_uniform with GridGeometry_staggered {
  // direct accesses
  override def stagCVWidth(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) = {
    // TODO: this introduces a slight extension at the physical boundary in the stagger dimension -> how to handle this? relevant or neglectable?
    0.5 * (cellWidth(level, GridUtil.offsetIndex(index, -1, dim), arrayIndex, dim) + cellWidth(level, index, arrayIndex, dim))
  }

  // nothing else to do here since everything can be pre-computed/ inlined
  override def initL4() = {}
  override def generateInitCode() = ListBuffer()
}

object GridGeometry_nonUniform_nonStaggered_AA extends GridGeometry_nonUniform {
  override def generateInitCode = {
    Knowledge.grid_spacingModel match {
      case "uniform"   =>
        ((Knowledge.maxLevel to Knowledge.minLevel by -1).map(level =>
          (0 until Knowledge.dimensionality).to[ListBuffer].flatMap(dim => setupNodePos_Uniform(dim, level)))
          .reduceLeft(_ ++ _))
      case "linearFct" =>
        Logger.warn("LinearFct spacing model is currently not recommended for GridGeometry_nonUniform_nonStaggered_AA since grid point positions won't match accross levels")
        ((Knowledge.maxLevel to Knowledge.minLevel by -1).map(level =>
          (0 until Knowledge.dimensionality).to[ListBuffer].flatMap(dim => setupNodePos_LinearFct(dim, level)))
          .reduceLeft(_ ++ _))
    }
  }
}

object GridGeometry_nonUniform_staggered_AA extends GridGeometry_nonUniform with GridGeometry_staggered {
  // direct accesses
  override def stagCVWidth(level : IR_Expression, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = FieldCollection.getFieldByIdentifierLevExp(s"stag_cv_width_${ dimToString(dim) }", level).get
    IR_FieldAccess(FieldSelection(field, field.level, 0, arrayIndex), GridUtil.projectIdx(index, dim))
  }

  // injection of  missing l4 information for virtual fields and generation of setup code
  override def initL4 = {
    super.initL4 // set basic stuff

    // extend with info required by staggered grid
    val root = StateManager.root_.asInstanceOf[l4.Root]

    root.otherNodes += L4_FieldDecl(
      l4.LeveledIdentifier("stag_cv_width_x", l4.FinestLevelSpecification), "global", "DefNodeLineLayout_x", None, 1, 0)
    if (Knowledge.dimensionality > 1)
      root.otherNodes += L4_FieldDecl(
        l4.LeveledIdentifier("stag_cv_width_y", l4.FinestLevelSpecification), "global", "DefNodeLineLayout_y", None, 1, 0)
    if (Knowledge.dimensionality > 2)
      root.otherNodes += L4_FieldDecl(
        l4.LeveledIdentifier("stag_cv_width_z", l4.FinestLevelSpecification), "global", "DefNodeLineLayout_z", None, 1, 0)
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
        (0 until Knowledge.dimensionality).to[ListBuffer].flatMap(dim => setupNodePos_Diego(dim, Knowledge.maxLevel)) ++
          (0 until Knowledge.dimensionality).to[ListBuffer].flatMap(dim => setupStagCVWidth(dim, Knowledge.maxLevel))
      case "diego2"    =>
        (0 until Knowledge.dimensionality).to[ListBuffer].flatMap(dim => setupNodePos_Diego2(dim, Knowledge.maxLevel)) ++
          (0 until Knowledge.dimensionality).to[ListBuffer].flatMap(dim => setupStagCVWidth(dim, Knowledge.maxLevel))
      case "linearFct" =>
        (0 until Knowledge.dimensionality).to[ListBuffer].flatMap(dim => setupNodePos_LinearFct(dim, Knowledge.maxLevel)) ++
          (0 until Knowledge.dimensionality).to[ListBuffer].flatMap(dim => setupStagCVWidth(dim, Knowledge.maxLevel))
    }
  }

  def setupNodePos_Diego(dim : Int, level : Int) : ListBuffer[IR_Statement] = {
    val expo = 1.5
    val numCells = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim) // number of cells per fragment
    val zoneSize = numCells / 4
    val step = 1.0 / zoneSize

    val zoneLength = 0.0095 //* 8 / zoneSize

    val field = FieldCollection.getFieldByIdentifier(s"node_pos_${ dimToString(dim) }", level).get
    val baseIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim
    val baseAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), baseIndex)

    val innerIt = IR_LoopOverDimensions.defItForDim(dim)

    var leftGhostIndex = IR_ExpressionIndex(0, 0, 0, 0);
    leftGhostIndex(dim) = -1
    val leftGhostAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), leftGhostIndex)
    var rightGhostIndex = IR_ExpressionIndex(0, 0, 0, 0);
    rightGhostIndex(dim) = numCells + 1
    val rightGhostAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), rightGhostIndex)

    // TODO: fix loop offsets -> no duplicate layers - don't generate iterationOffset loop bounds

    ListBuffer(
      IR_LoopOverPoints(field, None, true,
        GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -1, dim),
        GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -1, dim),
        IR_ExpressionIndex(1, 1, 1),
        ListBuffer[IR_Statement](
          IR_IfCondition(IR_LowerEqualExpression(innerIt, 0),
            IR_Assignment(Duplicate(baseAccess), 0.0),
            IR_IfCondition(IR_LowerEqualExpression(innerIt, 1 * zoneSize),
              IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + 0 * zoneSize, dim)
                + zoneLength * IR_FunctionCall("pow", ListBuffer[IR_Expression](step * (IR_LoopOverDimensions.defItForDim(dim) - 0.0 * zoneSize), expo))),
              IR_IfCondition(IR_LowerEqualExpression(innerIt, 2 * zoneSize),
                IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + 1 * zoneSize, dim)
                  + zoneLength * step * (IR_LoopOverDimensions.defItForDim(dim) - 1.0 * zoneSize)),
                IR_IfCondition(IR_LowerEqualExpression(innerIt, 3 * zoneSize),
                  IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + 2 * zoneSize, dim)
                    + zoneLength * step * (IR_LoopOverDimensions.defItForDim(dim) - 2.0 * zoneSize)),
                  IR_IfCondition(IR_LowerEqualExpression(innerIt, 4 * zoneSize),
                    IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + 3 * zoneSize, dim)
                      + zoneLength * (1.0 - IR_FunctionCall("pow", ListBuffer[IR_Expression](1.0 - step * (IR_LoopOverDimensions.defItForDim(dim) - 3.0 * zoneSize), expo)))),
                    IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1, dim))))))))),
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

    val field = FieldCollection.getFieldByIdentifier(s"node_pos_${ dimToString(dim) }", level).get
    val baseIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // TODO: dim
    val baseAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), baseIndex)

    val innerIt = IR_LoopOverDimensions.defItForDim(dim)

    var leftGhostIndex = IR_ExpressionIndex(0, 0, 0, 0);
    leftGhostIndex(dim) = -1
    val leftGhostAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), leftGhostIndex)
    var rightGhostIndex = IR_ExpressionIndex(0, 0, 0, 0);
    rightGhostIndex(dim) = numCells + 1
    val rightGhostAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), rightGhostIndex)

    // TODO: fix loop offsets -> no duplicate layers - don't generate iterationOffset loop bounds

    ListBuffer(
      IR_LoopOverPoints(field, None, true,
        GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -1, dim),
        GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -1, dim),
        IR_ExpressionIndex(1, 1, 1),
        ListBuffer[IR_Statement](
          IR_IfCondition(IR_LowerEqualExpression(innerIt, 0),
            IR_Assignment(Duplicate(baseAccess), 0.0),
            IR_IfCondition(IR_LowerEqualExpression(innerIt, zoneSize_1),
              IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt, dim)
                + zoneLength_1 * IR_FunctionCall("pow", ListBuffer[IR_Expression](step_1 * (IR_LoopOverDimensions.defItForDim(dim)), expo))),
              IR_IfCondition(IR_LowerEqualExpression(innerIt, (zoneSize_1 + zoneSize_2)),
                IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + zoneSize_1, dim)
                  + zoneLength_2 * step_2 * (IR_LoopOverDimensions.defItForDim(dim) - zoneSize_1)),
                IR_IfCondition(IR_LowerEqualExpression(innerIt, innerIt + (zoneSize_1 + zoneSize_2 + zoneSize_3)),
                  IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + (zoneSize_1 + zoneSize_2), dim)
                    + zoneLength_3 * step_3 * (IR_LoopOverDimensions.defItForDim(dim) - (zoneSize_1 + zoneSize_2))),
                  IR_Assignment(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1, dim)))))))),
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
    val field = FieldCollection.getFieldByIdentifier(s"stag_cv_width_${ dimToString(dim) }", level).get
    val baseAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), Duplicate(baseIndex))
    val npField = FieldCollection.getFieldByIdentifier(s"node_pos_${ dimToString(dim) }", level).get
    val npBaseAccess = IR_FieldAccess(FieldSelection(npField, npField.level, 0), Duplicate(baseIndex))

    // fix the inner iterator -> used for zone checks
    def innerIt =
    if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
      IR_LoopOverDimensions.defItForDim(dim)
    else
      IR_VariableAccess(s"global_${ dimToString(dim) }", IR_IntegerDatatype)
    val innerItDecl =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_NullStatement
      else
        IR_VariableDeclaration(innerIt.asInstanceOf[IR_VariableAccess], IR_LoopOverDimensions.defItForDim(dim) + IR_ArrayAccess(iv.PrimitiveIndex(), dim) * numCellsPerFrag)

    // compile special boundary handling expressions
    var leftDir = Array(0, 0, 0);
    leftDir(dim) = -1
    val leftNeighIndex = knowledge.Fragment.getNeigh(leftDir).index

    var leftGhostIndex = IR_ExpressionIndex(0, 0, 0, 0);
    leftGhostIndex(dim) = -2
    val leftGhostAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), leftGhostIndex)

    val leftBoundaryUpdate = IR_IfCondition(
      IR_NegationExpression(iv.NeighborIsValid(field.domain.index, leftNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(leftGhostAccess, 1, dim), GridUtil.offsetAccess(leftGhostAccess, 2, dim)),
        IR_Assignment(Duplicate(leftGhostAccess), GridUtil.offsetAccess(leftGhostAccess, 1, dim))))

    var rightDir = Array(0, 0, 0);
    rightDir(dim) = 1
    val rightNeighIndex = knowledge.Fragment.getNeigh(rightDir).index

    var rightGhostIndex = IR_ExpressionIndex(0, 0, 0, 0);
    rightGhostIndex(dim) = numCellsPerFrag + 2
    val rightGhostAccess = IR_FieldAccess(FieldSelection(field, field.level, 0), rightGhostIndex)

    val rightBoundaryUpdate = IR_IfCondition(
      IR_NegationExpression(iv.NeighborIsValid(field.domain.index, rightNeighIndex)),
      ListBuffer[IR_Statement](
        IR_Assignment(GridUtil.offsetAccess(rightGhostAccess, -1, dim), GridUtil.offsetAccess(rightGhostAccess, -2, dim)),
        IR_Assignment(Duplicate(rightGhostAccess), GridUtil.offsetAccess(rightGhostAccess, -1, dim))))

    // compile final loop
    ListBuffer[IR_Statement](
      IR_LoopOverFragments(ListBuffer[IR_Statement](
        IR_LoopOverPoints(field, None, true,
          GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -1, dim),
          GridUtil.offsetIndex(IR_ExpressionIndex(0, 0, 0), -1, dim),
          IR_ExpressionIndex(1, 1, 1),
          ListBuffer[IR_Statement](
            innerItDecl,
            IR_IfCondition(IR_EqEqExpression(0, innerIt),
              IR_Assignment(Duplicate(baseAccess),
                0.5 * (Duplicate(npBaseAccess) + GridUtil.offsetAccess(npBaseAccess, 1, dim))
                  - Duplicate(npBaseAccess)),
              IR_IfCondition(IR_EqEqExpression(numCellsTotal, innerIt),
                IR_Assignment(Duplicate(baseAccess),
                  Duplicate(npBaseAccess)
                    - 0.5 * (GridUtil.offsetAccess(npBaseAccess, -1, dim) + Duplicate(npBaseAccess))),
                IR_Assignment(Duplicate(baseAccess),
                  0.5 * (Duplicate(npBaseAccess) + GridUtil.offsetAccess(npBaseAccess, 1, dim))
                    - 0.5 * (GridUtil.offsetAccess(npBaseAccess, -1, dim) + Duplicate(npBaseAccess))))))),
        leftBoundaryUpdate,
        rightBoundaryUpdate)))
  }
}
