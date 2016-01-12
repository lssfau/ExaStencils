package exastencils.grid

import exastencils.core._
import exastencils.datastructures.l4
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._
import scala.collection.mutable.ListBuffer

abstract class GridGeometry() {
  // information always required
  def nodePosition(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression
  def cellCenter(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression

  def cellWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression
  def gridWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = cellWidth(level, index, arrayIndex, dim) // simple alias for most grids

  def cellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int]) : Expression = {
    var exp : Expression = cellWidth(level, index, arrayIndex, 0)
    for (dim <- 1 until Knowledge.dimensionality)
      exp *= cellWidth(level, index, arrayIndex, dim)
    exp
  }

  def cellCenterToFace(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = { 0.5 * cellWidth(level, index, arrayIndex, dim) }

  // resolution of special function accessing virtual fields
  def resolveGridMemberFunction(name : String) : Option[java.lang.reflect.Method] = {
    this.getClass().getMethods.find(_.getName.toLowerCase() == name.toLowerCase())
  }

  // helper method to map names of special fields to actual member functions implementing the resolving step
  def invokeAccessResolve(virtualField : VirtualFieldAccess) : Expression = {
    var functionName = virtualField.fieldName
    if (functionName.startsWith("vf_")) functionName = functionName.substring(3)
    functionName.substring(functionName.length() - 2) match {
      case "_x" => {
        val method = resolveGridMemberFunction(functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level, virtualField.index, virtualField.arrayIndex, 0 : Integer).asInstanceOf[Expression]
      }
      case "_y" => {
        val method = resolveGridMemberFunction(functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level, virtualField.index, virtualField.arrayIndex, 1 : Integer).asInstanceOf[Expression]
      }
      case "_z" => {
        val method = resolveGridMemberFunction(functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level, virtualField.index, virtualField.arrayIndex, 2 : Integer).asInstanceOf[Expression]
      }
      case _ => {
        val method = resolveGridMemberFunction(functionName)
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, virtualField.level, virtualField.index, virtualField.arrayIndex).asInstanceOf[Expression]
      }
    }
  }

  // injection of  missing l4 information for virtual fields and generation of setup code
  def initL4()
  def generateInitCode() : ListBuffer[Statement]
}

object GridGeometry {
  def getGeometry = {
    if (Knowledge.grid_isUniform && !Knowledge.grid_isStaggered && Knowledge.grid_isAxisAligned)
      GridGeometry_uniform_nonStaggered_AA
    else if (Knowledge.grid_isUniform && Knowledge.grid_isStaggered && Knowledge.grid_isAxisAligned)
      GridGeometry_uniform_staggered_AA
    else if (!Knowledge.grid_isUniform && Knowledge.grid_isStaggered && Knowledge.grid_isAxisAligned)
      GridGeometry_nonUniform_staggered_AA
    else
      Logger.error(s"Trying to get geometry for unsupported configuration of ( uniform : ${Knowledge.grid_isUniform} ), ( staggered : ${Knowledge.grid_isStaggered} ), ( axis-aligned : ${Knowledge.grid_isAxisAligned} )")
  }
}

trait GridGeometry_uniform extends GridGeometry {
  // properties of uniform grids
  override def cellWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    val levelIndex = level.asInstanceOf[IntegerConstant].v.toInt - Knowledge.minLevel
    dim match {
      case 0 => Knowledge.discr_hx(levelIndex)
      case 1 => Knowledge.discr_hy(levelIndex)
      case 2 => Knowledge.discr_hz(levelIndex)
    }
  }

  override def nodePosition(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    index(dim) * cellWidth(level, index, arrayIndex, dim) + ArrayAccess(iv.PrimitivePositionBegin(), dim)
  }

  override def cellCenter(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    (index(dim) + 0.5) * cellWidth(level, index, arrayIndex, dim) + ArrayAccess(iv.PrimitivePositionBegin(), dim)
  }
}

trait GridGeometry_nonUniform extends GridGeometry {
  // direct accesses
  override def nodePosition(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = FieldCollection.getFieldByIdentifierLevExp(s"node_pos_${dimToString(dim)}", level).get
    FieldAccess(FieldSelection(field, field.level, 0, arrayIndex), GridUtil.projectIdx(index, dim))
  }

  // compound accesses
  override def cellCenter(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    0.5 * (nodePosition(level, GridUtil.offsetIndex(index, 1, dim), arrayIndex, dim) + nodePosition(level, Duplicate(index), arrayIndex, dim))
  }

  override def cellWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    nodePosition(level, GridUtil.offsetIndex(index, 1, dim), arrayIndex, dim) - nodePosition(level, Duplicate(index), arrayIndex, dim)
  }
}

trait GridGeometry_staggered extends GridGeometry {
  // additional information introduced by the staggered property
  def stagCVWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression // depends on uniform property

  // compound accesses
  def staggeredCellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int], stagDim : Int) = {
    var exp : Expression = (
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

  def xStagCellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int]) : Expression = staggeredCellVolume(level, index, arrayIndex, 0)
  def yStagCellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int]) : Expression = staggeredCellVolume(level, index, arrayIndex, 1)
  def zStagCellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int]) : Expression = staggeredCellVolume(level, index, arrayIndex, 2)
}

object GridGeometry_uniform_nonStaggered_AA extends GridGeometry_uniform {
  // nothing else to do here since everything can be pre-computed/ inlined
  override def initL4() = {}
  override def generateInitCode() = ListBuffer()
}

object GridGeometry_uniform_staggered_AA extends GridGeometry_uniform with GridGeometry_staggered {
  // direct accesses
  override def stagCVWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    // TODO: this introduces a slight extension at the physical boundary in the stagger dimension -> how to handle this? relevant or neglectable?
    0.5 * (cellWidth(level, GridUtil.offsetIndex(index, -1, dim), arrayIndex, dim) + cellWidth(level, index, arrayIndex, dim))
  }

  // nothing else to do here since everything can be pre-computed/ inlined
  override def initL4() = {}
  override def generateInitCode() = ListBuffer()
}

object GridGeometry_nonUniform_staggered_AA extends GridGeometry_nonUniform with GridGeometry_staggered {
  // direct accesses
  override def stagCVWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = FieldCollection.getFieldByIdentifierLevExp(s"stag_cv_width_${dimToString(dim)}", level).get
    FieldAccess(FieldSelection(field, field.level, 0, arrayIndex), GridUtil.projectIdx(index, dim))
  }

  // injection of  missing l4 information for virtual fields and generation of setup code
  override def initL4 = {
    val root = StateManager.root_.asInstanceOf[l4.Root]
    root.fieldLayouts += l4.LayoutDeclarationStatement(
      l4.LeveledIdentifier("DefNodeLineLayout_x", l4.FinestLevelSpecification()),
      l4.RealDatatype(), "Edge_Node".toLowerCase(),
      Some(l4.Index3D(2, 0, 0)), None,
      Some(l4.Index3D(1, 0, 0)), None,
      Some(l4.Index3D((1 << Knowledge.maxLevel) * Knowledge.domain_fragmentLength_x - 1, 1, 1)))
    root.fieldLayouts += l4.LayoutDeclarationStatement(
      l4.LeveledIdentifier("DefNodeLineLayout_y", l4.FinestLevelSpecification()),
      l4.RealDatatype(), "Edge_Node".toLowerCase(),
      Some(l4.Index3D(0, 2, 0)), None,
      Some(l4.Index3D(0, 1, 0)), None,
      Some(l4.Index3D(1, (1 << Knowledge.maxLevel) * Knowledge.domain_fragmentLength_y - 1, 1)))
    root.fieldLayouts += l4.LayoutDeclarationStatement(
      l4.LeveledIdentifier("DefNodeLineLayout_z", l4.FinestLevelSpecification()),
      l4.RealDatatype(), "Edge_Node".toLowerCase(),
      Some(l4.Index3D(0, 0, 2)), None,
      Some(l4.Index3D(0, 0, 1)), None,
      Some(l4.Index3D(1, 1, (1 << Knowledge.maxLevel) * Knowledge.domain_fragmentLength_z - 1)))

    root.fields += l4.FieldDeclarationStatement(
      l4.LeveledIdentifier("node_pos_x", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout_x", None, 1, 0)
    if (Knowledge.dimensionality > 1)
      root.fields += l4.FieldDeclarationStatement(
        l4.LeveledIdentifier("node_pos_y", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout_y", None, 1, 0)
    if (Knowledge.dimensionality > 2)
      root.fields += l4.FieldDeclarationStatement(
        l4.LeveledIdentifier("node_pos_z", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout_z", None, 1, 0)

    root.fields += l4.FieldDeclarationStatement(
      l4.LeveledIdentifier("stag_cv_width_x", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout_x", None, 1, 0)
    if (Knowledge.dimensionality > 1)
      root.fields += l4.FieldDeclarationStatement(
        l4.LeveledIdentifier("stag_cv_width_y", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout_y", None, 1, 0)
    if (Knowledge.dimensionality > 2)
      root.fields += l4.FieldDeclarationStatement(
        l4.LeveledIdentifier("stag_cv_width_z", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout_z", None, 1, 0)
  }

  override def generateInitCode() = {
    /// node_pos        -> nodes of the original grid
    /// o   o   o   o   o
    /// cell_width      -> width of the control volumes of the original grid
    /// |---|   |---|
    /// stag_cv_width   -> width of the staggered control volumes
    /// |-|   |---|   |-|

    val gridSpacing = "linearFct" // "diego" or "linearFct" -> TODO: integrate with knowledge

    gridSpacing match {
      case "diego" =>
        (0 until Knowledge.dimensionality).to[ListBuffer].flatMap(dim => setupNodePos_Diego(dim, Knowledge.maxLevel)) ++
          (0 until Knowledge.dimensionality).to[ListBuffer].flatMap(dim => setupStagCVWidth(dim, Knowledge.maxLevel))
      case "linearFct" =>
        (0 until Knowledge.dimensionality).to[ListBuffer].flatMap(dim => setupNodePos_LinearFct(dim, Knowledge.maxLevel)) ++
          (0 until Knowledge.dimensionality).to[ListBuffer].flatMap(dim => setupStagCVWidth(dim, Knowledge.maxLevel))
    }
  }

  def setupNodePos_Diego(dim : Int, level : Int) : ListBuffer[Statement] = {
    val expo = 1.5
    val numCells = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim) // number of cells per fragment
    val zoneSize = numCells / 4
    val step = 1.0 / zoneSize

    val field = FieldCollection.getFieldByIdentifier(s"node_pos_${dimToString(dim)}", level).get
    var baseIndex = LoopOverDimensions.defIt
    baseIndex(Knowledge.dimensionality) = 0
    val baseAccess = FieldAccess(FieldSelection(field, field.level, 0), baseIndex)

    val innerIt = LoopOverDimensions.defIt(dim)

    var leftGhostIndex = new MultiIndex(0, 0, 0, 0); leftGhostIndex(dim) = -1
    val leftGhostAccess = FieldAccess(FieldSelection(field, field.level, 0), leftGhostIndex)
    var rightGhostIndex = new MultiIndex(0, 0, 0, 0); rightGhostIndex(dim) = numCells + 1
    val rightGhostAccess = FieldAccess(FieldSelection(field, field.level, 0), rightGhostIndex)

    // TODO: fix loop offsets -> no duplicate layers - don't generate iterationOffset loop bounds

    ListBuffer(
      LoopOverPoints(field, None, true,
        GridUtil.offsetIndex(MultiIndex(0, 0, 0), -1, dim),
        GridUtil.offsetIndex(MultiIndex(0, 0, 0), -1, dim),
        MultiIndex(1, 1, 1),
        ListBuffer[Statement](
          new ConditionStatement(LowerEqualExpression(innerIt, 0),
            AssignmentStatement(Duplicate(baseAccess), 0.0),
            new ConditionStatement(LowerEqualExpression(innerIt, 1 * zoneSize),
              AssignmentStatement(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + 0 * zoneSize, dim)
                + 0.0095 * FunctionCallExpression("pow", ListBuffer[Expression](step * (LoopOverDimensions.defIt(dim) - 0.0 * zoneSize), expo))),
              new ConditionStatement(LowerEqualExpression(innerIt, 2 * zoneSize),
                AssignmentStatement(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + 1 * zoneSize, dim)
                  + 0.0095 * step * (LoopOverDimensions.defIt(dim) - 1.0 * zoneSize)),
                new ConditionStatement(LowerEqualExpression(innerIt, 3 * zoneSize),
                  AssignmentStatement(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + 2 * zoneSize, dim)
                    + 0.0095 * step * (LoopOverDimensions.defIt(dim) - 2.0 * zoneSize)),
                  new ConditionStatement(LowerEqualExpression(innerIt, 4 * zoneSize),
                    AssignmentStatement(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1 * innerIt + 3 * zoneSize, dim)
                      + 0.0095 * (1.0 - FunctionCallExpression("pow", ListBuffer[Expression](1.0 - step * (LoopOverDimensions.defIt(dim) - 3.0 * zoneSize), expo)))),
                    AssignmentStatement(Duplicate(baseAccess), GridUtil.offsetAccess(baseAccess, -1, dim))))))))),
      AssignmentStatement(Duplicate(leftGhostAccess),
        2 * GridUtil.offsetAccess(leftGhostAccess, 1, dim) - GridUtil.offsetAccess(leftGhostAccess, 2, dim)),
      AssignmentStatement(Duplicate(rightGhostAccess),
        2 * GridUtil.offsetAccess(rightGhostAccess, -1, dim) - GridUtil.offsetAccess(rightGhostAccess, -2, dim)))
  }

  def setupNodePos_LinearFct(dim : Int, level : Int) : ListBuffer[Statement] = {
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
    val domainSize = 0.05 // TODO: get from DSL

    // simple approach: alpha and beta are equal -> results in very small volumes and aspect ratios if the number of points is high
    //    val alpha = domainSize / (lastPointAlphaCoeff + lastPointBetaCoeff)
    //    val beta = alpha

    // better approach: fix the ratio between smallest and largest cell width to 8
    val factor = (numCellsTotal / 4) / 8.0
    val alpha = domainSize / (lastPointAlphaCoeff + lastPointBetaCoeff * factor)
    val beta = factor * alpha

    //Logger.debug(s"Using alpha $alpha and beta $beta")

    // look up field and compile access to base element
    val field = FieldCollection.getFieldByIdentifier(s"node_pos_${dimToString(dim)}", level).get
    var baseIndex = LoopOverDimensions.defIt
    baseIndex(Knowledge.dimensionality) = 0
    val baseAccess = FieldAccess(FieldSelection(field, field.level, 0), baseIndex)

    // fix the inner iterator -> used for zone checks
    def innerIt =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        LoopOverDimensions.defIt(dim)
      else
        VariableAccess(s"global_${dimToString(dim)}", Some(IntegerDatatype))
    val innerItDecl =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        NullStatement
      else
        new VariableDeclarationStatement(innerIt.asInstanceOf[VariableAccess], LoopOverDimensions.defIt(dim) + ArrayAccess(iv.PrimitiveIndex(), dim) * numCellsPerFrag)

    // compile special boundary handling expressions
    var leftDir = Array(0, 0, 0); leftDir(dim) = -1
    val leftNeighIndex = Fragment.getNeigh(leftDir).index

    var leftGhostIndex = new MultiIndex(0, 0, 0, 0); leftGhostIndex(dim) = -2
    val leftGhostAccess = FieldAccess(FieldSelection(field, field.level, 0), leftGhostIndex)

    val leftBoundaryUpdate = new ConditionStatement(
      NegationExpression(iv.NeighborIsValid(field.domain.index, leftNeighIndex)),
      ListBuffer[Statement](
        AssignmentStatement(GridUtil.offsetAccess(leftGhostAccess, 1, dim),
          2 * GridUtil.offsetAccess(leftGhostAccess, 2, dim) - GridUtil.offsetAccess(leftGhostAccess, 3, dim)),
        AssignmentStatement(Duplicate(leftGhostAccess),
          2 * GridUtil.offsetAccess(leftGhostAccess, 1, dim) - GridUtil.offsetAccess(leftGhostAccess, 2, dim))))

    var rightDir = Array(0, 0, 0); rightDir(dim) = 1
    val rightNeighIndex = Fragment.getNeigh(rightDir).index

    var rightGhostIndex = new MultiIndex(0, 0, 0, 0); rightGhostIndex(dim) = numCellsPerFrag + 2
    val rightGhostAccess = FieldAccess(FieldSelection(field, field.level, 0), rightGhostIndex)

    val rightBoundaryUpdate = new ConditionStatement(
      NegationExpression(iv.NeighborIsValid(field.domain.index, rightNeighIndex)),
      ListBuffer[Statement](
        AssignmentStatement(GridUtil.offsetAccess(rightGhostAccess, -1, dim),
          2 * GridUtil.offsetAccess(rightGhostAccess, -2, dim) - GridUtil.offsetAccess(rightGhostAccess, -3, dim)),
        AssignmentStatement(Duplicate(rightGhostAccess),
          2 * GridUtil.offsetAccess(rightGhostAccess, -1, dim) - GridUtil.offsetAccess(rightGhostAccess, -2, dim))))

    // compile final loop
    ListBuffer[Statement](
      LoopOverFragments(ListBuffer[Statement](
        LoopOverPoints(field, None, true,
          GridUtil.offsetIndex(MultiIndex(0, 0, 0), -2, dim),
          GridUtil.offsetIndex(MultiIndex(0, 0, 0), -2, dim),
          MultiIndex(1, 1, 1),
          ListBuffer[Statement](
            innerItDecl,
            new ConditionStatement(LowerEqualExpression(innerIt, xf + 1),
              AssignmentStatement(Duplicate(baseAccess),
                0.5 * alpha * innerIt * innerIt + (beta - 0.5 * alpha) * innerIt),
              new ConditionStatement(LowerEqualExpression(innerIt, xs + 1),
                AssignmentStatement(Duplicate(baseAccess),
                  -0.5 * alpha * (xf * xf + xf) + (beta + alpha * xf) * innerIt),
                AssignmentStatement(Duplicate(baseAccess),
                  -0.5 * alpha * innerIt * innerIt
                    + (alpha * xf + alpha * xs + 0.5 * alpha + beta) * innerIt
                    - 0.5 * alpha * (xf * xf + xf + xs * xs + xs)))))),
        leftBoundaryUpdate,
        rightBoundaryUpdate)))
  }

  def setupStagCVWidth(dim : Int, level : Int) : ListBuffer[Statement] = {
    val numCellsPerFrag = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim)
    val numCellsTotal = numCellsPerFrag * Knowledge.domain_rect_numFragsTotalAsVec(dim)

    // look up field and compile access to base element
    var baseIndex = LoopOverDimensions.defIt
    baseIndex(Knowledge.dimensionality) = 0
    val field = FieldCollection.getFieldByIdentifier(s"stag_cv_width_${dimToString(dim)}", level).get
    val baseAccess = FieldAccess(FieldSelection(field, field.level, 0), Duplicate(baseIndex))
    val npField = FieldCollection.getFieldByIdentifier(s"node_pos_${dimToString(dim)}", level).get
    val npBaseAccess = FieldAccess(FieldSelection(npField, npField.level, 0), Duplicate(baseIndex))

    // fix the inner iterator -> used for zone checks
    def innerIt =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        LoopOverDimensions.defIt(dim)
      else
        VariableAccess(s"global_${dimToString(dim)}", Some(IntegerDatatype))
    val innerItDecl =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        NullStatement
      else
        new VariableDeclarationStatement(innerIt.asInstanceOf[VariableAccess], LoopOverDimensions.defIt(dim) + ArrayAccess(iv.PrimitiveIndex(), dim) * numCellsPerFrag)

    // compile special boundary handling expressions
    var leftDir = Array(0, 0, 0); leftDir(dim) = -1
    val leftNeighIndex = Fragment.getNeigh(leftDir).index

    var leftGhostIndex = new MultiIndex(0, 0, 0, 0); leftGhostIndex(dim) = -2
    val leftGhostAccess = FieldAccess(FieldSelection(field, field.level, 0), leftGhostIndex)

    val leftBoundaryUpdate = new ConditionStatement(
      NegationExpression(iv.NeighborIsValid(field.domain.index, leftNeighIndex)),
      ListBuffer[Statement](
        AssignmentStatement(GridUtil.offsetAccess(leftGhostAccess, 1, dim), GridUtil.offsetAccess(leftGhostAccess, 2, dim)),
        AssignmentStatement(Duplicate(leftGhostAccess), GridUtil.offsetAccess(leftGhostAccess, 1, dim))))

    var rightDir = Array(0, 0, 0); rightDir(dim) = 1
    val rightNeighIndex = Fragment.getNeigh(rightDir).index

    var rightGhostIndex = new MultiIndex(0, 0, 0, 0); rightGhostIndex(dim) = numCellsPerFrag + 2
    val rightGhostAccess = FieldAccess(FieldSelection(field, field.level, 0), rightGhostIndex)

    val rightBoundaryUpdate = new ConditionStatement(
      NegationExpression(iv.NeighborIsValid(field.domain.index, rightNeighIndex)),
      ListBuffer[Statement](
        AssignmentStatement(GridUtil.offsetAccess(rightGhostAccess, -1, dim), GridUtil.offsetAccess(rightGhostAccess, -2, dim)),
        AssignmentStatement(Duplicate(rightGhostAccess), GridUtil.offsetAccess(rightGhostAccess, -1, dim))))

    // compile final loop
    ListBuffer[Statement](
      LoopOverFragments(ListBuffer[Statement](
        LoopOverPoints(field, None, true,
          GridUtil.offsetIndex(MultiIndex(0, 0, 0), -1, dim),
          GridUtil.offsetIndex(MultiIndex(0, 0, 0), -1, dim),
          MultiIndex(1, 1, 1),
          ListBuffer[Statement](
            innerItDecl,
            new ConditionStatement(EqEqExpression(0, innerIt),
              AssignmentStatement(Duplicate(baseAccess),
                0.5 * (Duplicate(npBaseAccess) + GridUtil.offsetAccess(npBaseAccess, 1, dim))
                  - Duplicate(npBaseAccess)),
              new ConditionStatement(EqEqExpression(numCellsTotal, innerIt),
                AssignmentStatement(Duplicate(baseAccess),
                  Duplicate(npBaseAccess)
                    - 0.5 * (GridUtil.offsetAccess(npBaseAccess, -1, dim) + Duplicate(npBaseAccess))),
                AssignmentStatement(Duplicate(baseAccess),
                  0.5 * (Duplicate(npBaseAccess) + GridUtil.offsetAccess(npBaseAccess, 1, dim))
                    - 0.5 * (GridUtil.offsetAccess(npBaseAccess, -1, dim) + Duplicate(npBaseAccess))))))),
        leftBoundaryUpdate,
        rightBoundaryUpdate)))
  }
}
