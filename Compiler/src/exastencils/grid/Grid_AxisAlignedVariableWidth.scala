package exastencils.grid

import scala.annotation.migration
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._

object Grid_AxisAlignedVariableWidth extends Grid {
  override def initL4() : Unit = {
    val root = StateManager.root_.asInstanceOf[l4.Root]
    root.fieldLayouts += l4.LayoutDeclarationStatement(
      l4.LeveledIdentifier("DefNodeLineLayout", l4.FinestLevelSpecification()),
      l4.RealDatatype(), "Edge_Node".toLowerCase(),
      Some(l4.Index3D(1, 0, 0)), None,
      Some(l4.Index3D(1, 0, 0)), None,
      None)

    root.fields += l4.FieldDeclarationStatement(
      l4.LeveledIdentifier("node_pos_x", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout", None, 1, 0)
    if (Knowledge.dimensionality > 1)
      root.fields += l4.FieldDeclarationStatement(
        l4.LeveledIdentifier("node_pos_y", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout", None, 1, 0)
    if (Knowledge.dimensionality > 2)
      root.fields += l4.FieldDeclarationStatement(
        l4.LeveledIdentifier("node_pos_z", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout", None, 1, 0)

    root.fields += l4.FieldDeclarationStatement(
      l4.LeveledIdentifier("stag_cv_width_x", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout", None, 1, 0)
    if (Knowledge.dimensionality > 1)
      root.fields += l4.FieldDeclarationStatement(
        l4.LeveledIdentifier("stag_cv_width_y", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout", None, 1, 0)
    if (Knowledge.dimensionality > 2)
      root.fields += l4.FieldDeclarationStatement(
        l4.LeveledIdentifier("stag_cv_width_z", l4.FinestLevelSpecification()), "global", "DefNodeLineLayout", None, 1, 0)
  }

  override def generateInitCode() = {
    /// node_pos      -> nodes of the original grid
    /// o   o   o   o   o
    /// cell_width      -> width of the control volumes of the original grid
    /// |---|   |---|
    /// stag_cv_width   -> width of the staggered control volumes
    /// |-|   |---|   |-|

    (0 until Knowledge.dimensionality).to[ListBuffer].map(dim => setupNodePos(dim, Knowledge.maxLevel)) ++
      (0 until Knowledge.dimensionality).to[ListBuffer].map(dim => setupStagCVWidth(dim, Knowledge.maxLevel))
  }

  def setupNodePos(dim : Integer, level : Integer) : Statement = {
    val expo = 1.5
    val numCells = (1 << level) // TODO: adapt for non-unit fragments
    val zoneSize = numCells / 4
    val step = 1.0 / zoneSize

    val field = FieldCollection.getFieldByIdentifier(s"node_pos_${dimToString(dim)}", level).get
    var baseIndex = LoopOverDimensions.defIt
    baseIndex(Knowledge.dimensionality) = 0
    val baseAccess = FieldAccess(FieldSelection(field, field.level, 0), baseIndex)

    val innerIt = LoopOverDimensions.defIt(0)

    LoopOverPoints(field, None, true,
      MultiIndex(-2, -1, -1), MultiIndex(-2, -1, -1), MultiIndex(1, 1, 1),
      ListBuffer[Statement](
        new ConditionStatement(LowerEqualExpression(innerIt, 0),
          AssignmentStatement(Duplicate(baseAccess), 0.0),
          new ConditionStatement(LowerEqualExpression(innerIt, 1 * zoneSize),
            AssignmentStatement(Duplicate(baseAccess), offsetAccess(baseAccess, -1 * innerIt + 0 * zoneSize, 0)
              + 0.0095 * FunctionCallExpression("pow", ListBuffer[Expression](step * (LoopOverDimensions.defIt(0) - 0.0 * zoneSize), expo))),
            new ConditionStatement(LowerEqualExpression(innerIt, 2 * zoneSize),
              AssignmentStatement(Duplicate(baseAccess), offsetAccess(baseAccess, -1 * innerIt + 1 * zoneSize, 0)
                + 0.0095 * step * (LoopOverDimensions.defIt(0) - 1.0 * zoneSize)),
              new ConditionStatement(LowerEqualExpression(innerIt, 3 * zoneSize),
                AssignmentStatement(Duplicate(baseAccess), offsetAccess(baseAccess, -1 * innerIt + 2 * zoneSize, 0)
                  + 0.0095 * step * (LoopOverDimensions.defIt(0) - 2.0 * zoneSize)),
                new ConditionStatement(LowerEqualExpression(innerIt, 4 * zoneSize),
                  AssignmentStatement(Duplicate(baseAccess), offsetAccess(baseAccess, -1 * innerIt + 3 * zoneSize, 0)
                    + 0.0095 * (1.0 - FunctionCallExpression("pow", ListBuffer[Expression](1.0 - step * (LoopOverDimensions.defIt(0) - 3.0 * zoneSize), expo)))),
                  AssignmentStatement(Duplicate(baseAccess), offsetAccess(baseAccess, -1, 0)))))))))
  }

  def setupStagCVWidth(dim : Integer, level : Integer) : Statement = {
    val expo = 1.5
    val numCells = (1 << level) // TODO: adapt for non-unit fragments
    val zoneSize = numCells / 4
    val step = 1.0 / zoneSize

    var baseIndex = LoopOverDimensions.defIt
    baseIndex(Knowledge.dimensionality) = 0
    val field = FieldCollection.getFieldByIdentifier(s"stag_cv_width_${dimToString(dim)}", level).get
    val baseAccess = FieldAccess(FieldSelection(field, field.level, 0), Duplicate(baseIndex))
    val npField = FieldCollection.getFieldByIdentifier(s"node_pos_${dimToString(dim)}", level).get
    val npBaseAccess = FieldAccess(FieldSelection(npField, npField.level, 0), Duplicate(baseIndex))

    val innerIt = LoopOverDimensions.defIt(0)

    LoopOverPoints(field, None, true,
      MultiIndex(-1, -1, -1), MultiIndex(-1, -1, -1), MultiIndex(1, 1, 1),
      ListBuffer[Statement](
        new ConditionStatement(EqEqExpression(0, innerIt),
          AssignmentStatement(Duplicate(baseAccess),
            0.5 * (Duplicate(npBaseAccess) + offsetAccess(npBaseAccess, 1, 0))
              - Duplicate(npBaseAccess)),
          new ConditionStatement(EqEqExpression(numCells, innerIt),
            AssignmentStatement(Duplicate(baseAccess),
              Duplicate(npBaseAccess)
                - 0.5 * (offsetAccess(npBaseAccess, -1, 0) + Duplicate(npBaseAccess))),
            AssignmentStatement(Duplicate(baseAccess),
              0.5 * (Duplicate(npBaseAccess) + offsetAccess(npBaseAccess, 1, 0))
                - 0.5 * (offsetAccess(npBaseAccess, -1, 0) + Duplicate(npBaseAccess)))))))
  }

  override def resolveGridMemberFunction(name : String) : Option[java.lang.reflect.Method] = {
    this.getClass().getMethods.find(_.getName.toLowerCase() == name.toLowerCase())
  }

  override def invokeEvalResolve(functionName : String, fieldAccess : FieldAccess, interpolation : String) : Expression = {
    val method = this.getClass().getMethods.find(_.getName == functionName)
    if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
    method.get.invoke(this, fieldAccess, interpolation).asInstanceOf[Expression]
  }

  override def invokeIntegrateResolve(functionName : String, exp : Expression) : Expression = {
    val method = this.getClass().getMethods.find(_.getName == functionName)
    if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
    method.get.invoke(this, exp).asInstanceOf[Expression]
  }

  def projectIdx(baseIndex : MultiIndex, dim : Int) = {
    new MultiIndex(baseIndex(dim), 0, 0, 0)
  }

  def offsetIndex(index : MultiIndex, offset : Expression, dim : Int) : MultiIndex = {
    var modIndex = Duplicate(index)
    modIndex(dim) += offset
    modIndex
  }
  def offsetAccess(fieldAccess : FieldAccess, offset : Expression, dim : Int) : FieldAccess = {
    var modAccess = Duplicate(fieldAccess)
    modAccess.index(dim) += offset
    modAccess
  }

  // direct accesses
  def nodePosition(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = FieldCollection.getFieldByIdentifierLevExp(s"node_pos_${dimToString(dim)}", level).get
    FieldAccess(FieldSelection(field, field.level, 0, arrayIndex), projectIdx(index, dim))
  }

  def stagCVWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = FieldCollection.getFieldByIdentifierLevExp(s"stag_cv_width_${dimToString(dim)}", level).get
    FieldAccess(FieldSelection(field, field.level, 0, arrayIndex), projectIdx(index, dim))
  }

  // compound accesses
  def cellWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    nodePosition(level, offsetIndex(index, 1, dim), arrayIndex, dim) - nodePosition(level, Duplicate(index), arrayIndex, dim)
  }

  def cellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int]) = {
    var exp : Expression = cellWidth(level, index, arrayIndex, 0)
    for (dim <- 1 until Knowledge.dimensionality)
      exp *= cellWidth(level, index, arrayIndex, dim)
    exp
  }

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

  def xStagCellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int]) = staggeredCellVolume(level, index, arrayIndex, 0)
  def yStagCellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int]) = staggeredCellVolume(level, index, arrayIndex, 1)
  def zStagCellVolume(level : Expression, index : MultiIndex, arrayIndex : Option[Int]) = staggeredCellVolume(level, index, arrayIndex, 2)

  def cellCenterToFace(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    0.5 * cellWidth(level, index, arrayIndex, dim)
  }

  // evaluations and interpolations
  def evalAtEastFace(fieldAccess : FieldAccess, interpolation : String) = evalAtRFace(fieldAccess, 0, interpolation)
  def evalAtWestFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 0, interpolation)
  def evalAtNorthFace(fieldAccess : FieldAccess, interpolation : String) = evalAtRFace(fieldAccess, 1, interpolation)
  def evalAtSouthFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 1, interpolation)
  def evalAtTopFace(fieldAccess : FieldAccess, interpolation : String) = evalAtRFace(fieldAccess, 2, interpolation)
  def evalAtBottomFace(fieldAccess : FieldAccess, interpolation : String) = evalAtLFace(fieldAccess, 2, interpolation)

  def evalAtLFace(fieldAccess : FieldAccess, dim : Int, interpolation : String = "default") : Expression = {
    val field = fieldAccess.fieldSelection.field
    val baseIndex = fieldAccess.index
    val level = field.level

    if ("cell" != field.discretization)
      Logger.warn(s"Attempting (${dimToString(dim)}-)face evaluation for non-cell based discretization (field ${field.identifier}, level ${field.level}, discretization ${field.discretization})")

    // compile evaluation
    val a0 = (() => { cellCenterToFace(level, Duplicate(baseIndex), None, dim) })
    val a1 = (() => { cellCenterToFace(level, offsetIndex(baseIndex, -1, dim), None, dim) })
    val x0 = (() => { Duplicate(fieldAccess) })
    val x1 = (() => { offsetAccess(fieldAccess, -1, dim) })

    interpolation match {
      case "linear" | "default" => (a1() * x0() + a0() * x1()) / (a0() + a1())
      case "harmonicMean"       => ((a0() + a1()) * (x0() * x1())) / (a1() * x0() + a0() * x1())
      case _ =>
        Logger.warn(s"Trying to use interpolation scheme $interpolation which is unknown - falling back to default scheme")
        evalAtLFace(fieldAccess, dim)
    }
  }

  def evalAtRFace(fieldAccess : FieldAccess, dim : Int, interpolation : String = "default") = evalAtLFace(offsetAccess(fieldAccess, 1, dim), dim, interpolation)

  // integrations
  def integrateOverEastFace(exp : Expression) : Expression = integrateOverRFace(exp, 0)
  def integrateOverWestFace(exp : Expression) : Expression = integrateOverLFace(exp, 0)
  def integrateOverNorthFace(exp : Expression) : Expression = integrateOverRFace(exp, 1)
  def integrateOverSouthFace(exp : Expression) : Expression = integrateOverLFace(exp, 1)
  def integrateOverTopFace(exp : Expression) : Expression = integrateOverRFace(exp, 2)
  def integrateOverBottomFace(exp : Expression) : Expression = integrateOverLFace(exp, 2)

  def integrateOverXStaggeredEastFace(exp : Expression) : Expression = integrateOverStaggeredRFace(exp, 0, 0)
  def integrateOverXStaggeredWestFace(exp : Expression) : Expression = integrateOverStaggeredLFace(exp, 0, 0)
  def integrateOverXStaggeredNorthFace(exp : Expression) : Expression = integrateOverStaggeredRFace(exp, 0, 1)
  def integrateOverXStaggeredSouthFace(exp : Expression) : Expression = integrateOverStaggeredLFace(exp, 0, 1)
  def integrateOverXStaggeredTopFace(exp : Expression) : Expression = integrateOverStaggeredRFace(exp, 0, 2)
  def integrateOverXStaggeredBottomFace(exp : Expression) : Expression = integrateOverStaggeredLFace(exp, 0, 2)

  def integrateOverYStaggeredEastFace(exp : Expression) : Expression = integrateOverStaggeredRFace(exp, 1, 0)
  def integrateOverYStaggeredWestFace(exp : Expression) : Expression = integrateOverStaggeredLFace(exp, 1, 0)
  def integrateOverYStaggeredNorthFace(exp : Expression) : Expression = integrateOverStaggeredRFace(exp, 1, 1)
  def integrateOverYStaggeredSouthFace(exp : Expression) : Expression = integrateOverStaggeredLFace(exp, 1, 1)
  def integrateOverYStaggeredTopFace(exp : Expression) : Expression = integrateOverStaggeredRFace(exp, 1, 2)
  def integrateOverYStaggeredBottomFace(exp : Expression) : Expression = integrateOverStaggeredLFace(exp, 1, 2)

  def integrateOverZStaggeredEastFace(exp : Expression) : Expression = integrateOverStaggeredRFace(exp, 2, 0)
  def integrateOverZStaggeredWestFace(exp : Expression) : Expression = integrateOverStaggeredLFace(exp, 2, 0)
  def integrateOverZStaggeredNorthFace(exp : Expression) : Expression = integrateOverStaggeredRFace(exp, 2, 1)
  def integrateOverZStaggeredSouthFace(exp : Expression) : Expression = integrateOverStaggeredLFace(exp, 2, 1)
  def integrateOverZStaggeredTopFace(exp : Expression) : Expression = integrateOverStaggeredRFace(exp, 2, 2)
  def integrateOverZStaggeredBottomFace(exp : Expression) : Expression = integrateOverStaggeredLFace(exp, 2, 2)

  def integrateOverLFace(exp : Expression, faceDim : Int) : Expression = {
    ShiftFieldAccessIndices.offset = -1
    ShiftFieldAccessIndices.dim = faceDim
    ShiftFieldAccessIndices.applyStandalone(exp)

    integrateOverRFace(exp, faceDim)
  }

  def integrateOverRFace(exp : Expression, faceDim : Int) : Expression = {
    val compDim0 = (if (0 == faceDim) 1 else 0)
    val compDim1 = (if (2 == faceDim) 1 else 2)
    exp match {
      case fieldAccess : FieldAccess => {
        val level = fieldAccess.fieldSelection.level
        val index = fieldAccess.index

        fieldAccess.fieldSelection.field.discretization match {
          case "cell" => cellWidth(level, index, None, compDim0) * cellWidth(level, index, None, compDim1) * evalAtRFace(fieldAccess, faceDim)
        }
      }
      case MultiplicationExpression(leftFieldAccess : FieldAccess, rightFieldAccess : FieldAccess) => {
        if (leftFieldAccess.fieldSelection.level != rightFieldAccess.fieldSelection.level)
          Logger.warn(s"Mix level field integration is currently not supported ($leftFieldAccess, $rightFieldAccess)")
        val level = leftFieldAccess.fieldSelection.level

        (leftFieldAccess.fieldSelection.field.discretization, rightFieldAccess.fieldSelection.field.discretization) match {
          case (leftDisc, "cell") if leftDisc == s"face_${dimToString(faceDim)}" => {
            val rightIndex = rightFieldAccess.index
            (cellWidth(level, rightIndex, None, compDim0) * cellWidth(level, rightIndex, None, compDim1)
              * offsetAccess(leftFieldAccess, 1, faceDim) * evalAtRFace(rightFieldAccess, faceDim))
          }
        }
      }
      case _ => {
        Logger.warn(s"Integration over staggered faces for expression ${exp.prettyprint} is currently not supported")
        exp
      }
    }
  }

  def integrateOverStaggeredLFace(exp : Expression, stagDim : Int, faceDim : Int) : Expression = {
    ShiftFieldAccessIndices.offset = -1
    ShiftFieldAccessIndices.dim = faceDim
    ShiftFieldAccessIndices.applyStandalone(exp)

    integrateOverStaggeredRFace(exp, stagDim, faceDim)
  }

  def integrateOverStaggeredRFace(exp : Expression, stagDim : Int, faceDim : Int) : Expression = {
    // check if there are any field accesses in the current (sub-)expression
    CollectFieldAccesses.applyStandalone(new Scope(exp))

    // TODO: find a way to handle constants
    if (0 == CollectFieldAccesses.fieldAccesses.size) {
      Logger.warn(s"Trying to evaluate index-independent expression ${exp.prettyprint} - currently unsupported")
      return exp
    }

    // check if all occurring level specifications are identical
    val level = CollectFieldAccesses.fieldAccesses(0).fieldSelection.level
    for (fieldAccess <- CollectFieldAccesses.fieldAccesses) {
      if (level != fieldAccess.fieldSelection.level) {
        Logger.warn(s"Mixed level field integration is currently not supported (${exp.prettyprint})")
        return exp
      }
    }
    for (fieldAccess <- CollectFieldAccesses.vFieldAccesses) { // check virtual fields as well
      if (level != fieldAccess.level) {
        Logger.warn(s"Mixed level field integration is currently not supported (${exp.prettyprint})")
        return exp
      }
    }

    // filter cases in which only one field access happens
    if (1 == CollectFieldAccesses.fieldAccesses.size) {
      val index = (() => { Duplicate(CollectFieldAccesses.fieldAccesses(0).index) })
      val discr = CollectFieldAccesses.fieldAccesses(0).fieldSelection.field.discretization

      return discr match {
        case "cell" =>
          if (stagDim == faceDim) { // evaluation of a cell centered value w.r.t. a staggered face (in the correct dim) is simply sampling
            val compDim0 = (if (0 == faceDim) 1 else 0)
            val compDim1 = (if (2 == faceDim) 1 else 2)

            (VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim0)}", level, index())
              * VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim1)}", level, index())
              * exp)
          } else { // stagDim != faceDim => piecewise integration is required
            val compDim = (if (0 != faceDim && 0 != stagDim) 0 else (if (1 != faceDim && 1 != stagDim) 1 else 2))

            val centerExp = Duplicate(exp)
            val offsetExp = Duplicate(exp)

            ShiftFieldAccessIndices.offset = -1
            ShiftFieldAccessIndices.dim = stagDim
            ShiftFieldAccessIndices.applyStandalone(offsetExp)

            (VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim)}", level, index()) *
              (VirtualFieldAccess(s"vf_cellCenterToFace_${dimToString(stagDim)}", level, index()) * centerExp
                + VirtualFieldAccess(s"vf_cellCenterToFace_${dimToString(stagDim)}", level, offsetIndex(index(), -1, stagDim)) * offsetExp))
          }
        case _ =>
          Logger.warn(s"Integration over staggered faces for expression ${exp.prettyprint} is currently not supported")
          exp
      }
    }

    // more complicated case => match current expression and process it in parts
    return exp match {
      case AdditionExpression(left, right) => // addition components can be integrated separately
        integrateOverStaggeredRFace(left, stagDim, faceDim) + integrateOverStaggeredRFace(right, stagDim, faceDim)

      case MultiplicationExpression(left : Number, right) => // resolve multiplications with constants
        left * integrateOverStaggeredRFace(right, stagDim, faceDim)
      case MultiplicationExpression(left, right : Number) => // resolve multiplications with constants
        right * integrateOverStaggeredRFace(left, stagDim, faceDim)

      case MultiplicationExpression(left : FieldAccess, right : FieldAccess) if (
        s"face_${dimToString(faceDim)}" == left.fieldSelection.field.discretization
        && "cell" == right.fieldSelection.field.discretization) => {

        val index = (() => Duplicate(right.index))

        if (faceDim == stagDim) {
          val compDim0 = (if (0 == faceDim) 1 else 0)
          val compDim1 = (if (2 == faceDim) 1 else 2)

          (VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim0)}", level, index())
            * VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim1)}", level, index())
            * 0.5 * (Duplicate(left) + offsetAccess(left, 1, faceDim)) * Duplicate(right))
        } else { // faceDim != stagDim
          val compDim = (if (0 != faceDim && 0 != stagDim) 0 else (if (1 != faceDim && 1 != stagDim) 1 else 2))

          // eval cell value at face points; multiply with face values; multiply with dist to (original) cell interface; add up
          (VirtualFieldAccess(s"vf_cellWidth_${dimToString(compDim)}", level, index()) *
            ((VirtualFieldAccess(s"vf_cellCenterToFace_${dimToString(stagDim)}", level, index())
              * offsetAccess(left, 1, faceDim)
              * evalAtRFace(Duplicate(right), faceDim))
              + (VirtualFieldAccess(s"vf_cellCenterToFace_${dimToString(stagDim)}", level, offsetIndex(index(), -1, stagDim))
                * offsetAccess(offsetAccess(left, -1, stagDim), 1, faceDim)
                * evalAtRFace(offsetAccess(right, -1, stagDim), faceDim))))
        }
      }
      case MultiplicationExpression(left : FieldAccess, right : FieldAccess) if (
        "cell" == right.fieldSelection.field.discretization
        && s"face_${dimToString(faceDim)}" == left.fieldSelection.field.discretization) => {
        integrateOverStaggeredRFace(MultiplicationExpression(right, left), stagDim, faceDim)
      }

      case _ => {
        Logger.warn(s"Integration over staggered faces for expression ${exp.prettyprint} is currently not supported")
        exp
      }
    }
  }
}
