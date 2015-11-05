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
    val compDim0 = (if (0 == faceDim) 1 else 0)
    val compDim1 = (if (2 == faceDim) 1 else 2)
    exp match {
      case fieldAccess : FieldAccess => {
        val level = fieldAccess.fieldSelection.level
        val index = fieldAccess.index

        fieldAccess.fieldSelection.field.discretization match {
          case "cell" => cellWidth(level, index, None, compDim0) * cellWidth(level, index, None, compDim1) * evalAtLFace(fieldAccess, faceDim)
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
              * leftFieldAccess * evalAtLFace(rightFieldAccess, faceDim))
          }
        }
      }
      case _ => {
        Logger.warn(s"Integration over staggered faces for expression ${exp.prettyprint} is currently not supported")
        exp
      }
    }
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
    exp match {
      case MultiplicationExpression(leftFieldAccess : FieldAccess, rightFieldAccess : FieldAccess) => {
        if (leftFieldAccess.fieldSelection.level != rightFieldAccess.fieldSelection.level)
          Logger.warn(s"Mix level field integration is currently not supported ($leftFieldAccess, $rightFieldAccess)")
        val level = leftFieldAccess.fieldSelection.level

        (leftFieldAccess.fieldSelection.field.discretization, rightFieldAccess.fieldSelection.field.discretization) match {
          case (leftDisc, "cell") if leftDisc == s"face_${dimToString(faceDim)}" => {
            if (faceDim == stagDim) {
              val compDim0 = (if (0 == faceDim) 1 else 0)
              val compDim1 = (if (2 == faceDim) 1 else 2)
              val cellIndex = rightFieldAccess.index
              (cellWidth(level, cellIndex, None, compDim0) * cellWidth(level, cellIndex, None, compDim1)
                * 0.5 * (offsetAccess(leftFieldAccess, -1, faceDim) + Duplicate(leftFieldAccess)) * Duplicate(rightFieldAccess))
            } else { // 0 != stagDim
              val compDim = (if (0 != faceDim && 0 != stagDim) 0 else (if (1 != faceDim && 1 != stagDim) 1 else 2))
              val cellIndex = rightFieldAccess.index
              // eval cell value at face points; multiply with face values; multiply with dist to (original) cell interface; add up
              (cellWidth(level, cellIndex, None, compDim) *
                ((cellCenterToFace(level, cellIndex, None, stagDim)
                  * Duplicate(leftFieldAccess)
                  * evalAtLFace(Duplicate(rightFieldAccess), faceDim))
                  + (cellCenterToFace(level, offsetIndex(cellIndex, -1, stagDim), None, stagDim)
                    * offsetAccess(leftFieldAccess, -1, stagDim)
                    * evalAtLFace(offsetAccess(rightFieldAccess, -1, stagDim), faceDim))))
            }
          }
          case _ => {
            Logger.warn(s"Integration over staggered faces for expression ${exp.prettyprint} is currently not supported")
            exp
          }
        }
      }
    }
  }
  def integrateOverStaggeredRFace(exp : Expression, stagDim : Int, faceDim : Int) : Expression = {
    // TODO: check correct duplication of shared information
    exp match {
      case fieldAccess : FieldAccess => {
        val level = fieldAccess.fieldSelection.level
        fieldAccess.fieldSelection.field.discretization match {
          case "cell" if (stagDim == faceDim) => {
            val compDim0 = (if (0 == faceDim) 1 else 0)
            val compDim1 = (if (2 == faceDim) 1 else 2)
            val index = fieldAccess.index
            (cellWidth(level, index, None, compDim0) * cellWidth(level, index, None, compDim1)
              * Duplicate(fieldAccess))
          }
        }
      }

      case functionCall : FunctionCallExpression => {
        if (stagDim == faceDim) {
          ???
        } else {
          val compDim = (if (0 != faceDim && 0 != stagDim) 0 else (if (1 != faceDim && 1 != stagDim) 1 else 2))

          // scan for fields and extract level and index
          object CollectFields extends DefaultStrategy("Collecting field accesses") {
            var fieldAccesses : ListBuffer[FieldAccess] = ListBuffer()

            override def apply(node : Option[Node] = None) = {
              fieldAccesses.clear
              super.apply(node)
            }

            override def applyStandalone(node : Node) = {
              fieldAccesses.clear
              super.applyStandalone(node)
            }

            this += new Transformation("Collecting", {
              case fieldAccess : FieldAccess =>
                fieldAccesses += fieldAccess
                fieldAccess
            })
          }
          CollectFields.applyStandalone(functionCall)
          if (CollectFields.fieldAccesses.size != 1) ???

          val level = CollectFields.fieldAccesses(0).fieldSelection.level
          val cellIndex = CollectFields.fieldAccesses(0).index

          val centerExp = Duplicate(functionCall)
          val offsetExp = Duplicate(functionCall)

          object ShiftFieldAccessIndices extends DefaultStrategy("Shifting indices of field accesses") {
            var offset : Expression = 0
            var dim : Int = 0

            this += new Transformation("Searching and shifting", {
              case fieldAccess : FieldAccess =>
                fieldAccess.index(dim) += offset
                fieldAccess
            })
          }

          ShiftFieldAccessIndices.offset = -1
          ShiftFieldAccessIndices.dim = stagDim
          ShiftFieldAccessIndices.applyStandalone(offsetExp)

          (cellWidth(level, cellIndex, None, compDim) *
            (cellCenterToFace(level, cellIndex, None, stagDim) * centerExp
              + cellCenterToFace(level, offsetIndex(cellIndex, -1, stagDim), None, stagDim) * offsetExp))
        }

        //            Val diffnu : Real = vf_cellWidth_z@current * integrateOverXStaggeredNorthFace ( evalAtNorthFace ( vis@current, "harmonicMean" ) ) / vf_cellWidth_y@current
        //    Val diffnu : Real = vf_cellWidth_z@current * (
        //      vf_cellCenterToFace_x@current * evalAtNorthFace ( vis@current, "harmonicMean" )
        //      + vf_cellCenterToFace_x@current@[-1, 0, 0] * evalAtNorthFace ( vis@current@[-1, 0, 0], "harmonicMean" )
        //      ) / vf_cellWidth_y@current

      }

      case MultiplicationExpression(leftFieldAccess : FieldAccess, rightFieldAccess : FieldAccess) => {
        if (leftFieldAccess.fieldSelection.level != rightFieldAccess.fieldSelection.level)
          Logger.warn(s"Mix level field integration is currently not supported ($leftFieldAccess, $rightFieldAccess)")
        val level = leftFieldAccess.fieldSelection.level

        (leftFieldAccess.fieldSelection.field.discretization, rightFieldAccess.fieldSelection.field.discretization) match {
          case (leftDisc, "cell") if leftDisc == s"face_${dimToString(faceDim)}" => {
            if (faceDim == stagDim) {
              val compDim0 = (if (0 == faceDim) 1 else 0)
              val compDim1 = (if (2 == faceDim) 1 else 2)
              val cellIndex = rightFieldAccess.index
              (cellWidth(level, cellIndex, None, compDim0) * cellWidth(level, cellIndex, None, compDim1)
                * 0.5 * (Duplicate(leftFieldAccess) + offsetAccess(leftFieldAccess, 1, faceDim)) * Duplicate(rightFieldAccess))
            } else { // 0 != stagDim
              val compDim = (if (0 != faceDim && 0 != stagDim) 0 else (if (1 != faceDim && 1 != stagDim) 1 else 2))
              val cellIndex = rightFieldAccess.index
              // eval cell value at face points; multiply with face values; multiply with dist to (original) cell interface; add up
              (cellWidth(level, cellIndex, None, compDim) *
                ((cellCenterToFace(level, cellIndex, None, stagDim)
                  * offsetAccess(leftFieldAccess, 1, faceDim)
                  * evalAtRFace(Duplicate(rightFieldAccess), faceDim))
                  + (cellCenterToFace(level, offsetIndex(cellIndex, -1, stagDim), None, stagDim)
                    * offsetAccess(offsetAccess(leftFieldAccess, -1, stagDim), 1, faceDim)
                    * evalAtRFace(offsetAccess(rightFieldAccess, -1, stagDim), faceDim))))
            }
          }
          case _ => {
            Logger.warn(s"Integration over staggered faces for expression ${exp.prettyprint} is currently not supported")
            exp
          }
        }
      }
    }
  }
}
