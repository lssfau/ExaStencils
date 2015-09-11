package exastencils.grid

import exastencils.core._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._

object Grid_AxisAlignedVariableWidth extends Grid {
  override def resolveGridMemberFunction(name : String) : Option[java.lang.reflect.Method] = {
    this.getClass().getMethods.find(_.getName.toLowerCase() == name.toLowerCase())
  }

  def invokeEvalResolve(functionName : String, fieldAccess : FieldAccess) : Expression = {
    val method = this.getClass().getMethods.find(_.getName == functionName)
    if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
    method.get.invoke(this, fieldAccess).asInstanceOf[Expression]
  }

  def invokeIntegrateResolve(functionName : String, exp : Expression) : Expression = {
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
  def evalAtEastFace(fieldAccess : FieldAccess) = evalAtRFace(fieldAccess, 0)
  def evalAtWestFace(fieldAccess : FieldAccess) = evalAtLFace(fieldAccess, 0)
  def evalAtNorthFace(fieldAccess : FieldAccess) = evalAtRFace(fieldAccess, 1)
  def evalAtSouthFace(fieldAccess : FieldAccess) = evalAtLFace(fieldAccess, 1)
  def evalAtTopFace(fieldAccess : FieldAccess) = evalAtRFace(fieldAccess, 2)
  def evalAtBottomFace(fieldAccess : FieldAccess) = evalAtLFace(fieldAccess, 2)

  def evalAtLFace(fieldAccess : FieldAccess, dim : Int) = {
    val field = fieldAccess.fieldSelection.field
    val baseIndex = fieldAccess.index
    val level = field.level

    if ("cell" != field.discretization)
      Logger.warn(s"Attempting (${dimToString(dim)}-)face evaluation for non-cell based discretization (field ${field.identifier}, level ${field.level}, discretization ${field.discretization})")

    // compile evaluation
    ((cellCenterToFace(level, offsetIndex(baseIndex, -1, dim), None, dim) * Duplicate(fieldAccess)
      + cellCenterToFace(level, Duplicate(baseIndex), None, dim) * offsetAccess(fieldAccess, -1, dim))
      / stagCVWidth(level, Duplicate(Duplicate(baseIndex)), None, dim))
  }

  def evalAtRFace(fieldAccess : FieldAccess, dim : Int) = evalAtLFace(offsetAccess(fieldAccess, 1, dim), dim)

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
