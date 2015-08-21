package exastencils.data

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._

abstract class Grid {
  def invokeAccessResolve(specialField : SpecialFieldAccess) : Expression
  def invokeEvalResolve(functionName : String, fieldAccess : FieldAccess) : Expression
  def invokeIntegrateResolve(functionName : String, exp : Expression) : Expression
}

// helper object/method to branch grid types
object Grid {
  def getGridObject : Grid = {
    val gridType = "AxisAlignedVariableWidth" // TODO: move to knowledge
    gridType match {
      case "AxisAlignedVariableWidth" => Grid_AxisAlignedVariableWidth
    }
  }
}

object Grid_AxisAlignedVariableWidth extends Grid {
  // helper method to map names of special fields to actual member functions implementing the resolving step
  override def invokeAccessResolve(specialField : SpecialFieldAccess) : Expression = {
    val functionName = specialField.fieldName
    functionName.substring(functionName.length() - 2) match {
      case "_x" => {
        val method = this.getClass().getMethods.find(_.getName == functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, specialField.level, specialField.index, specialField.arrayIndex, 0 : Integer).asInstanceOf[Expression]
      }
      case "_y" => {
        val method = this.getClass().getMethods.find(_.getName == functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, specialField.level, specialField.index, specialField.arrayIndex, 1 : Integer).asInstanceOf[Expression]
      }
      case "_z" => {
        val method = this.getClass().getMethods.find(_.getName == functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, specialField.level, specialField.index, specialField.arrayIndex, 2 : Integer).asInstanceOf[Expression]
      }
      case _ => {
        val method = this.getClass().getMethods.find(_.getName == functionName)
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, specialField.level, specialField.index, specialField.arrayIndex).asInstanceOf[Expression]
      }
    }
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
  def get_node_pos(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = FieldCollection.getFieldByIdentifierLevExp(s"node_pos_${dimToString(dim)}", level).get
    FieldAccess(FieldSelection(field, field.level, 0, arrayIndex), projectIdx(index, dim))
  }

  def get_stag_cv_width(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    val field = FieldCollection.getFieldByIdentifierLevExp(s"stag_cv_width_${dimToString(dim)}", level).get
    FieldAccess(FieldSelection(field, field.level, 0, arrayIndex), projectIdx(index, dim))
  }

  // compound accesses
  def get_cell_width(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    var offset = MultiIndex(0, 0, 0, 0)
    offset(dim) = 1
    get_node_pos(level, Duplicate(index) + offset, arrayIndex, dim) - get_node_pos(level, Duplicate(index), arrayIndex, dim)
  }

  def get_cell_center_to_face(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) = {
    0.5 * get_cell_width(level, index, arrayIndex, dim)
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
    ((get_cell_center_to_face(level, offsetIndex(baseIndex, -1, dim), None, dim) * Duplicate(fieldAccess)
      + get_cell_center_to_face(level, Duplicate(baseIndex), None, dim) * offsetAccess(fieldAccess, -1, dim))
      / get_stag_cv_width(level, Duplicate(Duplicate(baseIndex)), None, dim))
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
          case "cell" => get_cell_width(level, index, None, compDim0) * get_cell_width(level, index, None, compDim1) * evalAtLFace(fieldAccess, faceDim)
        }
      }
      case MultiplicationExpression(leftFieldAccess : FieldAccess, rightFieldAccess : FieldAccess) => {
        if (leftFieldAccess.fieldSelection.level != rightFieldAccess.fieldSelection.level)
          Logger.warn(s"Mix level field integration is currently not supported ($leftFieldAccess, $rightFieldAccess)")
        val level = leftFieldAccess.fieldSelection.level

        (leftFieldAccess.fieldSelection.field.discretization, rightFieldAccess.fieldSelection.field.discretization) match {
          case (leftDisc, "cell") if leftDisc == s"face_${dimToString(faceDim)}" => {
            val rightIndex = rightFieldAccess.index
            (get_cell_width(level, rightIndex, None, compDim0) * get_cell_width(level, rightIndex, None, compDim1)
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
          case "cell" => get_cell_width(level, index, None, compDim0) * get_cell_width(level, index, None, compDim1) * evalAtRFace(fieldAccess, faceDim)
        }
      }
      case MultiplicationExpression(leftFieldAccess : FieldAccess, rightFieldAccess : FieldAccess) => {
        if (leftFieldAccess.fieldSelection.level != rightFieldAccess.fieldSelection.level)
          Logger.warn(s"Mix level field integration is currently not supported ($leftFieldAccess, $rightFieldAccess)")
        val level = leftFieldAccess.fieldSelection.level

        (leftFieldAccess.fieldSelection.field.discretization, rightFieldAccess.fieldSelection.field.discretization) match {
          case (leftDisc, "cell") if leftDisc == s"face_${dimToString(faceDim)}" => {
            val rightIndex = rightFieldAccess.index
            (get_cell_width(level, rightIndex, None, compDim0) * get_cell_width(level, rightIndex, None, compDim1)
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
              (get_cell_width(level, cellIndex, None, compDim0) * get_cell_width(level, cellIndex, None, compDim1)
                * 0.5 * (offsetAccess(leftFieldAccess, -1, faceDim) + Duplicate(leftFieldAccess)) * Duplicate(rightFieldAccess))
            } else { // 0 != stagDim
              val compDim = (if (0 != faceDim && 0 != stagDim) 0 else (if (1 != faceDim && 1 != stagDim) 1 else 2))
              val cellIndex = rightFieldAccess.index
              // eval cell value at face points; multiply with face values; multiply with dist to (original) cell interface; add up
              (get_cell_width(level, cellIndex, None, compDim) *
                ((get_cell_center_to_face(level, cellIndex, None, stagDim)
                  * Duplicate(leftFieldAccess)
                  * evalAtLFace(Duplicate(rightFieldAccess), faceDim))
                  + (get_cell_center_to_face(level, offsetIndex(cellIndex, -1, stagDim), None, stagDim)
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
              (get_cell_width(level, cellIndex, None, compDim0) * get_cell_width(level, cellIndex, None, compDim1)
                * 0.5 * (Duplicate(leftFieldAccess) + offsetAccess(leftFieldAccess, 1, faceDim)) * Duplicate(rightFieldAccess))
            } else { // 0 != stagDim
              val compDim = (if (0 != faceDim && 0 != stagDim) 0 else (if (1 != faceDim && 1 != stagDim) 1 else 2))
              val cellIndex = rightFieldAccess.index
              // eval cell value at face points; multiply with face values; multiply with dist to (original) cell interface; add up
              (get_cell_width(level, cellIndex, None, compDim) *
                ((get_cell_center_to_face(level, cellIndex, None, stagDim)
                  * offsetAccess(leftFieldAccess, 1, faceDim)
                  * evalAtRFace(Duplicate(rightFieldAccess), faceDim))
                  + (get_cell_center_to_face(level, offsetIndex(cellIndex, -1, stagDim), None, stagDim)
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

object ResolveSpecialFields extends DefaultStrategy("ResolveSpecialFields") {

  this += new Transformation("SearchAndReplace", {
    case specialField : SpecialFieldAccess => Grid.getGridObject.invokeAccessResolve(specialField)
  })
}

object ResolveGeometryFunctions extends DefaultStrategy("ResolveGeometryFunctions") {
  val evalFunctions = ListBuffer(
    "evalAtEastFace", "evalAtWestFace", "evalAtNorthFace", "evalAtSouthFace", "evalAtTopFace", "evalAtBottomFace")
  val integrateFunctions = ListBuffer(
    "integrateOverEastFace", "integrateOverWestFace", "integrateOverNorthFace", "integrateOverSouthFace", "integrateOverTopFace", "integrateOverBottomFace",
    "integrateOverXStaggeredEastFace", "integrateOverXStaggeredNorthFace", "integrateOverXStaggeredTopFace",
    "integrateOverXStaggeredWestFace", "integrateOverXStaggeredSouthFace", "integrateOverXStaggeredBottomFace",
    "integrateOverYStaggeredEastFace", "integrateOverYStaggeredNorthFace", "integrateOverYStaggeredTopFace",
    "integrateOverYStaggeredWestFace", "integrateOverYStaggeredSouthFace", "integrateOverYStaggeredBottomFace",
    "integrateOverZStaggeredEastFace", "integrateOverZStaggeredNorthFace", "integrateOverZStaggeredTopFace",
    "integrateOverZStaggeredWestFace", "integrateOverZStaggeredSouthFace", "integrateOverZStaggeredBottomFace")

  this += new Transformation("SearchAndReplace", {
    case FunctionCallExpression(functionName, args) if evalFunctions.contains(functionName) => {
      if (0 == args.length) {
        Logger.warn(s"Trying to use build-in function $functionName without arguments")
        NullExpression
      } else {
        if (args.length > 1) Logger.warn(s"Trying to use build-in function $functionName with more than one arguments; additional arguments are discarded")
        args(0) match {
          case access : FieldAccess => Grid.getGridObject.invokeEvalResolve(functionName, access)
          case _ => {
            Logger.warn(s"Argument ${args(0).prettyprint} is currently not supported for function $functionName")
            args(0)
          }
        }
      }
    }

    case FunctionCallExpression(functionName, args) if integrateFunctions.contains(functionName) => {
      Grid.getGridObject.invokeIntegrateResolve(functionName, args(0).asInstanceOf[Expression])
      if (0 == args.length) {
        Logger.warn(s"Trying to use build-in function $functionName without arguments")
        NullExpression
      } else {
        if (args.length > 1) Logger.warn(s"Trying to use build-in function $functionName with more than one arguments; additional arguments are discarded")
        Grid.getGridObject.invokeIntegrateResolve(functionName, args(0))
      }
    }
  })
}

