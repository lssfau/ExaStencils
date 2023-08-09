package exastencils.communication.ir

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.communication.DefaultNeighbors
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.field.ir.IR_Field
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_Localization
import exastencils.grid.ir.IR_VF_CellCenterPerDim
import exastencils.grid.ir.IR_VF_CellWidthPerDim
import exastencils.grid.ir.IR_VirtualFieldAccess
import exastencils.logger.Logger

/// IR_InterpPackingHelper
// contains helper functions for packing with interpolation

object IR_InterpPackingHelper {
  // get all neighbor directions that are orthogonal to the communication direction
  def getOrthogonalNeighborDirs(commDir : Array[Int]) : ListBuffer[Array[Int]] = {
    DefaultNeighbors.neighbors.map(_.dir).filter(dir => !(dir sameElements commDir) && !(dir sameElements commDir.map(_ * -1)))
  }

  // get dimension that a unit vector points to
  def getDimFromDir(dir : Array[Int]) : Int = {
    if (dir.map(math.abs).sum != 1)
      Logger.error("Invalid direction used for quadratic interp: " + dir.mkString("Array(", ", ", ")"))

    // assume only axis directions -> first non-zero entry of array is requested dimension
    dir.indexWhere(_ != 0)
  }

  // check vector points in upwind direction
  def isUpwindDir(dir : Array[Int]) : Boolean = dir(getDimFromDir(dir)) > 0

  // prettyprint function for direction arrays
  def dirToString(dir : Array[Int]) : String = dir match {
    case Array(-1, 0, 0) => "W"
    case Array(1, 0, 0)  => "E"
    case Array(0, -1, 0) => "S"
    case Array(0, 1, 0)  => "N"
    case Array(0, 0, -1) => "B"
    case Array(0, 0, 1)  => "T"
  }

  // adds two arrays
  def dirSum(a : Array[Int], b : Array[Int]) : Array[Int] = (a, b).zipped.map(_ + _)

  // get cell center positions
  def getCellCenter(level : Int, dim : Int, origin : IR_ExpressionIndex, offset : IR_ExpressionIndex = IR_ExpressionIndex(0)) : IR_VirtualFieldAccess =
    IR_VF_CellCenterPerDim.access(level, dim, origin + offset)

  // get cell widths
  def getCellWidth(level : Int, dim : Int, origin : IR_ExpressionIndex, offset : IR_ExpressionIndex = IR_ExpressionIndex(0)) : IR_Expression =
    IR_VF_CellWidthPerDim.access(level, dim, origin + offset)
}

/// IR_InterpPackingBaseHelper

object IR_InterpPackingBaseHelper {
  import exastencils.communication.ir.IR_InterpPackingCaches._
  import exastencils.communication.ir.IR_InterpPackingHelper._

  private def createBaseValues(vals : IR_Expression*) : BaseValues = vals.length match {
    case 2 =>
      LinearBaseValues(vals(0), vals(1))
    case 3 =>
      QuadraticBaseValues(vals(0), vals(1), vals(2))
  }

  private def createBasePositions(positions : IR_Expression*) = positions.length match {
    case 2 =>
      LinearBasePositions(positions(0), positions(1))
    case 3 =>
      QuadraticBasePositions(positions(0), positions(1), positions(2))
  }

  def getBaseValues(field : IR_Field, slot : IR_Expression, dir : Array[Int], origin : IR_ExpressionIndex, shifts : BaseShifts) : BaseValues = {
    if (!areValuesCached(field, slot, dir, origin, shifts)) {
      val offsets = shifts.toOffsetArrays(dir)

      val baseVals = shifts.toArray.length match {
        case 3 =>
          createBaseValues(
            IR_DirectFieldAccess(field, Duplicate(slot), origin + IR_ExpressionIndex(offsets(0))),
            IR_DirectFieldAccess(field, Duplicate(slot), origin + IR_ExpressionIndex(offsets(1))),
            IR_DirectFieldAccess(field, Duplicate(slot), origin + IR_ExpressionIndex(offsets(2)))
          )
      }
      addValuesToCache(field, slot, dir, origin, shifts, baseVals)
      baseVals
    } else {
      getValuesFromCache(field, slot, dir, origin, shifts)
    }
  }

  def getBasePositions(level : Int, localization : IR_Localization, dir : Array[Int], origin : IR_ExpressionIndex, shifts : BaseShifts) : BasePositions = {
    if (!arePositionsCached(level, localization, dir, origin, shifts)) {
      val offsets = shifts.toOffsetArrays(dir)
      val dim = getDimFromDir(dir)

      val centerIdx = shifts.toArray.indexWhere(_ == 0)
      val basePos = localization match {
        case IR_AtCellCenter =>
          if (Knowledge.grid_isUniform) {
            // set origin to center cell
            val off = shifts.toArray(centerIdx) * getCellWidth(level, dim, origin)
            createBasePositions(shifts.toArray.map(s => s * getCellWidth(level, dim, origin) - off) : _ *)
          } else {
            // set origin to center cell
            val off = getCellCenter(level, dim, origin, IR_ExpressionIndex(offsets(centerIdx)))
            createBasePositions(offsets.map(o => getCellCenter(level, dim, origin, IR_ExpressionIndex(o)) - off) : _ *)
          }
        case _               =>
          Logger.error("Unsupported localization for quadratic interp in comm for mesh refinement.")
      }
      addPositionsToCache(level, localization, dir, origin, shifts, basePos)
      basePos
    } else {
      getPositionsFromCache(level, localization, dir, origin, shifts)
    }
  }
}

/* basic traits extended for interpolation/extrapolation classes */

/// BaseShifts

trait BaseShifts {
  def toArray : Array[Int]

  def scaleDir(dir : Array[Int], scale : Int) : Array[IR_Expression] = dir.map(_ * scale : IR_Expression)

  def toOffsetArrays(dir : Array[Int]) : Array[Array[IR_Expression]] = {
    Array(scaleDir(dir, toArray(0)), scaleDir(dir, toArray(1)), scaleDir(dir, toArray(2)))
  }
}

/// BasePositions

trait BasePositions {
  def computeWeights(x0 : IR_Expression) : BaseWeights

  def toArray : Array[IR_Expression]
}

/// BaseWeights

trait BaseWeights {
  def toArray : Array[IR_Expression]
}

/// BaseValues

trait BaseValues {
  def toArray : Array[IR_Expression]
}
