package exastencils.communication.ir

import scala.collection.immutable.HashMap

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.field.ir.IR_Field
import exastencils.grid.ir.IR_Localization

/* caches for packing with interp */

/// InterpPackingCaches
// caches computes weights, base positions and base values

object IR_InterpPackingCaches {
  private var weightCache : HashMap[(IR_Expression, BasePositions), BaseWeights] = HashMap()
  private var positionCache : HashMap[(Int, IR_Localization, Array[Int], IR_ExpressionIndex, BaseShifts), BasePositions] = HashMap()
  private var valuesCache : HashMap[(IR_Field, IR_Expression, Array[Int], IR_Expression, BaseShifts), BaseValues] = HashMap()

  def areWeightsCached(x0 : IR_Expression, basePos : BasePositions) : Boolean = {
    weightCache.contains((x0, basePos))
  }
  def addWeightsToCache(x0 : IR_Expression, basePos : BasePositions, weights : BaseWeights) : Unit = {
    weightCache += ((x0, basePos) -> weights)
  }
  def getWeightsFromCache(x0 : IR_Expression, basePos : BasePositions) : BaseWeights = {
    weightCache((x0, basePos))
  }

  def arePositionsCached(level : Int, localization : IR_Localization, dir : Array[Int], origin : IR_ExpressionIndex, shifts : BaseShifts) : Boolean = {
    positionCache.contains((level, localization, dir, origin, shifts))
  }
  def addPositionsToCache(level : Int, localization : IR_Localization, dir : Array[Int], origin : IR_ExpressionIndex, shifts : BaseShifts, basePos : BasePositions) : Unit = {
    positionCache += ((level, localization, dir, origin, shifts) -> basePos)
  }
  def getPositionsFromCache(level : Int, localization : IR_Localization, dir : Array[Int], origin : IR_ExpressionIndex, shifts : BaseShifts) : BasePositions = {
    positionCache((level, localization, dir, origin, shifts))
  }

  def areValuesCached(field : IR_Field, slot : IR_Expression, dir : Array[Int], origin : IR_ExpressionIndex, shifts : BaseShifts) : Boolean = {
    valuesCache.contains((field, slot, dir, origin, shifts))
  }
  def addValuesToCache(field : IR_Field, slot : IR_Expression, dir : Array[Int], origin : IR_ExpressionIndex, shifts : BaseShifts, baseVal : BaseValues) : Unit = {
    valuesCache += ((field, slot, dir, origin, shifts) -> baseVal)
  }
  def getValuesFromCache(field : IR_Field, slot : IR_Expression, dir : Array[Int], origin : IR_ExpressionIndex, shifts : BaseShifts) : BaseValues = {
    valuesCache((field, slot, dir, origin, shifts))
  }
}
