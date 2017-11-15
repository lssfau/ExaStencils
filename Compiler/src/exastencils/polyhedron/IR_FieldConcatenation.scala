package exastencils.polyhedron

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Node
import exastencils.core.Duplicate
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldLayoutPerDim
import exastencils.logger.Logger

case class IR_FieldConcatenation(mergedFieldName : String, fieldsToMerge : ListBuffer[IR_Field]) extends IR_Node {

  if (fieldsToMerge.size < 2)
    Logger.error("there must be at least to fields to merge")

  val level : Int = fieldsToMerge(0).level
  val slots : Int = fieldsToMerge(0).numSlots

  // check validity and update attributes of newField
  {
    val it = fieldsToMerge.iterator
    val first = it.next()
    val datatype = first.gridDatatype

    while (it.hasNext) {
      val next = it.next()
      if (level != next.level)
        Logger.error("levels of fields to merge do not match!")
      if (slots != next.numSlots)
        Logger.error("number of slots of fields to merge do not match!")
      if (datatype != next.gridDatatype)
        Logger.error("datatypes of fields to merge do not match!")
    }
  }

  def computeNewField() : IR_Field = {
    val newField = Duplicate.forceClone(fieldsToMerge(0))
    newField.name = mergedFieldName
    newField.fieldLayout.name = "merged_" + mergedFieldName

    val dim : Int = newField.fieldLayout.numDimsData + 1
    val nLpD = new Array[IR_FieldLayoutPerDim](dim)
    for (i <- 0 until dim)
      nLpD(i) = IR_FieldLayoutPerDim(0, 0, 0, 0, 0, 0, 0)

    // add new dimensions outermost // FIXME: is the last array entry outermost?
    for (f <- fieldsToMerge) {
      val oLpD = f.fieldLayout.layoutsPerDim
      for (i <- 0 until dim - 1) {
        oLpD(i).total match {
          case IR_IntegerConstant(c) =>
            if (c > nLpD(i).numInnerLayers)
              nLpD(i).numInnerLayers = c.toInt
          case _                     =>
            Logger.error(s"size of field $f for dimension $i is not constant")
        }
      }
    }
    nLpD(dim - 1).numInnerLayers = fieldsToMerge.length
    for (i <- 0 until dim)
      nLpD(i).updateTotal()
    newField.fieldLayout.layoutsPerDim = nLpD
    newField
  }
}
