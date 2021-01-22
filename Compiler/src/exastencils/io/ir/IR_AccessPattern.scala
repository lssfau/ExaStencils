package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Index
import exastencils.base.ir.IR_Node

object IR_AccessPattern {
  def apply(callback : IR_Index => IR_Access) : IR_AccessPattern = new IR_AccessPattern(accessCallbackFuntion = callback, accessIndices = None)
  def apply(callback : IR_Index => IR_Access, accessIndices : Option[ListBuffer[IR_Index]]) : IR_AccessPattern =
    new IR_AccessPattern(accessCallbackFuntion = callback, accessIndices)
}

case class IR_AccessPattern(
    var accessCallbackFuntion : IR_Index => IR_Access, // callback function to be registered and used by IR_DataBuffer wrapper
    var accessIndices : Option[ListBuffer[IR_Index]], // contains N indices to be accessed for each grid element (e.g. nodes/cells/...)
) extends IR_Node {

  def callAccessFunction(index : IR_Index) : IR_Access = accessCallbackFuntion(index)
}

