package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_Index
import exastencils.base.ir.IR_Node
import exastencils.logger.Logger

object IR_AccessPattern {
  def apply(callback : IR_Index => IR_Access) : IR_AccessPattern = new IR_AccessPattern(accessCallbackFuntion = callback, accessIndices = None, stridePerDimension = None)
  def apply(callback : IR_Index => IR_Access, accessIndices : Option[ListBuffer[IR_Index]]) : IR_AccessPattern =
    new IR_AccessPattern(accessCallbackFuntion = callback, accessIndices, stridePerDimension = None)
  def apply(callback : IR_Index => IR_Access, stridePerDimension : Option[ListBuffer[IR_Expression]])(implicit d:DummyImplicit) : IR_AccessPattern =
    new IR_AccessPattern(accessCallbackFuntion = callback, accessIndices = None, stridePerDimension = stridePerDimension)
}

case class IR_AccessPattern(
    var accessCallbackFuntion : IR_Index => IR_Access, // callback function to be registered and used by IR_DataBuffer wrapper
    var accessIndices : Option[ListBuffer[IR_Index]], // contains N indices to be accessed for each grid element (e.g. nodes/cells/...)
    var stridePerDimension : Option[ListBuffer[IR_Expression]] // contains the stride value for each dimension
) extends IR_Node {

  def callAccessFunction(index : IR_Index) : IR_Access = accessCallbackFuntion(index)

  if (accessIndices.isDefined && stridePerDimension.isDefined) {
    Logger.error("IR_AccessPattern: Combination of access patterns not implemented yet!")
  }
}

