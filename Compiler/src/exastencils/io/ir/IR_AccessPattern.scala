package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_Index
import exastencils.logger.Logger

object IR_AccessPattern {
  def apply(callback : IR_Index => IR_Access) : IR_AccessPattern = new IR_AccessPattern(accessCallbackFuntion = callback, accessIndices = None, stridePerDimension = None)
  def apply(callback : IR_Index => IR_Access, accessIndices : ListBuffer[IR_Index]) : IR_AccessPattern =
    new IR_AccessPattern(accessCallbackFuntion = callback, accessIndices = Some(accessIndices), stridePerDimension = None)
  def apply(callback : IR_Index => IR_Access, stridePerDimension : ListBuffer[IR_Expression])(implicit d:DummyImplicit) : IR_AccessPattern =
    new IR_AccessPattern(accessCallbackFuntion = callback, accessIndices = None, stridePerDimension = Some(stridePerDimension))
}

// TODO
case class IR_AccessPattern(
    var accessCallbackFuntion : IR_Index => IR_Access,
    var accessIndices : Option[ListBuffer[IR_Index]],
    var stridePerDimension : Option[ListBuffer[IR_Expression]]) {

  def callAccessFunction(index : IR_Index) : IR_Access = accessCallbackFuntion(index)

  if (accessIndices.isDefined && stridePerDimension.isDefined) {
    Logger.error("IR_AccessPattern: Combination of access patterns not implemented yet!")
  }
}

