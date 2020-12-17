package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_Index

object IR_AccessPattern {
  def apply(callback : IR_Index => IR_Access) : IR_AccessPattern = new IR_AccessPattern(accessCallbackFuntion = callback, accessIndices = None, stridePerDimension = None)
}

// TODO
case class IR_AccessPattern(
    var accessCallbackFuntion : IR_Index => IR_Access,
    var accessIndices : Option[ListBuffer[IR_ExpressionIndex]],
    var stridePerDimension : Option[ListBuffer[IR_Expression]]) {

  def callAccessFunction(index : IR_Index) : IR_Access = accessCallbackFuntion(index)
}

