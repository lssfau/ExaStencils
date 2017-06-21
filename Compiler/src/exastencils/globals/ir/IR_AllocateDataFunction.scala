package exastencils.globals.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.communication.NeighborInfo
import exastencils.field.ir._

/// IR_AllocateDataFunction

object IR_AllocateDataFunction {
  val fctName = "setupBuffers"
}

// TODO: split to separate functions for (host) fields, communication buffers and device data
case class IR_AllocateDataFunction(
    var fields : ListBuffer[IR_Field],
    var neighbors : ListBuffer[NeighborInfo]) extends IR_FuturePlainFunction {

  override var name = IR_AllocateDataFunction.fctName
  override def prettyprint_decl() : String = prettyprint

  override def generateFct() = {
    val body = ListBuffer[IR_Statement]()

    // add static allocations here

    IR_PlainFunction(name, IR_UnitDatatype, body)
  }
}
