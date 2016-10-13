package exastencils.globals.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.communication.NeighborInfo
import exastencils.datastructures.Transformation._
import exastencils.field.ir._
import exastencils.prettyprinting._

/// IR_AllocateDataFunction

object IR_AllocateDataFunction {
  val fctName = "setupBuffers"
}

// TODO: split to separate functions for (host) fields, communication buffers and device data
case class IR_AllocateDataFunction(
    var fields : ListBuffer[IR_Field],
    var neighbors : ListBuffer[NeighborInfo]) extends IR_AbstractFunction with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl() : String = prettyprint
  override def name = IR_AllocateDataFunction.fctName

  override def expand() : Output[IR_Function] = {
    val body = ListBuffer[IR_Statement]()

    // add static allocations here

    IR_Function(IR_UnitDatatype, name, body)
  }
}
