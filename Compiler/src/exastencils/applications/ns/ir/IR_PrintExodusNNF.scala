package exastencils.applications.ns.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.io.ir.IR_DataBuffer
import exastencils.visualization.ir.IR_PrintExodus

case class IR_PrintExodusNNF(
    var filename : IR_Expression,
    level : Int) extends IR_PrintExodus with IR_PrintVisualizationNS {

  override def variableEntityType : IR_VariableAccess = EX_ELEM_BLOCK
  override def elementName : String = if (numDimsGrid > 2) "hex" else "quad"
  override def nodesPerElement : Int = connectivityForCell().length

  override def fieldnames : ListBuffer[String] = ListBuffer("velX", "velY", "velZ", "p", "rho", "mue", "gamma", "phi")

  override def writeData : ListBuffer[IR_Statement] = ???

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = ???
}
