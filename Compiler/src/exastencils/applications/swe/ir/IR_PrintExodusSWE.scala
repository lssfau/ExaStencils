package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.io.ir.IR_DataBuffer
import exastencils.visualization.ir.IR_PrintExodus

case class IR_PrintExodusSWE(
    var filename : IR_Expression,
    level : Int) extends IR_PrintExodus with IR_PrintVisualizationSWE {

  override def variableEntityType : IR_VariableAccess = EX_NODAL
  override def elementName : String = "tri"
  override def nodesPerElement : Int = 3

  override def writeData : ListBuffer[IR_Statement] = ???

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = ???
}
