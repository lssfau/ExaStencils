package exastencils.applications.ns.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.visualization.ir.IR_PrintXdmf

case class IR_PrintXdmfNNF(var filename : IR_Expression, level : Int, ioMethod : IR_Expression, binaryFpp : Boolean) extends IR_PrintXdmf(ioMethod, binaryFpp) with IR_PrintVisualizationNS {
  override def writeXdmf : ListBuffer[IR_Statement] = ???
  override def writeData : ListBuffer[IR_Statement] = ???
  override def writeXdmfGrid(gstream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = ???
  override def numFields : Int = ???
  override def seekpOffsets(global : Boolean, constantReduction : Boolean) : ListBuffer[IR_Expression] = ???
  override def stmtsForPreparation : ListBuffer[IR_Statement] = ???
}
