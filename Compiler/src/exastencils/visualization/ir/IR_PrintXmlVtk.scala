package exastencils.visualization.ir

import exastencils.base.ir.IR_Expandable
import exastencils.base.ir.IR_Statement
import exastencils.datastructures.Transformation.OutputType

case class IR_PrintXmlVtk() extends IR_Statement with IR_Expandable {
  override def expand() : OutputType = ???

  def writeDomainPiecesVTI() = {
    // TODO
  }

  def writePVTI() = {
    // TODO
  }

  def writeDomainPiecesVTU() = {
    // TODO
  }

  def writePVTU() = {
    // TODO
  }

  // TODO: wrapper that distinguishes between uniform and others
}
