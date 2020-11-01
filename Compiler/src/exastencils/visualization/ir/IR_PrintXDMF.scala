package exastencils.visualization.ir

import exastencils.base.ir.IR_Expandable
import exastencils.base.ir.IR_Statement
import exastencils.datastructures.Transformation.OutputType

case class IR_PrintXDMF() extends IR_Statement with IR_Expandable {
  override def expand() : OutputType = ???

  // TODO: distinguish between uniform and others

  // needs filenames of hdf5/binary files
}
