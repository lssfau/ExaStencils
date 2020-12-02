package exastencils.applications.ns.ir

import exastencils.base.ir.IR_Expression
import exastencils.visualization.ir.IR_PrintXdmf

case class IR_PrintXdmfNS(var filename : IR_Expression, level : Int, ioMethod : IR_Expression, binaryFpp : Boolean) extends IR_PrintXdmf(ioMethod, binaryFpp) with IR_PrintVisualizationNS {

}
