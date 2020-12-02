package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_MemberFunctionCall
import exastencils.base.ir.IR_ObjectInstantiation
import exastencils.base.ir.IR_Statement
import exastencils.core.Duplicate
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintXdmf

case class IR_PrintXdmfSWE(var filename : IR_Expression, level : Int, ioMethod : IR_Expression, binaryFpp : Boolean) extends IR_PrintXdmf(ioMethod, binaryFpp) with IR_PrintVisualizationSWE {

  def printXdmf = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val stream = newStream

    var seekp : IR_Expression = IR_IntegerConstant(0)

    val print = IR_Print(stream)
    print.toPrint += xmlHeader
    print.toPrint += openXdmf
    print.toPrint += openDomain
    print.toPrint += openGrid("Grid", "Uniform")

    print.toPrint += openGeometry("X_Y") // nodePositions are not interleaved

    // TODO

    statements += IR_ObjectInstantiation(stream, Duplicate(filename))
    statements += print
    statements += IR_MemberFunctionCall(stream, "close")

    statements
  }
}
