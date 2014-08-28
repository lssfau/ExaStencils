package exastencils.datastructures.l3

import exastencils.knowledge._

object Restriction {
  def addFunction(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"Function Restriction$postfix@((coarsest + 1) to finest) ( ) : Unit {")
    if (Knowledge.l3tmp_genAsyncCommunication)
      printer.println(s"\tfinish communicate Residual$postfix@current")
    else
      Communication.exch(printer, s"Residual$postfix@current")

    if (Knowledge.l3tmp_genFragLoops)
      printer.println(s"\tloop over fragments {")
    printer.println(s"\tloop over RHS$postfix@coarser {")
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims)
      printer.println(s"\t\t${Fields.rhs(s"coarser", postfix)(vecDim)} = RestrictionStencil@current * ${Fields.residual(s"current", postfix)(vecDim)}")
    if (Knowledge.l3tmp_genFragLoops)
      printer.println(s"\t}")
    printer.println(s"\t}")
    printer.println(s"}")
  }
}