package exastencils.datastructures.l3

import exastencils.knowledge._

object Restriction {
  def addFunction(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"Function Restriction$postfix@((coarsest + 1) to finest) ( ) : Unit {")
    if (Knowledge.testCommCompOverlap)
      printer.println(s"\tfinish communicate Residual$postfix@current")
    else
      Communication.exch(printer, s"Residual$postfix@current")

    if (Knowledge.testFragLoops)
      printer.println(s"\tloop over fragments {")
    printer.println(s"\tloop over RHS$postfix@coarser {")
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\t\t${Fields.rhs(s"coarser", postfix)(vecDim)} = RestrictionStencil@current * ToCoarser ( ${Fields.residual(s"current", postfix)(vecDim)} )")
    if (Knowledge.testFragLoops)
      printer.println(s"\t}")
    printer.println(s"\t}")
    printer.println(s"}")
  }
}