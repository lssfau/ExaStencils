package exastencils.deprecated.l3Generate

import exastencils.config._

object Restriction {
  def addFunction(printer : java.io.PrintStream, postfix : String) = {
    printer.println(s"Function Restriction$postfix@((coarsest + 1) to finest) ( ) : Unit {")
    if (Knowledge.l3tmp_genAsyncCommunication)
      printer.println(s"\tfinish communicate Residual$postfix@current", "ghost")
    else
      Communication.exch(printer, s"Residual$postfix@current", "ghost")

    if (Knowledge.l3tmp_genFragLoops)
      printer.println(s"\tloop over fragments {")
    printer.println(s"\tloop over RHS$postfix@coarser {")
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims) {
      printer.print(s"\t\t${ Fields.rhs(s"coarser", postfix)(vecDim) } = ")
      if (!Knowledge.l3tmp_genHDepStencils)
        printer.print(s"4.0 * ")
      printer.println(s"RestrictionStencil@current * ${ Fields.residual(s"current", postfix)(vecDim) }"
      )
    }
    if (Knowledge.l3tmp_genFragLoops)
      printer.println(s"\t}")
    printer.println(s"\t}")
    printer.println(s"}")
  }
}