package exastencils.datastructures.l3

import exastencils.knowledge._

object Correction {
  def addFunction(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"Function Correction$postfix@((coarsest + 1) to finest) ( ) : Unit {")

    if (Knowledge.l3tmp_genFragLoops)
      printer.println(s"\tloop over fragments {")
    printer.println(s"\tloop over Solution$postfix@current {")
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims)
      printer.println(s"\t\t${Fields.solutionSlotted(s"current", "active", postfix)(vecDim)} += CorrectionStencil@current * ${Fields.solutionSlotted(s"coarser", "active", postfix)(vecDim)}")
    printer.println(s"\t}")
    if (Knowledge.l3tmp_genFragLoops)
      printer.println(s"\t}")
    printer.println(s"}")
    printer.println
  }
}