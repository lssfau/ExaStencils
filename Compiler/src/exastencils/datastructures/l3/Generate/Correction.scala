package exastencils.datastructures.l3

import exastencils.knowledge._

object Correction {
  def addFunction(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"Function Correction$postfix@((coarsest + 1) to finest) ( ) : Unit {")
    Communication.exch(printer, s"Solution$postfix@current")

    if (Knowledge.testFragLoops)
      printer.println(s"\tloop over fragments {")
    printer.println(s"\tloop over Solution$postfix@current {")
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\t\t${Fields.solution(s"current", postfix)(vecDim)} += CorrectionStencil@current * ToFiner ( ${Fields.solution(s"coarser", postfix)(vecDim)} )")
    printer.println(s"\t}")
    if (Knowledge.testFragLoops)
      printer.println(s"\t}")
    printer.println(s"}")
    printer.println
  }
}