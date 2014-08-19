package exastencils.datastructures.l3

import exastencils.knowledge._

object Util {
  def addFunctions(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"def SetSolution$postfix@all (value : Real) : Unit {")
    printer.println(s"\tloop over Solution$postfix@current {")
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\t\t${Fields.solution(s"current", postfix)(vecDim)} = value")
    printer.println(s"\t}")
    printer.println(s"}")
  }
}