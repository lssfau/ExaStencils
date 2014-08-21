package exastencils.datastructures.l3

import exastencils.knowledge._

object Cycle {
  def addCycle(printer : java.io.PrintWriter, postfix : String) = {
    if ("Jac" == Knowledge.smoother && !Knowledge.useSlotVariables) {
      Knowledge.numPre /= 2
      Knowledge.numPost /= 2
    }
    printer.println(s"Function VCycle$postfix@((coarsest + 1) to finest) ( ) : Unit {")

    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstartTimer ( preSmoothTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")
    if (!Knowledge.testTempBlocking)
      printer.println(s"\trepeat ${Knowledge.numPre} times {")
    printer.println(s"\t\tSmoother$postfix@current ( )")
    if (!Knowledge.testTempBlocking)
      printer.println(s"\t}")
    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstopTimer ( preSmoothTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")

    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstartTimer ( upResidualTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")
    if (Knowledge.testCommCompOverlap)
      printer.println(s"\tUpResidual$postfix@current ( 1 )")
    else
      printer.println(s"\tUpResidual$postfix@current ( )")
    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstopTimer ( upResidualTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")

    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstartTimer ( restrictionTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tRestriction$postfix@current ( )")
    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstopTimer ( restrictionTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")

    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstartTimer ( setSolutionTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tSetSolution$postfix@coarser ( 0 )")
    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstopTimer ( setSolutionTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")

    printer.println(s"\tVCycle$postfix@coarser ( )")

    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstartTimer ( correctionTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tCorrection$postfix@current ( )")
    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstopTimer ( correctionTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")

    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstartTimer ( postSmoothTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")
    if (!Knowledge.testTempBlocking)
      printer.println(s"\trepeat ${Knowledge.numPost} times {")
    printer.println(s"\t\tSmoother$postfix@current ( )")
    if (!Knowledge.testTempBlocking)
      printer.println(s"\t}")
    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstopTimer ( postSmoothTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")

    printer.println(s"}")
    printer.println
  }
}