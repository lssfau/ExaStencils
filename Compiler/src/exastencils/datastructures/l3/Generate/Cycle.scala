package exastencils.datastructures.l3

import exastencils.knowledge._

object Cycle {
  def printBody(printer : java.io.PrintWriter, postfix : String, tempBlocking : Boolean) = {
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstartTimer ( preSmoothTimer$postfix${if (Knowledge.l3tmp_genTimersPerLevel) "@current" else ""} )")
    if (!tempBlocking)
      printer.println(s"\trepeat ${Knowledge.l3tmp_numPre} times {")
    printer.println(s"\t\tSmoother$postfix@current ( )")
    if (!tempBlocking)
      printer.println(s"\t}")
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstopTimer ( preSmoothTimer$postfix${if (Knowledge.l3tmp_genTimersPerLevel) "@current" else ""} )")

    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstartTimer ( upResidualTimer$postfix${if (Knowledge.l3tmp_genTimersPerLevel) "@current" else ""} )")
    if (Knowledge.l3tmp_genAsyncCommunication)
      printer.println(s"\tUpResidual$postfix@current ( 1 )")
    else
      printer.println(s"\tUpResidual$postfix@current ( )")
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstopTimer ( upResidualTimer$postfix${if (Knowledge.l3tmp_genTimersPerLevel) "@current" else ""} )")

    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstartTimer ( restrictionTimer$postfix${if (Knowledge.l3tmp_genTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tRestriction$postfix@current ( )")
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstopTimer ( restrictionTimer$postfix${if (Knowledge.l3tmp_genTimersPerLevel) "@current" else ""} )")

    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstartTimer ( setSolutionTimer$postfix${if (Knowledge.l3tmp_genTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tSetSolution$postfix@coarser ( 0 )")
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstopTimer ( setSolutionTimer$postfix${if (Knowledge.l3tmp_genTimersPerLevel) "@current" else ""} )")

    printer.println(s"\trepeat ${Knowledge.l3tmp_numRecCycleCalls} times {")
    printer.println(s"\t\tVCycle$postfix@coarser ( )")
    printer.println(s"\t}")

    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstartTimer ( correctionTimer$postfix${if (Knowledge.l3tmp_genTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tCorrection$postfix@current ( )")
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstopTimer ( correctionTimer$postfix${if (Knowledge.l3tmp_genTimersPerLevel) "@current" else ""} )")

    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstartTimer ( postSmoothTimer$postfix${if (Knowledge.l3tmp_genTimersPerLevel) "@current" else ""} )")
    if (!tempBlocking)
      printer.println(s"\trepeat ${Knowledge.l3tmp_numPost} times {")
    printer.println(s"\t\tSmoother$postfix@current ( )")
    if (!tempBlocking)
      printer.println(s"\t}")
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstopTimer ( postSmoothTimer$postfix${if (Knowledge.l3tmp_genTimersPerLevel) "@current" else ""} )")
  }

  def addCycle(printer : java.io.PrintWriter, postfix : String) = {
    if ("Jac" == Knowledge.l3tmp_smoother && !Knowledge.l3tmp_useSlotVariables) {
      Knowledge.l3tmp_numPre /= 2
      Knowledge.l3tmp_numPost /= 2
    }

    if (Knowledge.l3tmp_genTemporalBlocking) {
      if (Knowledge.l3tmp_tempBlockingMinLevel > 1) {
        printer.println(s"Function VCycle$postfix@((coarsest + 1) to ${Knowledge.l3tmp_tempBlockingMinLevel - 1}) ( ) : Unit {")
        printBody(printer, postfix, false)
        printer.println(s"}")
      }
      printer.println(s"Function VCycle$postfix@(${Knowledge.l3tmp_tempBlockingMinLevel} to finest) ( ) : Unit {")
      printBody(printer, postfix, true)
      printer.println(s"}")
    } else {
      printer.println(s"Function VCycle$postfix@((coarsest + 1) to finest) ( ) : Unit {")
      printBody(printer, postfix, false)
      printer.println(s"}")
    }

    printer.println
  }
}