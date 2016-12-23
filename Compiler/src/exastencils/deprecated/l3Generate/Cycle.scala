package exastencils.deprecated.l3Generate

import exastencils.config._

object Cycle {
  def printBody(printer : java.io.PrintWriter, postfix : String, tempBlocking : Boolean, numCycleCalls : Int) = {
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstartTimer ( ${ if (Knowledge.l3tmp_genTimersPerLevel) s"concat ( 'preSmoothing${ postfix }_', levels@current() )" else s"'preSmoothing$postfix'" } )")
    if (!tempBlocking)
      printer.println(s"\trepeat ${ Knowledge.l3tmp_numPre } times {")
    printer.println(s"\t\tSmoother$postfix@current ( )")
    if (!tempBlocking)
      printer.println(s"\t}")
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstopTimer ( ${ if (Knowledge.l3tmp_genTimersPerLevel) s"concat ( 'preSmoothing${ postfix }_', levels@current() )" else s"'preSmoothing$postfix'" } )")

    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstartTimer ( ${ if (Knowledge.l3tmp_genTimersPerLevel) s"concat ( 'residualUpdate${ postfix }_', levels@current() )" else s"'residualUpdate$postfix'" } )")
    if (Knowledge.l3tmp_genAsyncCommunication)
      printer.println(s"\tUpResidual$postfix@current ( 1 )")
    else
      printer.println(s"\tUpResidual$postfix@current ( )")
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstopTimer ( ${ if (Knowledge.l3tmp_genTimersPerLevel) s"concat ( 'residualUpdate${ postfix }_', levels@current() )" else s"'residualUpdate$postfix'" } )")

    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstartTimer ( ${ if (Knowledge.l3tmp_genTimersPerLevel) s"concat ( 'restriction${ postfix }_', levels@current() )" else s"'restriction$postfix'" } )")
    printer.println(s"\tRestriction$postfix@current ( )")
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstopTimer ( ${ if (Knowledge.l3tmp_genTimersPerLevel) s"concat ( 'restriction${ postfix }_', levels@current() )" else s"'restriction$postfix'" } )")

    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstartTimer ( ${ if (Knowledge.l3tmp_genTimersPerLevel) s"concat ( 'settingSolution${ postfix }_', levels@current() )" else s"'settingSolution$postfix'" } )")
    printer.println(s"\tSetSolution$postfix@coarser ( 0 )")
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstopTimer ( ${ if (Knowledge.l3tmp_genTimersPerLevel) s"concat ( 'settingSolution${ postfix }_', levels@current() )" else s"'settingSolution$postfix'" } )")

    if (numCycleCalls > 1)
      printer.println(s"\trepeat ${ numCycleCalls } times {")
    printer.println(s"\t\tVCycle$postfix@coarser ( )")
    if (numCycleCalls > 1)
      printer.println(s"\t}")

    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstartTimer ( ${ if (Knowledge.l3tmp_genTimersPerLevel) s"concat ( 'correction${ postfix }_', levels@current() )" else s"'correction$postfix'" } )")
    printer.println(s"\tCorrection$postfix@current ( )")
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstopTimer ( ${ if (Knowledge.l3tmp_genTimersPerLevel) s"concat ( 'correction${ postfix }_', levels@current() )" else s"'correction$postfix'" } )")

    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstartTimer ( ${ if (Knowledge.l3tmp_genTimersPerLevel) s"concat ( 'postSmoothing${ postfix }_', levels@current() )" else s"'postSmoothing$postfix'" } )")
    if (!tempBlocking)
      printer.println(s"\trepeat ${ Knowledge.l3tmp_numPost } times {")
    printer.println(s"\t\tSmoother$postfix@current ( )")
    if (!tempBlocking)
      printer.println(s"\t}")
    if (Knowledge.l3tmp_genTimersPerFunction)
      printer.println(s"\tstopTimer ( ${ if (Knowledge.l3tmp_genTimersPerLevel) s"concat ( 'postSmoothing${ postfix }_', levels@current() )" else s"'postSmoothing$postfix'" } )")
  }

  def addCycle(printer : java.io.PrintWriter, postfix : String) = {
    if ("Jac" == Knowledge.l3tmp_smoother && !Knowledge.l3tmp_useSlotVariables) {
      Knowledge.l3tmp_numPre /= 2
      Knowledge.l3tmp_numPost /= 2
    }

    if (Knowledge.l3tmp_genTemporalBlocking) {
      if (Knowledge.l3tmp_tempBlockingMinLevel > 1) {
        printer.println(s"Function VCycle$postfix@((coarsest + 1) to ${ Knowledge.l3tmp_tempBlockingMinLevel - 1 }) ( ) : Unit {")
        printBody(printer, postfix, false, 1)
        printer.println(s"}")
      }
      if (Knowledge.l3tmp_numRecCycleCalls > 1) {
        printer.println(s"Function VCycle$postfix@finest ( ) : Unit {")
        printBody(printer, postfix, true, Knowledge.l3tmp_numRecCycleCalls)
        printer.println(s"}")
        printer.println(s"}")
        printer.println(s"Function VCycle$postfix@(${ Knowledge.l3tmp_tempBlockingMinLevel } to (finest - 1)) ( ) : Unit {")
        printBody(printer, postfix, true, 1)
        printer.println(s"}")
      } else {
        printer.println(s"Function VCycle$postfix@(${ Knowledge.l3tmp_tempBlockingMinLevel } to finest) ( ) : Unit {")
        printBody(printer, postfix, true, 1)
        printer.println(s"}")
      }
    } else {
      if (Knowledge.l3tmp_numRecCycleCalls > 1) {
        printer.println(s"Function VCycle$postfix@finest ( ) : Unit {")
        printBody(printer, postfix, false, Knowledge.l3tmp_numRecCycleCalls)
        printer.println(s"}")
        printer.println(s"Function VCycle$postfix@((coarsest + 1) to (finest - 1)) ( ) : Unit {")
        printBody(printer, postfix, false, 1)
        printer.println(s"}")
      } else {
        printer.println(s"Function VCycle$postfix@((coarsest + 1) to finest) ( ) : Unit {")
        printBody(printer, postfix, false, 1)
        printer.println(s"}")
      }
    }

    printer.println()
  }
}