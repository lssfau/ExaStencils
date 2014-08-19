package exastencils.datastructures.l3

import exastencils.knowledge._

object Communication {
  def exch(printer : java.io.PrintWriter, field : String) = {
    if (Knowledge.genTimersForComm)
      printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tcommunicate $field")
    if (Knowledge.genTimersForComm)
      printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
  }
}