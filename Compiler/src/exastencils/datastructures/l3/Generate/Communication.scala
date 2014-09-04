package exastencils.datastructures.l3

import exastencils.knowledge._

object Communication {
  def applyBCs(printer : java.io.PrintWriter, field : String) = {
    printer.println(s"\tapply bc to $field")
  }

  def exch(printer : java.io.PrintWriter, field : String) = {
    if (Knowledge.l3tmp_genTimersForComm)
      printer.println(s"\tstartTimer ( commTimer${if (Knowledge.l3tmp_genCommTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tcommunicate $field")
    if (Knowledge.l3tmp_genTimersForComm)
      printer.println(s"\tstopTimer ( commTimer${if (Knowledge.l3tmp_genCommTimersPerLevel) "@current" else ""} )")
  }
}