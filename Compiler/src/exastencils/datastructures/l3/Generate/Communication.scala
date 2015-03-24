package exastencils.datastructures.l3

import exastencils.knowledge._

object Communication {
  def applyBCs(printer : java.io.PrintWriter, field : String) = {
    printer.println(s"\tapply bc to $field")
  }

  def exch(printer : java.io.PrintWriter, field : String, target : String = "") = {
    if (Knowledge.l3tmp_genTimersForComm)
      printer.println(s"\tstartTimer ( ${if (Knowledge.l3tmp_genCommTimersPerLevel) "concat ( 'communication_', levelIndex@current() )" else "'communication'"} )")

    if (Knowledge.l3tmp_genCellBasedDiscr || Knowledge.experimental_Neumann)
      applyBCs(printer, field)

    if ("" == target)
      printer.println(s"\tcommunicate  $field")
    else
      printer.println(s"\tcommunicate $target of $field")

    if (Knowledge.l3tmp_genTimersForComm)
      printer.println(s"\tstopTimer ( ${if (Knowledge.l3tmp_genCommTimersPerLevel) "concat ( 'communication_', levelIndex@current() )" else "'communication'"} )")
  }
}
