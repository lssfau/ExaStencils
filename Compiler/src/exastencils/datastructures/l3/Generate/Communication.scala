package exastencils.datastructures.l3

import exastencils.knowledge._

object Communication {
  def applyBCs(printer : java.io.PrintWriter, field : String) = {
    printer.println(s"\tapply bc to $field")
  }

  def exch(printer : java.io.PrintWriter, field : String, target : String = "", condition : String = "") = {
    var commName = "communication"
    if (Knowledge.l3tmp_genCommTimersPerField) commName += s"_$field"
    if (Knowledge.l3tmp_genCommTimersPerLevel) commName = s"concat ( '$commName', levels@current() )" else s"'$commName'"

    if (Knowledge.l3tmp_genTimersForComm)
      printer.println(s"\tstartTimer ( $commName )")

    if (Knowledge.l3tmp_genCellBasedDiscr || Knowledge.experimental_Neumann)
      applyBCs(printer, field)

    printer.println(s"\tcommunicate${if ("" == target) "" else s" $target of"} $field${if ("" == condition) "" else s" where $condition"}")

    if (Knowledge.l3tmp_genTimersForComm)
      printer.println(s"\tstopTimer ( $commName )")
  }
}
