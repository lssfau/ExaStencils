package exastencils.deprecated.l3Generate

import scala.collection.mutable.HashMap

import exastencils.config._

object Communication {
  var commTimerNames = HashMap[String, String]() // TimerName -> label

  def applyBCs(printer : java.io.PrintWriter, field : String) = {
    printer.println(s"\tapply bc to $field")
  }

  def exch(printer : java.io.PrintWriter, field : String, target : String = "", condition : String = "") = {
    var commName = "communication"
    if (Knowledge.l3tmp_genCommTimersPerField) commName += "_" + field.split('[')(0).split('@')(0)
    commTimerNames += commName -> (if (Knowledge.l3tmp_genCommTimersPerField) " " + field.split('[')(0).split('@')(0) else "")
    commName = if (Knowledge.l3tmp_genCommTimersPerLevel) s"concat ( '${ commName }_', levels@current() )" else s"'$commName'"

    if (Knowledge.l3tmp_genTimersForComm)
      printer.println(s"\tstartTimer ( $commName )")

    if (Knowledge.l3tmp_genCellBasedDiscr || Knowledge.experimental_Neumann)
      applyBCs(printer, field)

    printer.println(s"\tcommunicate${ if ("" == target) "" else s" $target of" } $field${ if ("" == condition) "" else s" where $condition" }")

    if (Knowledge.l3tmp_genTimersForComm)
      printer.println(s"\tstopTimer ( $commName )")
  }
}
