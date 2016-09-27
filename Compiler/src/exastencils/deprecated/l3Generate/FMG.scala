package exastencils.deprecated.l3Generate

import exastencils.knowledge._

object FMG {
  def addFunctions(printer : java.io.PrintWriter) = {
    // specialized boundary handling functions
    val fields = if ("Jac" == Knowledge.l3tmp_smoother) {
      if (Knowledge.l3tmp_useSlotsForJac)
        Array("Solution[0]@current", "Solution[1]@current")
      else
        Array("Solution@current", "Solution2@current")
    } else {
      Array("Solution@current")
    }
    val directions = Knowledge.dimensionality match {
      case 2 => Array("[-1,  0]", "[ 1,  0]", "[ 0, -1]", "[ 0,  1]")
      case 3 => Array("[-1,  0,  0]", "[ 1,  0,  0]", "[ 0, -1,  0]", "[ 0,  1,  0]", "[ 0,  0, -1]", "[ 0,  0,  1]")
    }
    val bc = Functions.solFunction(false)

    printer.println(s"Function SetFuncDir@all ( ) : Unit {")
    for (dir <- directions) {
      for (field <- fields) {
        printer.println(s"\tloop over $field only dup $dir on boundary {")
        printer.println(s"\t\t$field = $bc")
        printer.println(s"\t}")
      }
    }
    printer.println(s"}")
    printer.println

    printer.println(s"Function ResetBC@all ( ) : Unit {")
    if ("Jac" == Knowledge.l3tmp_smoother) {
      if (Knowledge.l3tmp_useSlotsForJac) {
        Communication.applyBCs(printer, s"Solution[0]@current")
        Communication.applyBCs(printer, s"Solution[1]@current")
      } else {
        Communication.applyBCs(printer, s"Solution@current")
        Communication.applyBCs(printer, s"Solution2@current")
      }
    } else {
      Communication.applyBCs(printer, s"Solution@current")
    }
    printer.println(s"}")
    printer.println

    // FMG function itself
    printer.println(s"Function FMG@(coarsest to (finest - 1)) ( ) : Unit {")
    printer.println(s"\tSetFuncDir@current ( )")
    printer.println(s"\tInitRHS@current ( )")
    printer.println(s"\tVCycle@current ( )")
    printer.println(s"\tCorrection@finer ( )")
    printer.println(s"\tResetBC@current ( )")
    printer.println
    printer.println(s"\tif ( levels@finer ( ) < levels@finest ( ) ) {")
    printer.println(s"\t\tFMG@finer ( )")
    printer.println(s"\t}")
    printer.println(s"}")
    printer.println
  }
}
