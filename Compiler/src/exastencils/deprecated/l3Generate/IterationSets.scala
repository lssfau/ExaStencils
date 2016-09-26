package exastencils.deprecated.l3Generate

import exastencils.knowledge._

// NOTE: this is currently obsolete and only kept for reference

object IterationSets {
  def addIterationSets(printer : java.io.PrintWriter) = {
    Knowledge.dimensionality match {
      case 2 => {
        printer.println("Set inner [0, 0]")
        if ("RBGS" == Knowledge.l3tmp_smoother) {
          if (Knowledge.l3tmp_useConditionsForRBGS) {
            printer.println("Set red [0, 0] with 0 == ((x + y) % 2)")
            printer.println("Set black [0, 0] with 1 == ((x + y) % 2)")
          } else {
            printer.println("Set red [0 + (y % 2), 0] - [0, 0] steps [2, 1]")
            printer.println("Set black [1 - (y % 2), 0] - [0, 0] steps [2, 1]")
          }
        }
      }
      case 3 => {
        printer.println("Set inner [0, 0, 0]")
        if ("RBGS" == Knowledge.l3tmp_smoother) {
          if (Knowledge.l3tmp_useConditionsForRBGS) {
            printer.println("Set red [0, 0, 0] with 0 == ((x + y + z) % 2)")
            printer.println("Set black [0, 0, 0] with 1 == ((x + y + z) % 2)")
          } else {
            printer.println("Set red [0 + ((y + z) % 2), 0, 0] - [0, 0, 0] steps [2, 1, 1]")
            printer.println("Set black [1 - ((y + z) % 2), 0, 0] - [0, 0, 0] steps [2, 1, 1]")
          }
        }
      }
    }
    printer.println
  }
}