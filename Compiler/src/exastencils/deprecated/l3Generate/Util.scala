package exastencils.deprecated.l3Generate

import exastencils.config._

object Util {
  def addFunctions(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"Function SetSolution$postfix@all (value : Real) : Unit {")
    printer.println(s"\tloop over Solution$postfix@current {")
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims)
      printer.println(s"\t\t${ Fields.solutionSlotted(s"current", "active", postfix)(vecDim) } = value")
    printer.println(s"\t}")
    printer.println(s"}")
  }

  def addPrintAutoTestValueCode(printer : java.io.PrintWriter, toPrint : String) = {
    val maxPrec = Knowledge.l3tmp_autoTestMaxPrecision
    var threshold = if (Knowledge.useDblPrecision) 1e-12 else 1e-5
    var curPrec = 0

    printer.println(s"\t\tif ( $toPrint <= $threshold ) {")
    printer.println("\t\t\tprint ( 'EFFECTIVELY ZERO' )")
    while (curPrec < maxPrec) {
      threshold *= 10
      curPrec += 1
      if (curPrec == maxPrec)
        printer.println(s"\t\t} else {")
      else
        printer.println(s"\t\t} else if ( $toPrint <= $threshold ) {")
      printer.println(s"\t\t\tnative ( 'std::streamsize oldPrec = std::cout.precision()' )")
      printer.println(s"\t\t\tnative ( 'std::cout.precision($curPrec)' )")
      printer.println(s"\t\t\tprint ( $toPrint )")
      printer.println(s"\t\t\tnative ( 'std::cout.precision(oldPrec)' )")
    }
    printer.println(s"\t\t}")
  }
}
