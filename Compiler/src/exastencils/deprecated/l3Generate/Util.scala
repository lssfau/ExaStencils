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
    printer.println(s"\t\tif ( $toPrint <= ${ if (Knowledge.useDblPrecision) 1e-12 else 1e-5 } ) {")
    printer.println("\t\t\tprint ( 'EFFECTIVELY ZERO' )")
    printer.println(s"\t\t} else if ( $toPrint <= ${ if (Knowledge.useDblPrecision) 1e-11 else 1e-4 } ) {")
    printer.println(s"\t\t\tnative ( 'std::streamsize oldPrec = std::cout.precision()' )")
    printer.println(s"\t\t\tnative ( 'std::cout.precision(1)' )")
    printer.println(s"\t\t\tprint ( $toPrint )")
    printer.println(s"\t\t\tnative ( 'std::cout.precision(oldPrec)' )")
    printer.println(s"\t\t} else if ( $toPrint <= ${ if (Knowledge.useDblPrecision) 1e-10 else 1e-3 } ) {")
    printer.println(s"\t\t\tnative ( 'std::streamsize oldPrec = std::cout.precision()' )")
    printer.println(s"\t\t\tnative ( 'std::cout.precision(2)' )")
    printer.println(s"\t\t\tprint ( $toPrint )")
    printer.println(s"\t\t\tnative ( 'std::cout.precision(oldPrec)' )")
    printer.println(s"\t\t} else if ( $toPrint <= ${ if (Knowledge.useDblPrecision) 1e-9 else 1e-2 } ) {")
    printer.println(s"\t\t\tnative ( 'std::streamsize oldPrec = std::cout.precision()' )")
    printer.println(s"\t\t\tnative ( 'std::cout.precision(3)' )")
    printer.println(s"\t\t\tprint ( $toPrint )")
    printer.println(s"\t\t\tnative ( 'std::cout.precision(oldPrec)' )")
    printer.println(s"\t\t} else {")
    printer.println(s"\t\t\tnative ( 'std::streamsize oldPrec = std::cout.precision()' )")
    printer.println(s"\t\t\tnative ( 'std::cout.precision(4)' )")
    printer.println(s"\t\t\tprint ( $toPrint )")
    printer.println(s"\t\t\tnative ( 'std::cout.precision(oldPrec)' )")
    printer.println(s"\t\t}")
  }
}
