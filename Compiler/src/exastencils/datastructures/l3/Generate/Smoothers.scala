package exastencils.datastructures.l3

import exastencils.knowledge._

object Smoothers {
  def omegaToPrint = (if (Knowledge.omegaViaGlobals) "omega" else Knowledge.omega)

  def addBodyBefore(printer : java.io.PrintWriter, postfix : String) = {
    if (Knowledge.testFragLoops)
      printer.println(s"\tloop over fragments {")
    if (Knowledge.testTempBlocking)
      printer.println(s"\trepeat ${Knowledge.numPre} times with contraction {")
  }
  def addBodyAfter(printer : java.io.PrintWriter, postfix : String) = {
    if (Knowledge.testTempBlocking)
      printer.println(s"\t}")
    if (Knowledge.testFragLoops)
      printer.println(s"\t}")
  }

  def addBodyJac(printer : java.io.PrintWriter, postfix : String, stencil : String) = {
    if (Knowledge.useSlotVariables && Knowledge.useSlotsForJac) {
      Communication.exch(printer, s"Solution$postfix[curSlot]@current")
      if (Knowledge.testTempBlocking)
        Communication.exch(printer, s"RHS$postfix@current")

      addBodyBefore(printer, postfix)

      printer.println(s"\tloop over Solution$postfix@current {")
      for (vecDim <- 0 until Knowledge.numVecDims)
        printer.println(s"\t\t${Fields.solutionSlotted(s"current", "nextSlot", postfix)(vecDim)} = ${Fields.solutionSlotted(s"current", "curSlot", postfix)(vecDim)} + ( ( ( 1.0 / diag ( $stencil ) ) * $omegaToPrint ) * ( ${Fields.rhs(s"current", postfix)(vecDim)} - $stencil * ${Fields.solutionSlotted(s"current", "curSlot", postfix)(vecDim)} ) )")
      printer.println(s"\t}")
      printer.println(s"\tadvance ( Solution$postfix@current )")

      addBodyAfter(printer, postfix)
    } else {
      Communication.exch(printer, s"Solution$postfix${if (Knowledge.useSlotsForJac) "[0]" else ""}@current")

      printer.println(s"\tloop over Solution$postfix@current {")
      for (vecDim <- 0 until Knowledge.numVecDims)
        printer.println(s"\t\t${Fields.solution2(s"current", postfix)(vecDim)} = ${Fields.solution(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( $stencil ) ) * $omegaToPrint ) * ( ${Fields.rhs(s"current", postfix)(vecDim)} - $stencil * ${Fields.solution(s"current", postfix)(vecDim)} ) )")
      printer.println(s"\t}")

      Communication.exch(printer, s"Solution${if (Knowledge.useSlotsForJac) s"$postfix[1]" else s"2$postfix"}@current")

      printer.println(s"\tloop over Solution$postfix@current {")
      for (vecDim <- 0 until Knowledge.numVecDims)
        printer.println(s"\t\t${Fields.solution(s"current", postfix)(vecDim)} = ${Fields.solution2(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( $stencil ) ) * $omegaToPrint ) * ( ${Fields.rhs(s"current", postfix)(vecDim)} - $stencil * ${Fields.solution2(s"current", postfix)(vecDim)} ) )")
      printer.println(s"\t}")
    }
  }

  def addBodyRBGS(printer : java.io.PrintWriter, postfix : String, stencil : String) = {
    Communication.exch(printer, s"Solution$postfix@current")

    Knowledge.dimensionality match {
      case 2 => {
        if (Knowledge.genRBSetsWithConditions)
          printer.println(s"\tloop over Solution$postfix@current where 0 == ((x + y) % 2) {")
        else
          printer.println(s"\tloop over Solution$postfix@current starting [ 0 + (y % 2), 0 ] ending [ 0, 0 ] stepping [ 2, 1 ] {")
      }
      case 3 => {
        if (Knowledge.genRBSetsWithConditions)
          printer.println(s"\tloop over Solution$postfix@current where 0 == ((x + y + z) % 2) {")
        else
          printer.println(s"\tloop over Solution$postfix@current starting [ 0 + ((y + z) % 2), 0 ] ending [ 0, 0, 0 ] stepping [ 2, 1, 1 ] {")
      }
    }
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\t\t${Fields.solution(s"current", postfix)(vecDim)} = ${Fields.solution(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( $stencil ) ) * $omegaToPrint ) * ( ${Fields.rhs(s"current", postfix)(vecDim)} - $stencil * ${Fields.solution(s"current", postfix)(vecDim)} ) )")
    printer.println(s"\t}")

    Communication.exch(printer, s"Solution$postfix@current")

    Knowledge.dimensionality match {
      case 2 => {
        if (Knowledge.genRBSetsWithConditions)
          printer.println(s"\tloop over Solution$postfix@current where 1 == ((x + y) % 2) {")
        else
          printer.println(s"\tloop over Solution$postfix@current starting [ 1 - (y % 2), 0 ] ending [ 0, 0 ] stepping [ 2, 1 ] {")
      }
      case 3 => {
        if (Knowledge.genRBSetsWithConditions)
          printer.println(s"\tloop over Solution$postfix@current where 1 == ((x + y + z) % 2) {")
        else
          printer.println(s"\tloop over Solution$postfix@current starting [ 1 - ((y + z) % 2), 0 ] ending [ 0, 0, 0 ] stepping [ 2, 1, 1 ] {")
      }
    }
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\t\t${Fields.solution(s"current", postfix)(vecDim)} = ${Fields.solution(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( $stencil ) ) * $omegaToPrint ) * ( ${Fields.rhs(s"current", postfix)(vecDim)} - $stencil * ${Fields.solution(s"current", postfix)(vecDim)} ) )")
    printer.println(s"\t}")
  }

  def addBodyGS(printer : java.io.PrintWriter, postfix : String, stencil : String) = {
    Communication.exch(printer, s"Solution$postfix@current")
    if (Knowledge.testTempBlocking)
      Communication.exch(printer, s"RHS$postfix@current")

    addBodyBefore(printer, postfix)

    printer.println(s"\tloop over Solution$postfix@current {")
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\t\t${Fields.solution(s"current", postfix)(vecDim)} = ${Fields.solution(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( $stencil ) ) * $omegaToPrint ) * ( ${Fields.rhs(s"current", postfix)(vecDim)} - $stencil * ${Fields.solution(s"current", postfix)(vecDim)} ) )")
    printer.println(s"\t}")

    addBodyAfter(printer, postfix)
  }

  def addFunction(printer : java.io.PrintWriter, postfix : String) = {
    val bodyFunction = Knowledge.smoother match {
      case "Jac"  => ((stencil : String) => addBodyJac(printer, postfix, stencil))
      case "RBGS" => ((stencil : String) => addBodyRBGS(printer, postfix, stencil))
      case "GS"   => ((stencil : String) => addBodyGS(printer, postfix, stencil))
    }

    if (Knowledge.testStencilStencil && !Knowledge.genStencilFields)
      printer.println(s"Function Smoother$postfix@finest ( ) : Unit {")
    else
      printer.println(s"Function Smoother$postfix@((coarsest + 1) to finest) ( ) : Unit {")
    bodyFunction(s"Laplace$postfix@current")
    printer.println(s"}")

    if (Knowledge.testStencilStencil && !Knowledge.genStencilFields) {
      printer.println(s"Function Smoother$postfix@((coarsest + 1) to (finest - 1)) ( ) : Unit {")
      bodyFunction(Stencils.access(postfix))
      printer.println(s"}")
    }
    printer.println
  }
}
