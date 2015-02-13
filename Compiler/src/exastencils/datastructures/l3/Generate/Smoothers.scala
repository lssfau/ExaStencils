package exastencils.datastructures.l3

import exastencils.knowledge._

object Smoothers {
  def omegaToPrint = (if (Knowledge.l3tmp_genGlobalOmega) "l3tmp_omega" else Knowledge.l3tmp_omega)

  def addBodyBefore(printer : java.io.PrintWriter, postfix : String, tempBlocking : Boolean) = {
    if (Knowledge.l3tmp_genFragLoops)
      printer.println(s"\tloop over fragments {")
    if (tempBlocking)
      printer.println(s"\trepeat ${Knowledge.l3tmp_numPre} times with contraction {")
  }
  def addBodyAfter(printer : java.io.PrintWriter, postfix : String, tempBlocking : Boolean) = {
    if (tempBlocking)
      printer.println(s"\t}")
    if (Knowledge.l3tmp_genFragLoops)
      printer.println(s"\t}")
  }

  def addBodyJac(printer : java.io.PrintWriter, postfix : String, stencil : String, tempBlocking : Boolean) = {
    if (Knowledge.l3tmp_useSlotVariables && Knowledge.l3tmp_useSlotsForJac) {
      Communication.exch(printer, s"Solution$postfix[active]@current", "ghost")
      if (tempBlocking)
        Communication.exch(printer, s"RHS$postfix@current", "ghost")

      addBodyBefore(printer, postfix, tempBlocking)

      printer.println(s"\tloop over Solution$postfix@current {")
      for (vecDim <- 0 until Knowledge.l3tmp_numVecDims)
        printer.println(s"\t\t${Fields.solutionSlotted(s"current", "nextSlot", postfix)(vecDim)} = ${Fields.solutionSlotted(s"current", "active", postfix)(vecDim)} + ( ( ( 1.0 / diag ( $stencil ) ) * $omegaToPrint ) * ( ${Fields.rhs(s"current", postfix)(vecDim)} - $stencil * ${Fields.solutionSlotted(s"current", "active", postfix)(vecDim)} ) )")
      printer.println(s"\t}")
      printer.println(s"\tadvance Solution$postfix@current")

      addBodyAfter(printer, postfix, tempBlocking)
    } else {
      Communication.exch(printer, s"Solution$postfix${if (Knowledge.l3tmp_useSlotsForJac) "[0]" else ""}@current")

      printer.println(s"\tloop over Solution$postfix@current {")
      for (vecDim <- 0 until Knowledge.l3tmp_numVecDims)
        printer.println(s"\t\t${Fields.solution2(s"current", postfix)(vecDim)} = ${Fields.solution(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( $stencil ) ) * $omegaToPrint ) * ( ${Fields.rhs(s"current", postfix)(vecDim)} - $stencil * ${Fields.solution(s"current", postfix)(vecDim)} ) )")
      printer.println(s"\t}")

      Communication.exch(printer, s"Solution${if (Knowledge.l3tmp_useSlotsForJac) s"$postfix[1]" else s"2$postfix"}@current")

      printer.println(s"\tloop over Solution$postfix@current {")
      for (vecDim <- 0 until Knowledge.l3tmp_numVecDims)
        printer.println(s"\t\t${Fields.solution(s"current", postfix)(vecDim)} = ${Fields.solution2(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( $stencil ) ) * $omegaToPrint ) * ( ${Fields.rhs(s"current", postfix)(vecDim)} - $stencil * ${Fields.solution2(s"current", postfix)(vecDim)} ) )")
      printer.println(s"\t}")
    }
  }

  def addBodyRBGS(printer : java.io.PrintWriter, postfix : String, stencil : String, tempBlocking : Boolean) = {
    Communication.exch(printer, s"Solution$postfix@current")
    if (tempBlocking)
      Communication.exch(printer, s"RHS$postfix@current")

    addBodyBefore(printer, postfix, tempBlocking)

    Knowledge.dimensionality match {
      case 2 => {
        if (Knowledge.l3tmp_useConditionsForRBGS)
          printer.println(s"\tloop over Solution$postfix@current where 0 == ((x + y) % 2) {")
        else
          printer.println(s"\tloop over Solution$postfix@current starting [ 0 + (y % 2), 0 ] ending [ 0, 0 ] stepping [ 2, 1 ] {")
      }
      case 3 => {
        if (Knowledge.l3tmp_useConditionsForRBGS)
          printer.println(s"\tloop over Solution$postfix@current where 0 == ((x + y + z) % 2) {")
        else
          printer.println(s"\tloop over Solution$postfix@current starting [ 0 + ((y + z) % 2), 0 ] ending [ 0, 0, 0 ] stepping [ 2, 1, 1 ] {")
      }
    }
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims)
      printer.println(s"\t\t${Fields.solution(s"current", postfix)(vecDim)} = ${Fields.solution(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( $stencil ) ) * $omegaToPrint ) * ( ${Fields.rhs(s"current", postfix)(vecDim)} - $stencil * ${Fields.solution(s"current", postfix)(vecDim)} ) )")
    printer.println(s"\t}")

    if (!tempBlocking) // FIXME: else
      Communication.exch(printer, s"Solution$postfix@current")

    Knowledge.dimensionality match {
      case 2 => {
        if (Knowledge.l3tmp_useConditionsForRBGS)
          printer.println(s"\tloop over Solution$postfix@current where 1 == ((x + y) % 2) {")
        else
          printer.println(s"\tloop over Solution$postfix@current starting [ 1 - (y % 2), 0 ] ending [ 0, 0 ] stepping [ 2, 1 ] {")
      }
      case 3 => {
        if (Knowledge.l3tmp_useConditionsForRBGS)
          printer.println(s"\tloop over Solution$postfix@current where 1 == ((x + y + z) % 2) {")
        else
          printer.println(s"\tloop over Solution$postfix@current starting [ 1 - ((y + z) % 2), 0 ] ending [ 0, 0, 0 ] stepping [ 2, 1, 1 ] {")
      }
    }
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims)
      printer.println(s"\t\t${Fields.solution(s"current", postfix)(vecDim)} = ${Fields.solution(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( $stencil ) ) * $omegaToPrint ) * ( ${Fields.rhs(s"current", postfix)(vecDim)} - $stencil * ${Fields.solution(s"current", postfix)(vecDim)} ) )")
    printer.println(s"\t}")

    addBodyAfter(printer, postfix, tempBlocking)
  }

  def addBodyGS(printer : java.io.PrintWriter, postfix : String, stencil : String, tempBlocking : Boolean) = {
    Communication.exch(printer, s"Solution$postfix@current")
    if (tempBlocking)
      Communication.exch(printer, s"RHS$postfix@current")

    addBodyBefore(printer, postfix, tempBlocking)

    printer.println(s"\tloop over Solution$postfix@current {")
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims)
      printer.println(s"\t\t${Fields.solution(s"current", postfix)(vecDim)} = ${Fields.solution(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( $stencil ) ) * $omegaToPrint ) * ( ${Fields.rhs(s"current", postfix)(vecDim)} - $stencil * ${Fields.solution(s"current", postfix)(vecDim)} ) )")
    printer.println(s"\t}")

    addBodyAfter(printer, postfix, tempBlocking)
  }

  def addFunction(printer : java.io.PrintWriter, postfix : String) = {
    val bodyFunction = Knowledge.l3tmp_smoother match {
      case "Jac"  => ((stencil : String, tempBlocking : Boolean) => addBodyJac(printer, postfix, stencil, tempBlocking))
      case "RBGS" => ((stencil : String, tempBlocking : Boolean) => addBodyRBGS(printer, postfix, stencil, tempBlocking))
      case "GS"   => ((stencil : String, tempBlocking : Boolean) => addBodyGS(printer, postfix, stencil, tempBlocking))
    }

    if (Knowledge.l3tmp_genTemporalBlocking) {
      if (Knowledge.l3tmp_genStencilStencilConv && !Knowledge.l3tmp_genStencilFields) {
        if (Knowledge.l3tmp_tempBlockingMinLevel > 1) {
          printer.println(s"Function Smoother$postfix@((coarsest + 1) to ${Knowledge.l3tmp_tempBlockingMinLevel - 1}) ( ) : Unit {")
          bodyFunction(Stencils.access(postfix), false)
          printer.println(s"}")
        }
        printer.println(s"Function Smoother$postfix@(${Knowledge.l3tmp_tempBlockingMinLevel} to (finest - 1)) ( ) : Unit {")
        bodyFunction(Stencils.access(postfix), true)
        printer.println(s"}")
        printer.println(s"Function Smoother$postfix@finest ( ) : Unit {")
        bodyFunction(s"Laplace$postfix@current", true)
        printer.println(s"}")
      } else {
        if (Knowledge.l3tmp_tempBlockingMinLevel > 1) {
          printer.println(s"Function Smoother$postfix@((coarsest + 1) to ${Knowledge.l3tmp_tempBlockingMinLevel - 1}) ( ) : Unit {")
          bodyFunction(s"Laplace$postfix@current", false)
          printer.println(s"}")
        }
        printer.println(s"Function Smoother$postfix@(${Knowledge.l3tmp_tempBlockingMinLevel} to finest) ( ) : Unit {")
        bodyFunction(s"Laplace$postfix@current", true)
        printer.println(s"}")
      }
    } else {
      if (Knowledge.l3tmp_genStencilStencilConv && !Knowledge.l3tmp_genStencilFields) {
        printer.println(s"Function Smoother$postfix@((coarsest + 1) to (finest - 1)) ( ) : Unit {")
        bodyFunction(Stencils.access(postfix), false)
        printer.println(s"}")
      }

      if (Knowledge.l3tmp_genStencilStencilConv && !Knowledge.l3tmp_genStencilFields)
        printer.println(s"Function Smoother$postfix@finest ( ) : Unit {")
      else
        printer.println(s"Function Smoother$postfix@((coarsest + 1) to finest) ( ) : Unit {")
      bodyFunction(s"Laplace$postfix@current", false)
      printer.println(s"}")
    }

    printer.println
  }
}
