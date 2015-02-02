package exastencils.datastructures.l3

import exastencils.knowledge._

object CGS {
  def addFunction(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"Function VCycle$postfix@coarsest ( ) : Unit {")
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims)
      printer.println(s"\tVCycle${postfix}_$vecDim@current ( )")
    printer.println(s"}")

    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims) {
      printer.println(s"Function VCycle${postfix}_$vecDim@coarsest ( ) : Unit {")
      if (Knowledge.l3tmp_genAsyncCommunication)
        printer.println(s"\tUpResidual$postfix@current ( 0 )")
      else
        printer.println(s"\tUpResidual$postfix@current ( )")

      Communication.exch(printer, Fields.residual("current", postfix)(vecDim))

      printer.println(s"\tVariable res : Real = NormResidual${postfix}_$vecDim@current ( )")
      printer.println(s"\tVariable initialRes : Real = res")

      printer.println(s"\tloop over VecP$postfix@current {")
      printer.println(s"\t\tVecP$postfix@current = ${Fields.residual("current", postfix)(vecDim)}")
      printer.println(s"\t}")

      printer.println(s"\tVariable cgSteps : Integer")
      printer.println(s"\trepeat 512 times count cgSteps {")
      Communication.exch(printer, s"VecP$postfix@current")

      printer.println(s"\t\tloop over VecP$postfix@current {")
      if (Knowledge.l3tmp_genStencilStencilConv && !Knowledge.l3tmp_genStencilFields)
        printer.println(s"\t\t\tVecGradP$postfix@current = ${Stencils.access(postfix)} * VecP$postfix@current")
      else
        printer.println(s"\t\t\tVecGradP$postfix@current = Laplace$postfix@current * VecP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tVariable alphaDenom : Real = 0")
      printer.println(s"\t\tloop over VecP$postfix@current where x > 0 && y > 0 ${if (Knowledge.dimensionality > 2) "&& z > 0 " else ""}with reduction( + : alphaDenom ) {")
      printer.println(s"\t\t\talphaDenom += VecP$postfix@current * VecGradP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tVariable alpha : Real = res * res / alphaDenom")

      printer.println(s"\t\tloop over Solution$postfix@current {")
      printer.println(s"\t\t\t${Fields.solutionSlotted(s"current", "curSlot", postfix)(vecDim)} += alpha * VecP$postfix@current")
      printer.println(s"\t\t\t${Fields.residual("current", postfix)(vecDim)} -= alpha * VecGradP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tVariable nextRes : Real = NormResidual${postfix}_$vecDim@current ( )")

      printer.println(s"\t\tif ( nextRes <= 0.001 * initialRes ) {")
      //printer.println("\t\t\tprint ( '\"CG required\"', cgSteps, '\"steps\"' )")
      printer.println(s"\t\t\treturn")
      printer.println(s"\t\t}")

      printer.println(s"\t\tVariable beta : Real = (nextRes * nextRes) / (res * res)")

      printer.println(s"\t\tloop over VecP$postfix@current {")
      printer.println(s"\t\t\tVecP$postfix@current = ${Fields.residual("current", postfix)(vecDim)} + beta * VecP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tres = nextRes")
      printer.println(s"\t}")
      printer.println(s"}")
    }
    printer.println
  }
}