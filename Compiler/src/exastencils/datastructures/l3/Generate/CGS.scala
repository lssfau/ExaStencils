package exastencils.datastructures.l3

import exastencils.knowledge._

object CGS {
  def addFunction(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"Function VCycle$postfix@coarsest ( ) : Unit {")
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\tVCycle${postfix}_$vecDim@current ( )")
    printer.println(s"}")

    for (vecDim <- 0 until Knowledge.numVecDims) {
      printer.println(s"Function VCycle${postfix}_$vecDim@coarsest ( ) : Unit {")
      if (Knowledge.testCommCompOverlap)
        printer.println(s"\tUpResidual$postfix@current ( 0 )")
      else
        printer.println(s"\tUpResidual$postfix@current ( )")

      Communication.exch(printer, s"Residual$postfix@current")

      printer.println(s"\tVariable res : Real = L2Residual${postfix}_$vecDim@current ( )")
      printer.println(s"\tVariable initialRes : Real = res")

      printer.println(s"\tloop over VecP$postfix@current {")
      printer.println(s"\t\tVecP$postfix@current = ${Fields.residual("current", postfix)(vecDim)}")
      printer.println(s"\t}")

      printer.println(s"\tVariable cgSteps : Integer")
      printer.println(s"\trepeat 512 times count cgSteps {")
      Communication.exch(printer, s"VecP$postfix@current")

      printer.println(s"\t\tloop over VecP$postfix@current {")
      if (Knowledge.testStencilStencil && !Knowledge.genStencilFields)
        printer.println(s"\t\t\tVecGradP$postfix@current = ${Stencils.access(postfix)} * VecP$postfix@current")
      else
        printer.println(s"\t\t\tVecGradP$postfix@current = Laplace$postfix@current * VecP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tVariable alphaDenom : Real = 0")
      printer.println(s"\t\tloop over VecP$postfix@current with reduction( + : alphaDenom ) {")
      printer.println(s"\t\t\talphaDenom += VecP$postfix@current * VecGradP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tVariable alpha : Real = res * res / alphaDenom")

      printer.println(s"\t\tloop over Solution$postfix@current {")
      printer.println(s"\t\t\t${Fields.solution("current", postfix)(vecDim)} += alpha * VecP$postfix@current")
      printer.println(s"\t\t\t${Fields.residual("current", postfix)(vecDim)} -= alpha * VecGradP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tVariable nextRes : Real = L2Residual${postfix}_$vecDim@current ( )")

      printer.println(s"\t\tif ( nextRes <= 0.001 * initialRes ) {")
      //printer.println("\t\t\tprint ( '\"CG required\"', cgSteps, '\"steps\"' )")
      printer.println(s"\t\t\treturn ( )")
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