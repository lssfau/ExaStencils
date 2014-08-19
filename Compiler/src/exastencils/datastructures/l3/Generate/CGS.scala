package exastencils.datastructures.l3

import exastencils.knowledge._

object CGS {
  def addFunction(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"def VCycle$postfix@coarsest ( ) : Unit {")
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\tVCycle${postfix}_$vecDim@current ( )")
    printer.println(s"}")

    for (vecDim <- 0 until Knowledge.numVecDims) {
      printer.println(s"def VCycle${postfix}_$vecDim@coarsest ( ) : Unit {")
      if (Knowledge.testCommCompOverlap)
        printer.println(s"\tUpResidual$postfix@current ( 0 )")
      else
        printer.println(s"\tUpResidual$postfix@current ( )")

      Communication.exch(printer, s"Residual$postfix@current")

      printer.println(s"\tvar res : Real = L2Residual${postfix}_$vecDim@current ( )")
      printer.println(s"\tvar initialRes : Real = res")

      printer.println(s"\tloop over VecP$postfix@current {")
      printer.println(s"\t\tVecP$postfix@current = ${Fields.residual("current", postfix)(vecDim)}")
      printer.println(s"\t}")

      printer.println(s"\trepeat up 512 {")
      Communication.exch(printer, s"VecP$postfix@current")

      printer.println(s"\t\tloop over VecP$postfix@current {")
      if (Knowledge.testStencilStencil && !Knowledge.genStencilFields)
        printer.println(s"\t\t\tVecGradP$postfix@current = ${Stencils.access(postfix)} * VecP$postfix@current")
      else
        printer.println(s"\t\t\tVecGradP$postfix@current = Laplace$postfix@current * VecP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar alphaDenom : Real = 0")
      printer.println(s"\t\tloop over VecP$postfix@current with reduction( + : alphaDenom ) {")
      printer.println(s"\t\t\talphaDenom += VecP$postfix@current * VecGradP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar alpha : Real = res * res / alphaDenom")

      printer.println(s"\t\tloop over Solution$postfix@current {")
      printer.println(s"\t\t\t${Fields.solution("current", postfix)(vecDim)} += alpha * VecP$postfix@current")
      printer.println(s"\t\t\t${Fields.residual("current", postfix)(vecDim)} -= alpha * VecGradP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar nextRes : Real = L2Residual${postfix}_$vecDim@current ( )")

      printer.println(s"\t\tif ( nextRes <= 0.001 * initialRes ) {")
      printer.println(s"\t\t\treturn ( )")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar beta : Real = (nextRes * nextRes) / (res * res)")

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