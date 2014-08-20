package exastencils.datastructures.l3

import exastencils.knowledge._

object Residual {
  def addUpdateBody(printer : java.io.PrintWriter, postfix : String, stencil : String) = {
    Communication.exch(printer, s"Solution$postfix@current")

    if (Knowledge.testFragLoops)
      printer.println(s"\tloop over fragments {")
    printer.println(s"\tloop over Residual$postfix@current {")
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\t\t${Fields.residual(s"current", postfix)(vecDim)} = ${Fields.rhs(s"current", postfix)(vecDim)} - ($stencil * ${Fields.solution(s"current", postfix)(vecDim)})")
    printer.println(s"\t}")
    if (Knowledge.testFragLoops)
      printer.println(s"\t}")

    if (Knowledge.testCommCompOverlap) {
      printer.println(s"\tif ( levels@current > levels@coarsest && startComm > 0 ) {")
      printer.println(s"\t\tbegin communicate Residual$postfix@current")
      printer.println(s"\t}")
    }
  }

  def addUpdateFunction(printer : java.io.PrintWriter, postfix : String) = {
    val levels = if (Knowledge.testStencilStencil && !Knowledge.genStencilFields) "finest" else "all"
    val params = if (Knowledge.testCommCompOverlap) "startComm : Integer" else ""

    printer.println(s"Function UpResidual$postfix@$levels ( $params ) : Unit {")
    addUpdateBody(printer, postfix, s"Laplace$postfix@current")
    printer.println(s"}")

    if (Knowledge.testStencilStencil && !Knowledge.genStencilFields) {
      printer.println(s"Function UpResidual$postfix@(coarsest to (finest - 1)) ( $params ) : Unit {")
      addUpdateBody(printer, postfix, Stencils.access(postfix))
      printer.println(s"}")
    }
  }

  def addReductionFunction(printer : java.io.PrintWriter, postfix : String) = {
    for (vecDim <- 0 until Knowledge.numVecDims) {
      printer.println(s"Function L2Residual${postfix}_$vecDim@(coarsest and finest) ( ) : Real {")
      Communication.exch(printer, s"Residual$postfix@current")

      printer.println(s"\tVariable res : Real = 0")
      if (Knowledge.testFragLoops)
        printer.println(s"\tloop over fragments with reduction( + : res ) {")
      printer.println(s"\tloop over Residual$postfix@current where x > 0 && y > 0 ${if (Knowledge.dimensionality > 2) "&& z > 0 " else ""}with reduction( + : res ) {")
      printer.println(s"\t\tres += ${Fields.residual(s"current", postfix)(vecDim)} * ${Fields.residual(s"current", postfix)(vecDim)}")
      printer.println(s"\t}")
      if (Knowledge.testFragLoops)
        printer.println(s"\t}")
      printer.println(s"\treturn ( sqrt ( res ) )")
      printer.println(s"}")
      printer.println
    }
  }
}