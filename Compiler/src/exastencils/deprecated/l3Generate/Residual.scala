package exastencils.deprecated.l3Generate

import exastencils.knowledge._

object Residual {
  def addUpdateBody(printer : java.io.PrintWriter, postfix : String, stencil : String) = {
    if (Knowledge.l3tmp_genTemporalBlocking)
      Communication.exch(printer, s"Solution$postfix[active]@current", s"dup ghost [ ${ Array.fill(Knowledge.dimensionality)(0).mkString(", ") } ]")
    else
      Communication.exch(printer, s"Solution$postfix[active]@current")

    if (Knowledge.l3tmp_genFragLoops)
      printer.println(s"\tloop over fragments {")
    printer.println(s"\tloop over Residual$postfix@current {")
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims)
      printer.println(s"\t\t${ Fields.residual(s"current", postfix)(vecDim) } = ${ Fields.rhs(s"current", postfix)(vecDim) } - ($stencil * ${ Fields.solutionSlotted(s"current", "active", postfix)(vecDim) })")
    printer.println(s"\t}")
    if (Knowledge.l3tmp_genFragLoops)
      printer.println(s"\t}")

    if (Knowledge.l3tmp_genAsyncCommunication) {
      printer.println(s"\tif ( levels@current > levels@coarsest && startComm > 0 ) {")
      printer.println(s"\t\tbegin communicate Residual$postfix@current", "ghost")
      printer.println(s"\t}")
    }
  }

  def addUpdateFunction(printer : java.io.PrintWriter, postfix : String) = {
    val levels = if (Knowledge.l3tmp_genStencilStencilConv && !Knowledge.l3tmp_genStencilFields) "finest" else "all"
    val params = if (Knowledge.l3tmp_genAsyncCommunication) "startComm : Integer" else ""

    printer.println(s"Function UpResidual$postfix@$levels ( $params ) : Unit {")
    addUpdateBody(printer, postfix, s"Laplace$postfix@current")
    printer.println(s"}")

    if (Knowledge.l3tmp_genStencilStencilConv && !Knowledge.l3tmp_genStencilFields) {
      printer.println(s"Function UpResidual$postfix@(coarsest to (finest - 1)) ( $params ) : Unit {")
      addUpdateBody(printer, postfix, Stencils.access(postfix))
      printer.println(s"}")
    }
  }

  def addReductionFunction(printer : java.io.PrintWriter, postfix : String) = {
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims) {
      if (!Knowledge.l3tmp_useMaxNorm)
        printer.println(s"Function NormResidual${ postfix }_$vecDim@(coarsest and finest) ( ) : Real {")
      else
        printer.println(s"Function NormResidual${ postfix }_$vecDim@(coarsest) ( ) : Real {")

      printer.println(s"\tVariable res : Real = 0")
      if (Knowledge.l3tmp_genFragLoops)
        printer.println(s"\tloop over fragments with reduction( + : res ) {")
      if (Knowledge.l3tmp_genCellBasedDiscr)
        printer.println(s"\tloop over Residual$postfix@current with reduction( + : res ) {")
      else
        printer.println(s"\tloop over Residual$postfix@current where x > 0 && y > 0 ${ if (Knowledge.dimensionality > 2) "&& z > 0 " else "" }with reduction( + : res ) {")
      printer.println(s"\t\tres += ${ Fields.residual(s"current", postfix)(vecDim) } * ${ Fields.residual(s"current", postfix)(vecDim) }")
      printer.println(s"\t}")
      if (Knowledge.l3tmp_genFragLoops)
        printer.println(s"\t}")
      printer.println(s"\treturn sqrt ( res )")
      printer.println(s"}")
      printer.println

      if (Knowledge.l3tmp_useMaxNorm) {
        printer.println(s"Function NormResidual${ postfix }_$vecDim@(finest) ( ) : Real {")

        printer.println(s"\tVariable res : Real = 0")
        if (Knowledge.l3tmp_genFragLoops)
          printer.println(s"\tloop over fragments with reduction( max : res ) {")
        if (Knowledge.l3tmp_genCellBasedDiscr)
          printer.println(s"\tloop over Residual$postfix@current with reduction( max : res ) {")
        else
          printer.println(s"\tloop over Residual$postfix@current where x > 0 && y > 0 ${ if (Knowledge.dimensionality > 2) "&& z > 0 " else "" }with reduction( max : res ) {")
        printer.println(s"\t\tres = max(res, ${ Fields.residual(s"current", postfix)(vecDim) })")
        printer.println(s"\t}")
        if (Knowledge.l3tmp_genFragLoops)
          printer.println(s"\t}")
        printer.println(s"\treturn res")
        printer.println(s"}")
        printer.println
      }
    }
  }
}