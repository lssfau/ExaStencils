package exastencils.deprecated.l3Generate

import exastencils.config._

object Error {
  def addReductionFunction(printer : java.io.PrintWriter, postfix : String) = {
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims) {
      printer.println(s"Function NormError${ postfix }_$vecDim@(finest) ( ) : Real {")
      if (Knowledge.l3tmp_useMaxNormForError) {
        printer.println(s"\tVariable err : Real = 0")
        if (Knowledge.l3tmp_genFragLoops)
          printer.println(s"\tloop over fragments with reduction( max : err ) {")
        if (Knowledge.l3tmp_genCellBasedDiscr)
          printer.println(s"\tloop over Solution$postfix@current with reduction( max : err ) {")
        else
          printer.println(s"\tloop over Solution$postfix@current where x > 0 && y > 0 ${ if (Knowledge.dimensionality > 2) "&& z > 0 " else "" }with reduction( max : err ) {")
        printer.println(s"\t\tVariable curErr : Real = fabs ( ${ Fields.solutionSlotted(s"current", "active", postfix)(vecDim) } - ${ Functions.solFunction(false) } )")
        printer.println(s"\t\terr = max ( err, curErr )")
        printer.println(s"\t}")
        if (Knowledge.l3tmp_genFragLoops)
          printer.println(s"\t}")
        printer.println(s"\treturn ( err )")
      } else {
        printer.println(s"\tVariable err : Real = 0")
        if (Knowledge.l3tmp_genFragLoops)
          printer.println(s"\tloop over fragments with reduction( + : err ) {")
        if (Knowledge.l3tmp_genCellBasedDiscr)
          printer.println(s"\tloop over Solution$postfix@current with reduction( + : err ) {")
        else
          printer.println(s"\tloop over Solution$postfix@current where x > 0 && y > 0 ${ if (Knowledge.dimensionality > 2) "&& z > 0 " else "" }with reduction( + : err ) {")
        printer.println(s"\t\tVariable curErr : Real = ${ Fields.solutionSlotted(s"current", "active", postfix)(vecDim) } - ${ Functions.solFunction(false) }")
        printer.println(s"\t\terr += curErr * curErr")
        printer.println(s"\t}")
        if (Knowledge.l3tmp_genFragLoops)
          printer.println(s"\t}")
        printer.println(s"\treturn sqrt ( err )")
      }
      printer.println(s"}")
      printer.println()
    }
  }
}
