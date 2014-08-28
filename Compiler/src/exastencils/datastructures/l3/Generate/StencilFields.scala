package exastencils.datastructures.l3

import exastencils.knowledge._

object StencilFields {
  def addStencilFields(printer : java.io.PrintWriter, postfix : String, domain : String) = {
    if (Knowledge.l3tmp_genStencilFields) {
      if (Knowledge.l3tmp_genStencilStencilConv)
        printer.println(s"Field LaplaceCoeff$postfix< Array[Real][${var res = 1; for (i <- 0 until Knowledge.dimensionality) res *= 3; res}], $domain, CommFullTempBlockable, 0.0 >@all")
      else
        printer.println(s"Field LaplaceCoeff$postfix< Array[Real][${2 * Knowledge.dimensionality + 1}], $domain, CommPartTempBlockable, None >@all")
      printer.println(s"StencilField Laplace$postfix< LaplaceCoeff$postfix => LaplaceStencil$postfix >@all")
      printer.println
    }
  }
}