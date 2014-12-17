package exastencils.datastructures.l3

import exastencils.knowledge._

object StencilFields {
  def addStencilFields(printer : java.io.PrintWriter, postfix : String, domain : String) = {
    if (Knowledge.l3tmp_genStencilFields) {
      if (Knowledge.l3tmp_genStencilStencilConv)
        printer.println(s"Field LaplaceCoeff$postfix< $domain, CommFullTempBlockableSF, 0.0 >@all")
      else
        printer.println(s"Field LaplaceCoeff$postfix< $domain, ${if (Knowledge.l3tmp_genTemporalBlocking) "CommPartTempBlockableSF" else "NoCommSF"}, None >@all")
      printer.println(s"StencilField Laplace$postfix< LaplaceCoeff$postfix => LaplaceStencil$postfix >@all")
      printer.println
    }
  }
}