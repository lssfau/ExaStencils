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

      if (Knowledge.l3tmp_genInvDiagStencil) {
        printer.println(s"Field InvDiagLaplaceCoeff$postfix< $domain, ${if (Knowledge.l3tmp_genTemporalBlocking) "CommPartTempBlockable" else "NoComm"}, None >@all")
        printer.println(s"StencilField InvDiagLaplace$postfix< InvDiagLaplaceCoeff$postfix => InvDiagLaplaceStencil$postfix >@all")
        printer.println
      }
    }
  }
}