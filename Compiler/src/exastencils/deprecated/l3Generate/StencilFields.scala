package exastencils.deprecated.l3Generate

import exastencils.config._

object StencilFields {
  def addStencilFields(printer : java.io.PrintWriter, postfix : String, domain : String) = {
    if (Knowledge.l3tmp_genStencilFields) {
      printer.println(s"Field LaplaceCoeff$postfix< $domain, NoCommSF, 0.0 >@(coarsest to ${ Knowledge.l3tmp_tempBlockingMinLevel - 1 })")
      if (Knowledge.l3tmp_genStencilStencilConv)
        printer.println(s"Field LaplaceCoeff$postfix< $domain, CommFullTempBlockableSF, 0.0 >@(${ Knowledge.l3tmp_tempBlockingMinLevel } to finest)")
      else
        printer.println(s"Field LaplaceCoeff$postfix< $domain, ${ if (Knowledge.l3tmp_genTemporalBlocking) "CommPartTempBlockableSF" else "NoCommSF" }, None >@(${ Knowledge.l3tmp_tempBlockingMinLevel } to finest)")
      printer.println(s"StencilField Laplace$postfix< LaplaceCoeff$postfix => LaplaceStencil$postfix >@all")
      printer.println

      if (Knowledge.l3tmp_genInvDiagStencil) {
        printer.println(s"Field InvDiagLaplaceCoeff$postfix< $domain, NoComm, None >@(coarsest to ${ Knowledge.l3tmp_tempBlockingMinLevel - 1 })")
        printer.println(s"Field InvDiagLaplaceCoeff$postfix< $domain, ${ if (Knowledge.l3tmp_genTemporalBlocking) "CommPartTempBlockable" else "NoComm" }, None >@(${ Knowledge.l3tmp_tempBlockingMinLevel } to finest)")
        printer.println(s"StencilField InvDiagLaplace$postfix< InvDiagLaplaceCoeff$postfix => InvDiagLaplaceStencil$postfix >@all")
        printer.println
      }
    }
  }
}