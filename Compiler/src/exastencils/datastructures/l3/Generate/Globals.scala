package exastencils.datastructures.l3

import exastencils.knowledge._

object Globals {
  def addGlobals(printer : java.io.PrintWriter) = {
    printer.println("Globals {")
    if (Knowledge.l3tmp_genGlobalOmega)
      printer.println(s"\tVariable l3tmp_omega : Real = ${Knowledge.l3tmp_omega}")
    if (Knowledge.l3tmp_genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("\tVariable Laplace_Coeff_0_0 : Real")
          printer.println("\tVariable Laplace_Coeff_P1_0 : Real")
          printer.println("\tVariable Laplace_Coeff_N1_0 : Real")
          printer.println("\tVariable Laplace_Coeff_0_P1 : Real")
          printer.println("\tVariable Laplace_Coeff_0_N1 : Real")
        }
        case 3 => {
          printer.println("\tVariable Laplace_Coeff_0_0_0 : Real")
          printer.println("\tVariable Laplace_Coeff_P1_0_0 : Real")
          printer.println("\tVariable Laplace_Coeff_N1_0_0 : Real")
          printer.println("\tVariable Laplace_Coeff_0_P1_0 : Real")
          printer.println("\tVariable Laplace_Coeff_0_N1_0 : Real")
          printer.println("\tVariable Laplace_Coeff_0_0_P1 : Real")
          printer.println("\tVariable Laplace_Coeff_0_0_N1 : Real")
        }
      }
    }
    if (Knowledge.l3tmp_kelvin) {
      // Dir BC
      printer.println("\tVariable UN : Real = 1")
      printer.println("\tVariable US : Real = 10")
      printer.println("\tVariable UE : Real = 5")
      printer.println("\tVariable UW : Real = 3")
      // other parameters
      printer.println("\tVariable alpha : Integer = 2")
      printer.println("\tVariable sigma : Real = 0.3")
      printer.println("\tVariable lambda : Real = 0.1")
      printer.println("\tVariable nu : Real = 1 // alpha - dim/2")
      printer.println("\tVariable kappa : Real = sqrt( 8 * nu ) / ( lambda )")
      printer.println("\tVariable dim : Real = 2")
    }
    if (Knowledge.l3tmp_genHDepStencils) {
      // TODO: currently assumes (one) unit domain
      // FIXME: replace, add fragLen, etc
      for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
        printer.println(s"\tVariable hx@$level : Real = ( 1.0 / ${Knowledge.domain_rect_numFragsTotal_x * (1 << level)}.0 )")
        if (Knowledge.dimensionality > 1)
          printer.println(s"\tVariable hy@$level : Real = ( 1.0 / ${Knowledge.domain_rect_numFragsTotal_y * (1 << level)}.0 )")
        if (Knowledge.dimensionality > 2)
          printer.println(s"\tVariable hz@$level : Real = ( 1.0 / ${Knowledge.domain_rect_numFragsTotal_z * (1 << level)}.0 )")
      }
    }
    printer.println("}")
    printer.println
  }
}