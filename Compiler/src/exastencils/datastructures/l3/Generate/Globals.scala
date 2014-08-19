package exastencils.datastructures.l3

import exastencils.knowledge._

object Globals {
  def addGlobals(printer : java.io.PrintWriter) = {
    printer.println("Globals {")
    if (Knowledge.omegaViaGlobals)
      printer.println(s"\tvar omega : Real = ${Knowledge.omega}")
    if (Knowledge.genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("\tvar Laplace_Coeff_0_0 : Real")
          printer.println("\tvar Laplace_Coeff_P1_0 : Real")
          printer.println("\tvar Laplace_Coeff_N1_0 : Real")
          printer.println("\tvar Laplace_Coeff_0_P1 : Real")
          printer.println("\tvar Laplace_Coeff_0_N1 : Real")
        }
        case 3 => {
          printer.println("\tvar Laplace_Coeff_0_0_0 : Real")
          printer.println("\tvar Laplace_Coeff_P1_0_0 : Real")
          printer.println("\tvar Laplace_Coeff_N1_0_0 : Real")
          printer.println("\tvar Laplace_Coeff_0_P1_0 : Real")
          printer.println("\tvar Laplace_Coeff_0_N1_0 : Real")
          printer.println("\tvar Laplace_Coeff_0_0_P1 : Real")
          printer.println("\tvar Laplace_Coeff_0_0_N1 : Real")
        }
      }
    }
    if (Knowledge.kelvin) {
      // Dir BC
      printer.println("\tvar UN : Real = 1")
      printer.println("\tvar US : Real = 10")
      printer.println("\tvar UE : Real = 5")
      printer.println("\tvar UW : Real = 3")
      // other parameters
      printer.println("\tvar alpha : Integer = 2")
      printer.println("\tvar sigma : Real = 0.3")
      printer.println("\tvar lambda : Real = 0.1")
      printer.println("\tvar nu : Real = 1 // alpha - dim/2")
      printer.println("\tvar kappa : Real = sqrt( 8 * nu ) / ( lambda )")
      printer.println("\tvar dim : Real = 2")
    }
    printer.println("}")
    printer.println
  }
}