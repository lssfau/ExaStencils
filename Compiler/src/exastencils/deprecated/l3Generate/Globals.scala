package exastencils.deprecated.l3Generate

import exastencils.knowledge._

object Globals {
  def addGlobals(printer : java.io.PrintWriter) = {
    printer.println("Globals {")
    if (Knowledge.l3tmp_genGlobalOmega)
      printer.println(s"\tValue l3tmp_omega : Real = ${ Knowledge.l3tmp_omega }")
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
      printer.println("\tValue UN : Real = 1")
      printer.println("\tValue US : Real = 10")
      printer.println("\tValue UE : Real = 5")
      printer.println("\tValue UW : Real = 3")
      // other parameters
      printer.println("\tValue alpha : Integer = 2")
      printer.println("\tValue sigma : Real = 0.3")
      printer.println("\tValue lambda : Real = 0.1")
      printer.println("\tValue nu : Real = 1 // alpha - dim/2")
      printer.println("\tValue kappa : Real = sqrt( 8 * nu ) / ( lambda )")
      printer.println("\tValue dim : Real = 2")
    }

    if ("Kappa" == Knowledge.l3tmp_exactSolution || "Kappa_VC" == Knowledge.l3tmp_exactSolution) {
      printer.println("\tValue kappa : Real = 10.0")
    }

    printer.println("}")
    printer.println
  }
}