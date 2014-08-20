package exastencils.datastructures.l3

import exastencils.knowledge._

object Stencils {
  def access(postfix : String) : String = {
    if (Knowledge.testStencilStencil) {
      if (Knowledge.genStencilFields)
        s"( CorrectionStencil@current * ( Laplace$postfix@finer * RestrictionStencil@current ) )"
      else
        s"( CorrectionStencil@current * ( Laplace$postfix@finer * RestrictionStencil@current ) )"
    } else
      s"Laplace$postfix@current"
  }

  def addLaplaceStencil(printer : java.io.PrintWriter, postfix : String) = {
    if (Knowledge.genStencilFields)
      printer.println(s"Stencil LaplaceStencil$postfix@all {")
    else
      printer.println(s"Stencil Laplace$postfix@all {")
    if (Knowledge.genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("\t[ 0,  0] => Laplace_Coeff_0_0")
          printer.println("\t[ 1,  0] => Laplace_Coeff_P1_0")
          printer.println("\t[-1,  0] => Laplace_Coeff_N1_0")
          printer.println("\t[ 0,  1] => Laplace_Coeff_0_P1")
          printer.println("\t[ 0, -1] => Laplace_Coeff_0_N1")
        }
        case 3 =>
          printer.println("\t[ 0,  0,  0] => Laplace_Coeff_0_0_0")
          printer.println("\t[ 1,  0,  0] => Laplace_Coeff_P1_0_0")
          printer.println("\t[-1,  0,  0] => Laplace_Coeff_N1_0_0")
          printer.println("\t[ 0,  1,  0] => Laplace_Coeff_0_P1_0")
          printer.println("\t[ 0, -1,  0] => Laplace_Coeff_0_N1_0")
          printer.println("\t[ 0,  0,  1] => Laplace_Coeff_0_0_P1")
          printer.println("\t[ 0,  0, -1] => Laplace_Coeff_0_0_N1")
      }
    } else {
      Knowledge.dimensionality match {
        case 2 => {
          if (Knowledge.kelvin && "_GMRF" == postfix)
            printer.println("\t[ 0,  0] => (4.0 + kappa)")
          else
            printer.println("\t[ 0,  0] => 4.0")
          printer.println("\t[ 1,  0] => -1.0")
          printer.println("\t[-1,  0] => -1.0")
          printer.println("\t[ 0,  1] => -1.0")
          printer.println("\t[ 0, -1] => -1.0")
          if (Knowledge.testStencilStencil) {
            printer.println("\t[-1, -1] => 0.0")
            printer.println("\t[-1,  1] => 0.0")
            printer.println("\t[ 1, -1] => 0.0")
            printer.println("\t[ 1,  1] => 0.0")
          }
        }
        case 3 =>
          printer.println("\t[ 0,  0,  0] => 6.0")
          printer.println("\t[ 1,  0,  0] => -1.0")
          printer.println("\t[-1,  0,  0] => -1.0")
          printer.println("\t[ 0,  1,  0] => -1.0")
          printer.println("\t[ 0, -1,  0] => -1.0")
          printer.println("\t[ 0,  0,  1] => -1.0")
          printer.println("\t[ 0,  0, -1] => -1.0")
          if (Knowledge.testStencilStencil) {
            printer.println("\t[ 0, -1,  1] => 0.0")
            printer.println("\t[ 0, -1, -1] => 0.0")
            printer.println("\t[ 0,  1,  1] => 0.0")
            printer.println("\t[ 0,  1, -1] => 0.0")
            printer.println("\t[-1,  0,  1] => 0.0")
            printer.println("\t[-1,  0, -1] => 0.0")
            printer.println("\t[ 1,  0,  1] => 0.0")
            printer.println("\t[ 1,  0, -1] => 0.0")
            printer.println("\t[-1, -1,  0] => 0.0")
            printer.println("\t[-1,  1,  0] => 0.0")
            printer.println("\t[ 1, -1,  0] => 0.0")
            printer.println("\t[ 1,  1,  0] => 0.0")

            printer.println("\t[-1, -1,  1] => 0.0")
            printer.println("\t[-1, -1, -1] => 0.0")
            printer.println("\t[-1,  1,  1] => 0.0")
            printer.println("\t[-1,  1, -1] => 0.0")
            printer.println("\t[ 1, -1,  1] => 0.0")
            printer.println("\t[ 1, -1, -1] => 0.0")
            printer.println("\t[ 1,  1,  1] => 0.0")
            printer.println("\t[ 1,  1, -1] => 0.0")
          }
      }
    }
    printer.println("}")
  }

  def addDefaultStencils(printer : java.io.PrintWriter) = {
    Knowledge.dimensionality match {
      case 2 => {
        printer.println("Stencil CorrectionStencil@all {")
        printer.println("\t[    0,     0] => 0.25")
        printer.println("\t[x % 2,     0] => 0.25")
        printer.println("\t[    0, y % 2] => 0.25")
        printer.println("\t[x % 2, y % 2] => 0.25")
        printer.println("}")
      }
      case 3 => {
        printer.println("Stencil CorrectionStencil@all {")
        printer.println("\t[    0,     0,     0] => 0.0625")
        printer.println("\t[x % 2,     0,     0] => 0.0625")
        printer.println("\t[    0, y % 2,     0] => 0.0625")
        printer.println("\t[x % 2, y % 2,     0] => 0.0625")
        printer.println("\t[    0,     0, z % 2] => 0.0625")
        printer.println("\t[x % 2,     0, z % 2] => 0.0625")
        printer.println("\t[    0, y % 2, z % 2] => 0.0625")
        printer.println("\t[x % 2, y % 2, z % 2] => 0.0625")
        printer.println("}")
      }
    }

    Knowledge.dimensionality match {
      case 2 => {
        printer.println("Stencil RestrictionStencil@all {")
        printer.println("\t[ 0,  0] => 1.0")

        printer.println("\t[ 0, -1] => 0.5")
        printer.println("\t[ 0,  1] => 0.5")
        printer.println("\t[-1,  0] => 0.5")
        printer.println("\t[ 1,  0] => 0.5")

        printer.println("\t[-1, -1] => 0.25")
        printer.println("\t[-1,  1] => 0.25")
        printer.println("\t[ 1, -1] => 0.25")
        printer.println("\t[ 1,  1] => 0.25")
        printer.println("}")
      }
      case 3 => {
        printer.println("Stencil RestrictionStencil@all {")
        printer.println("\t[ 0,  0,  0] => 1.0")

        printer.println("\t[ 0,  0, -1] => 0.5")
        printer.println("\t[ 0,  0,  1] => 0.5")
        printer.println("\t[ 0, -1,  0] => 0.5")
        printer.println("\t[ 0,  1,  0] => 0.5")
        printer.println("\t[-1,  0,  0] => 0.5")
        printer.println("\t[ 1,  0,  0] => 0.5")

        printer.println("\t[ 0, -1,  1] => 0.25")
        printer.println("\t[ 0, -1, -1] => 0.25")
        printer.println("\t[ 0,  1,  1] => 0.25")
        printer.println("\t[ 0,  1, -1] => 0.25")
        printer.println("\t[-1,  0,  1] => 0.25")
        printer.println("\t[-1,  0, -1] => 0.25")
        printer.println("\t[ 1,  0,  1] => 0.25")
        printer.println("\t[ 1,  0, -1] => 0.25")
        printer.println("\t[-1, -1,  0] => 0.25")
        printer.println("\t[-1,  1,  0] => 0.25")
        printer.println("\t[ 1, -1,  0] => 0.25")
        printer.println("\t[ 1,  1,  0] => 0.25")

        printer.println("\t[-1, -1,  1] => 0.125")
        printer.println("\t[-1, -1, -1] => 0.125")
        printer.println("\t[-1,  1,  1] => 0.125")
        printer.println("\t[-1,  1, -1] => 0.125")
        printer.println("\t[ 1, -1,  1] => 0.125")
        printer.println("\t[ 1, -1, -1] => 0.125")
        printer.println("\t[ 1,  1,  1] => 0.125")
        printer.println("\t[ 1,  1, -1] => 0.125")
        printer.println("}")
      }
    }
    printer.println

    if (Knowledge.kelvin) {
      printer.println("Stencil TransferStencil_Center@all {")
      printer.println("\t[ 0,  0] => 2.0")
      printer.println("\t[-1,  0] => 0.5")
      printer.println("\t[ 1,  0] => 0.5")
      printer.println("\t[ 0,  1] => 0.5")
      printer.println("\t[ 0, -1] => 0.5")
      printer.println("}")
      printer.println("Stencil TransferStencil_Left@all {")
      printer.println("\t[ 0,  0] => -0.5")
      printer.println("\t[-1,  0] => -0.5")
      printer.println("}")
      printer.println("Stencil TransferStencil_Right@all {")
      printer.println("\t[ 0,  0] => -0.5")
      printer.println("\t[ 1,  0] => -0.5")
      printer.println("}")
      printer.println("Stencil TransferStencil_Up@all {")
      printer.println("\t[ 0,  0] => -0.5")
      printer.println("\t[ 0,  1] => -0.5")
      printer.println("}")
      printer.println("Stencil TransferStencil_Down@all {")
      printer.println("\t[ 0,  0] => -0.5")
      printer.println("\t[ 0, -1] => -0.5")
      printer.println("}")
    }
  }
}