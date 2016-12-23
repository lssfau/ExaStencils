package exastencils.deprecated.l3Generate

import exastencils.config._

object Stencils {
  def access(postfix : String) : String = {
    if (Knowledge.l3tmp_genStencilStencilConv) {
      if (Knowledge.l3tmp_genStencilFields)
        s"( CorrectionStencil@current * ( Laplace$postfix@finer * RestrictionStencil@current ) )"
      else
        s"( CorrectionStencil@current * ( Laplace$postfix@finer * RestrictionStencil@current ) )"
    } else
      s"Laplace$postfix@current"
  }

  def addLaplaceStencil(printer : java.io.PrintWriter, postfix : String) = {
    // special coefficient function
    if ("Kappa_VC" == Knowledge.l3tmp_exactSolution) {
      printer.println(s"Function getCoefficient ( x : Real, y : Real${ if (Knowledge.dimensionality > 2) ", z : Real" else "" } ) : Real {")
      printer.println(s"\treturn exp ( kappa * ( (x - x ** 2) * (y - y ** 2) ${ if (Knowledge.dimensionality > 2) "* (z - z ** 2) " else "" }) )")
      printer.println(s"}")
    }

    if (Knowledge.l3tmp_genStencilFields)
      printer.println(s"Stencil LaplaceStencil$postfix@all {")
    else
      printer.println(s"Stencil Laplace$postfix@all {")

    for (e <- MainStencilCoefficients.getEntries(postfix))
      printer.println(s"\t${ e._1 } => ( ${ e._2 } )")

    printer.println("}")

    if (Knowledge.l3tmp_genInvDiagStencil) {
      if (Knowledge.l3tmp_genStencilFields) {
        printer.println(s"Stencil InvDiagLaplaceStencil$postfix@all {")
        Knowledge.dimensionality match {
          case 2 => printer.println(s"\t[ 0,  0] => ( 1.0 / LaplaceStencil$postfix@current:[0, 0] )")
          case 3 => printer.println(s"\t[ 0,  0,  0] => ( 1.0 / LaplaceStencil$postfix@current:[0, 0, 0] )")
        }
      } else {
        printer.println(s"Stencil InvDiagLaplace$postfix@all {")
        Knowledge.dimensionality match {
          case 2 => printer.println(s"\t[ 0,  0] => ( 1.0 / Laplace$postfix@current:[0, 0] )")
          case 3 => printer.println(s"\t[ 0,  0,  0] => ( 1.0 / Laplace$postfix@current:[0, 0, 0] )")
        }
      }
      printer.println(s"}")
    }
  }

  def addDefaultStencils(printer : java.io.PrintWriter) = {
    Knowledge.dimensionality match {
      case 2 =>
        printer.println("Stencil CorrectionStencil@all {")
        if (Knowledge.l3tmp_genCellBasedDiscr) {
          val coeff = if (Knowledge.l3tmp_genHDepStencils) "1.0" else "1.0"
          printer.println(s"\t[ 0, 0] => $coeff")
        } else {
          val coeff = if (Knowledge.l3tmp_genHDepStencils) "0.25" else "0.25"
          printer.println(s"\t[    0,     0] => $coeff")
          printer.println(s"\t[x % 2,     0] => $coeff")
          printer.println(s"\t[    0, y % 2] => $coeff")
          printer.println(s"\t[x % 2, y % 2] => $coeff")
        }
        printer.println("}")
      case 3 =>
        printer.println("Stencil CorrectionStencil@all {")
        if (Knowledge.l3tmp_genCellBasedDiscr) {
          val coeff = if (Knowledge.l3tmp_genHDepStencils) "1.0" else "1.0"
          printer.println(s"\t[ 0, 0, 0] => $coeff")
        } else {
          val coeff = if (Knowledge.l3tmp_genHDepStencils) "0.125" else "0.0625"
          printer.println(s"\t[    0,     0,     0] => $coeff")
          printer.println(s"\t[x % 2,     0,     0] => $coeff")
          printer.println(s"\t[    0, y % 2,     0] => $coeff")
          printer.println(s"\t[x % 2, y % 2,     0] => $coeff")
          printer.println(s"\t[    0,     0, z % 2] => $coeff")
          printer.println(s"\t[x % 2,     0, z % 2] => $coeff")
          printer.println(s"\t[    0, y % 2, z % 2] => $coeff")
          printer.println(s"\t[x % 2, y % 2, z % 2] => $coeff")
        }
        printer.println("}")
    }

    Knowledge.dimensionality match {
      case 2 =>
        printer.println("Stencil RestrictionStencil@all {")
        if (Knowledge.l3tmp_genCellBasedDiscr) {
          val coeff = if (Knowledge.l3tmp_genHDepStencils) "( 1.0 / 4.0 )" else "( 4.0 / 4.0 )"
          printer.println(s"\t[ 0,  0] => $coeff")
          printer.println(s"\t[ 0,  1] => $coeff")
          printer.println(s"\t[ 1,  0] => $coeff")
          printer.println(s"\t[ 1,  1] => $coeff")
        } else {
          val coeffDist0 = if (Knowledge.l3tmp_genHDepStencils) "( 1.0 / 4.0 )" else "1.0"
          printer.println(s"\t[ 0,  0] => $coeffDist0")

          val coeffDist1 = if (Knowledge.l3tmp_genHDepStencils) "( 1.0 / 8.0 )" else "0.5"
          printer.println(s"\t[ 0, -1] => $coeffDist1")
          printer.println(s"\t[ 0,  1] => $coeffDist1")
          printer.println(s"\t[-1,  0] => $coeffDist1")
          printer.println(s"\t[ 1,  0] => $coeffDist1")

          val coeffDist2 = if (Knowledge.l3tmp_genHDepStencils) "( 1.0 / 16.0 )" else "0.25"
          printer.println(s"\t[-1, -1] => $coeffDist2")
          printer.println(s"\t[-1,  1] => $coeffDist2")
          printer.println(s"\t[ 1, -1] => $coeffDist2")
          printer.println(s"\t[ 1,  1] => $coeffDist2")
        }
        printer.println("}")
      case 3 =>
        printer.println("Stencil RestrictionStencil@all {")
        if (Knowledge.l3tmp_genCellBasedDiscr) {
          val coeff = if (Knowledge.l3tmp_genHDepStencils) "( 1.0 / 8.0 )" else "( 4.0 / 8.0 )"
          printer.println(s"\t[ 0,  0,  0] => $coeff")
          printer.println(s"\t[ 0,  0,  1] => $coeff")
          printer.println(s"\t[ 0,  1,  0] => $coeff")
          printer.println(s"\t[ 1,  0,  0] => $coeff")
          printer.println(s"\t[ 0,  1,  1] => $coeff")
          printer.println(s"\t[ 1,  0,  1] => $coeff")
          printer.println(s"\t[ 1,  1,  0] => $coeff")
          printer.println(s"\t[ 1,  1,  1] => $coeff")
        } else {
          val coeffDist0 = if (Knowledge.l3tmp_genHDepStencils) "( 1.0 / 8.0 )" else "1.0"
          printer.println(s"\t[ 0,  0,  0] => $coeffDist0")

          val coeffDist1 = if (Knowledge.l3tmp_genHDepStencils) "( 1.0 / 16.0 )" else "0.5"
          printer.println(s"\t[ 0,  0, -1] => $coeffDist1")
          printer.println(s"\t[ 0,  0,  1] => $coeffDist1")
          printer.println(s"\t[ 0, -1,  0] => $coeffDist1")
          printer.println(s"\t[ 0,  1,  0] => $coeffDist1")
          printer.println(s"\t[-1,  0,  0] => $coeffDist1")
          printer.println(s"\t[ 1,  0,  0] => $coeffDist1")

          val coeffDist2 = if (Knowledge.l3tmp_genHDepStencils) "( 1.0 / 32.0 )" else "0.25"
          printer.println(s"\t[ 0, -1,  1] => $coeffDist2")
          printer.println(s"\t[ 0, -1, -1] => $coeffDist2")
          printer.println(s"\t[ 0,  1,  1] => $coeffDist2")
          printer.println(s"\t[ 0,  1, -1] => $coeffDist2")
          printer.println(s"\t[-1,  0,  1] => $coeffDist2")
          printer.println(s"\t[-1,  0, -1] => $coeffDist2")
          printer.println(s"\t[ 1,  0,  1] => $coeffDist2")
          printer.println(s"\t[ 1,  0, -1] => $coeffDist2")
          printer.println(s"\t[-1, -1,  0] => $coeffDist2")
          printer.println(s"\t[-1,  1,  0] => $coeffDist2")
          printer.println(s"\t[ 1, -1,  0] => $coeffDist2")
          printer.println(s"\t[ 1,  1,  0] => $coeffDist2")

          val coeffDist3 = if (Knowledge.l3tmp_genHDepStencils) "( 1.0 / 64.0 )" else "0.125"
          printer.println(s"\t[-1, -1,  1] => $coeffDist3")
          printer.println(s"\t[-1, -1, -1] => $coeffDist3")
          printer.println(s"\t[-1,  1,  1] => $coeffDist3")
          printer.println(s"\t[-1,  1, -1] => $coeffDist3")
          printer.println(s"\t[ 1, -1,  1] => $coeffDist3")
          printer.println(s"\t[ 1, -1, -1] => $coeffDist3")
          printer.println(s"\t[ 1,  1,  1] => $coeffDist3")
          printer.println(s"\t[ 1,  1, -1] => $coeffDist3")
        }
        printer.println("}")
    }
    printer.println()

    if (Knowledge.l3tmp_kelvin) {
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