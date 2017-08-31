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

  def addLaplaceStencil(printer : java.io.PrintStream, postfix : String) = {
    // special coefficient function
    if ("Kappa_VC" == Knowledge.l3tmp_exactSolution) {
      printer.println(s"Function getCoefficient ( xPos : Real, yPos : Real${ if (Knowledge.dimensionality > 2) ", zPos : Real" else "" } ) : Real {")
      printer.println(s"\treturn exp ( kappa * ( (xPos - xPos ** 2) * (yPos - yPos ** 2) ${ if (Knowledge.dimensionality > 2) "* (zPos - zPos ** 2) " else "" }) )")
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

  def addDefaultStencils(printer : java.io.PrintStream) = {
    printer.print("Stencil CorrectionStencil@all from default prolongation on ")
    if (Knowledge.l3tmp_genCellBasedDiscr)
      printer.print("Cell")
    else
      printer.print("Node")
    printer.println(" with 'linear'")
    printer.println()

    printer.print("Stencil RestrictionStencil@all from default restriction on ")
    if (Knowledge.l3tmp_genCellBasedDiscr)
      printer.print("Cell")
    else
      printer.print("Node")
    printer.println(" with 'linear'")
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