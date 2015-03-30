package exastencils.datastructures.l3

import exastencils.knowledge._

object InitFields {
  def addFunction(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"Function InitSolution$postfix ( ) : Unit {")
    if (Knowledge.l3tmp_initSolWithRand) {
      // FIXME: seq HACK
      printer.println(s"\tloop over Solution$postfix@finest sequentially {")
      for (vecDim <- 0 until Knowledge.l3tmp_numVecDims) {
        printer.println(s"\t\t${Fields.solutionSlotted(s"finest", "active", postfix)(vecDim)} = native('((${if (Knowledge.useDblPrecision) "double" else "float"})std::rand()/RAND_MAX)')")
      }
      printer.println(s"\t}")
    } else {
      printer.println(s"\tloop over Solution$postfix@finest {")
      for (vecDim <- 0 until Knowledge.l3tmp_numVecDims) {
        printer.println(s"\t\t${Fields.solutionSlotted(s"finest", "active", postfix)(vecDim)} = 0")
      }
      printer.println(s"\t}")
    }

    printer.println(s"}")

    printer.println(s"Function InitRHS$postfix ( ) : Unit {")
    printer.println(s"\tloop over RHS$postfix@finest {")
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims) {
      printer.println(s"\t\t${Fields.rhs(s"finest", postfix)(vecDim)} = ${Functions.rhsFunction}")
    }
    printer.println(s"\t}")
    printer.println(s"}")

    if (Knowledge.l3tmp_genStencilFields) {
      if (Knowledge.l3tmp_genStencilStencilConv) {
        printer.println(s"Function InitLaplace$postfix@finest ( ) : Unit {")
        printer.println(s"\tloop over LaplaceCoeff$postfix@current {")
      } else {
        printer.println(s"Function InitLaplace$postfix@all ( ) : Unit {")
        printer.println(s"\tloop over LaplaceCoeff$postfix@current {")
      }

      for (e <- MainStencilCoefficients.getEntries(postfix))
        printer.println(s"\t\tLaplace$postfix@current:${e._1} = ${e._2}")

      if (Knowledge.l3tmp_genInvDiagStencil) {
        printer.println
        Knowledge.dimensionality match {
          case 2 => printer.println(s"\t\tInvDiagLaplace$postfix@current:[ 0,  0] = 1.0 / Laplace@current:[ 0,  0]")
          case 3 => printer.println(s"\t\tInvDiagLaplace$postfix@current:[ 0,  0,  0] = 1.0 / Laplace@current:[ 0,  0,  0]")
        }
      }

      printer.println(s"\t}")

      if (Knowledge.l3tmp_genTemporalBlocking) {
        printer.println
        printer.println(s"\tif (levels@current >= ${Knowledge.l3tmp_tempBlockingMinLevel}) {")
        printer.println(s"\t\tcommunicate LaplaceCoeff$postfix@current")
        if (Knowledge.l3tmp_genInvDiagStencil)
          printer.println(s"\t\tcommunicate InvDiagLaplaceCoeff$postfix@current")
        printer.println(s"}")
      }

      printer.println(s"}")

      if (Knowledge.l3tmp_genStencilStencilConv) {
        printer.println(s"Function InitLaplace$postfix@(coarsest to (finest - 1)) ( ) : Unit {")
        Communication.exch(printer, s"LaplaceCoeff$postfix@finer")

        printer.println(s"\tloop over LaplaceCoeff$postfix@current {")
        if (false && Knowledge.l3tmp_kelvin) { // hack injection
          for (i <- 0 until 9)
            printer.println(s"\t\tLaplaceCoeff$postfix@current[$i] = LaplaceCoeff$postfix@finer[$i]")
        } else {
          printer.println(s"\t\tLaplace$postfix@current = ( CorrectionStencil@current * ( Laplace$postfix@finer * RestrictionStencil@current ) )")
        }
        printer.println(s"\t}")
        printer.println(s"}")
      }
    }

    printer.println
  }
}