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
      Knowledge.dimensionality match {
        case 2 => {
          //    printer.println(s"\t\tLaplace$postfix@current = LaplaceStencil$postfix@current")

          printer.println(s"\t\tLaplace$postfix@current:[ 0,  0] = ${if (!Knowledge.l3tmp_genHDepStencils) 4 else "( 2.0 / ( hx@current * hx@current ) + 2.0 / ( hy@current * hy@current ) )"}")
          printer.println(s"\t\tLaplace$postfix@current:[ 1,  0] = ${if (!Knowledge.l3tmp_genHDepStencils) -1 else "( -1.0 / ( hx@current * hx@current ) )"}")
          printer.println(s"\t\tLaplace$postfix@current:[-1,  0] = ${if (!Knowledge.l3tmp_genHDepStencils) -1 else "( -1.0 / ( hx@current * hx@current ) )"}")
          printer.println(s"\t\tLaplace$postfix@current:[ 0,  1] = ${if (!Knowledge.l3tmp_genHDepStencils) -1 else "( -1.0 / ( hy@current * hy@current ) )"}")
          printer.println(s"\t\tLaplace$postfix@current:[ 0, -1] = ${if (!Knowledge.l3tmp_genHDepStencils) -1 else "( -1.0 / ( hy@current * hy@current ) )"}")
        }
        case 3 => {
          //    printer.println(s"\t\tLaplace$postfix@current = LaplaceStencil$postfix@current")

          printer.println(s"\t\tLaplace$postfix@current:[ 0,  0,  0] = ${if (!Knowledge.l3tmp_genHDepStencils) 6 else "( 2.0 / ( hx@current * hx@current ) + 2.0 / ( hy@current * hy@current ) + 2.0 / ( hz@current * hz@current ) )"}")
          printer.println(s"\t\tLaplace$postfix@current:[ 1,  0,  0] = ${if (!Knowledge.l3tmp_genHDepStencils) -1 else "( -1.0 / ( hx@current * hx@current ) )"}")
          printer.println(s"\t\tLaplace$postfix@current:[-1,  0,  0] = ${if (!Knowledge.l3tmp_genHDepStencils) -1 else "( -1.0 / ( hx@current * hx@current ) )"}")
          printer.println(s"\t\tLaplace$postfix@current:[ 0,  1,  0] = ${if (!Knowledge.l3tmp_genHDepStencils) -1 else "( -1.0 / ( hy@current * hy@current ) )"}")
          printer.println(s"\t\tLaplace$postfix@current:[ 0, -1,  0] = ${if (!Knowledge.l3tmp_genHDepStencils) -1 else "( -1.0 / ( hy@current * hy@current ) )"}")
          printer.println(s"\t\tLaplace$postfix@current:[ 0,  0,  1] = ${if (!Knowledge.l3tmp_genHDepStencils) -1 else "( -1.0 / ( hz@current * hz@current ) )"}")
          printer.println(s"\t\tLaplace$postfix@current:[ 0,  0, -1] = ${if (!Knowledge.l3tmp_genHDepStencils) -1 else "( -1.0 / ( hz@current * hzx@current ) )"}")
        }
      }
      printer.println(s"\t}")
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