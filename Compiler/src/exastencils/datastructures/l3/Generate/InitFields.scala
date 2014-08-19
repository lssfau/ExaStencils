package exastencils.datastructures.l3

import exastencils.knowledge._

object InitFields {
  def addFunction(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"def InitSolution$postfix ( ) : Unit {")
    if (Knowledge.initSolWithRand) {
      // FIXME: this loop needs to be marked as non-parallelizable somehow
      // FIXME: make results more reproducible via sth like 'std::srand((unsigned int)fragments[f]->id)'
      printer.println(s"\tloop over Solution$postfix@finest {")
      for (vecDim <- 0 until Knowledge.numVecDims) {
        printer.println(s"\t\t${Fields.solution(s"finest", postfix)(vecDim)} = native('((double)std::rand()/RAND_MAX)')")
      }
      printer.println(s"\t}")
    } else {
      printer.println(s"\tloop over Solution$postfix@finest {")
      for (vecDim <- 0 until Knowledge.numVecDims) {
        printer.println(s"\t\t${Fields.solution(s"finest", postfix)(vecDim)} = 0")
      }
      printer.println(s"\t}")
    }

    printer.println(s"}")

    printer.println(s"def InitRHS$postfix ( ) : Unit {")
    printer.println(s"\tloop over RHS$postfix@finest {")
    for (vecDim <- 0 until Knowledge.numVecDims) {
      printer.println(s"\t\t${Fields.rhs(s"finest", postfix)(vecDim)} = 0")
    }
    printer.println(s"\t}")
    printer.println(s"}")

    if (Knowledge.genStencilFields) {
      if (Knowledge.testStencilStencil) {
        printer.println(s"def InitLaplace$postfix@finest ( ) : Unit {")
        printer.println(s"\tloop over LaplaceCoeff$postfix@current {")
      } else {
        printer.println(s"def InitLaplace$postfix@all ( ) : Unit {")
        printer.println(s"\tloop over LaplaceCoeff$postfix@current {")
      }
      Knowledge.dimensionality match {
        case 2 => {
          printer.println(s"\t\tLaplace$postfix@current = LaplaceStencil$postfix@current")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[0] = 4")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[1] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[2] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[3] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[4] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[ 0,  0] = 4")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[ 1,  0] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[-1,  0] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[ 0,  1] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[ 0, -1] = -1")
        }
        case 3 => {
          printer.println(s"\t\tLaplace$postfix@current = LaplaceStencil$postfix@current")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[0] = 6")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[1] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[2] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[3] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[4] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[5] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[6] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[ 0,  0,  0] = 6")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[ 1,  0,  0] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[-1,  0,  0] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[ 0,  1,  0] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[ 0, -1,  0] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[ 0,  0,  1] = -1")
          //    printer.println(s"\t\tLaplaceCoeff$postfix@current[ 0,  0, -1] = -1")
        }
      }
      printer.println(s"\t}")
      printer.println(s"}")

      if (Knowledge.testStencilStencil) {
        printer.println(s"def InitLaplace$postfix@(coarsest to (finest - 1)) ( ) : Unit {")
        Communication.exch(printer, s"LaplaceCoeff$postfix@finer")

        printer.println(s"\tloop over LaplaceCoeff$postfix@current {")
        if (false && Knowledge.kelvin) { // hack injection
          for (i <- 0 until 9)
            printer.println(s"\t\tLaplaceCoeff$postfix@current[$i] = ToCoarser ( LaplaceCoeff$postfix@finer[$i] )")
        } else {
          printer.println(s"\t\tLaplace$postfix@current = ( CorrectionStencil@current * ( ToCoarser ( Laplace$postfix@finer ) * RestrictionStencil@current ) )")
        }
        printer.println(s"\t}")
        printer.println(s"}")
      }
    }

    printer.println
  }
}