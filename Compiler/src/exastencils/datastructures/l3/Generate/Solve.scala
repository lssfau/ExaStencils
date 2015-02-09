package exastencils.datastructures.l3

import exastencils.knowledge._

object Solve {
  def addFunction(printer : java.io.PrintWriter) = {
    printer.println("Function Solve ( ) : Unit {")
    if (Knowledge.l3tmp_genAsyncCommunication)
      printer.println("\tUpResidual@finest ( ( 0 ) )")
    else
      printer.println("\tUpResidual@finest ( )")
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims) {
      printer.println(s"\tVariable resStart_$vecDim : Real = NormResidual_$vecDim@finest (  )")
      printer.println(s"\tVariable res_$vecDim : Real = resStart_$vecDim")
      printer.println(s"\tVariable resOld_$vecDim : Real = 0")
      if (Knowledge.l3tmp_genForAutoTests)
        printer.println(s"\tprint ( resStart_$vecDim )")
      else
        printer.println("\tprint ( '\"" + s"Starting residual at $vecDim" + "\"', " + s"resStart_$vecDim )")
    }
    if (!Knowledge.l3tmp_genForAutoTests) {
      if (!Knowledge.l3tmp_genAdvancedTimers) {
        printer.println("\tVariable totalTime : Real = 0")
        printer.println("\tVariable timeToSolve : Real = 0")
      }
      printer.println("\tstartTimer ( timeToSolveWatch )")
    }
    printer.println("\tVariable numIt : Integer = 0")
    printer.println(s"\trepeat until res_0 < ( ${if (Knowledge.useDblPrecision) "1.0e-10" else "1.0e-4"} * resStart_0 ) {")
    printer.println("\t\tnumIt += 1")
    if (!Knowledge.l3tmp_genForAutoTests)
      printer.println("\t\tstartTimer ( stopWatch )")
    printer.println("\t\tVCycle@finest (  )")

    if (Knowledge.experimental_NeumannNormalize) {
      printer.println(s"\t\tVar integral : Real = 0.0")
      printer.println(s"\t\tloop over Solution[currentSlot]@current where x > 0 && y > 0 with reduction( + : integral ) {")
      printer.println(s"\t\t\tintegral += Solution[currentSlot]@current")
      printer.println(s"\t\t}")
      printer.println(s"\t\tintegral /= ${(0 until Knowledge.dimensionality).map(dim => Knowledge.domain_numFragsTotalPerDim(dim) * (1 << Knowledge.maxLevel) - 1).reduce((a, b) => a * b)}.0")
      printer.println(s"\t\tloop over Solution[currentSlot]@current {")
      printer.println(s"\t\t\tSolution[currentSlot]@current -= integral")
      printer.println(s"\t\t}")
      printer.println(s"\t\tapply bc to Solution[currentSlot]@current")
    }

    if (Knowledge.l3tmp_genAsyncCommunication)
      printer.println("\t\tUpResidual@finest ( 0 )")
    else
      printer.println("\t\tUpResidual@finest ( )")
    if (!Knowledge.l3tmp_genForAutoTests) {
      if (Knowledge.l3tmp_genAdvancedTimers)
        printer.println("\tstopTimer ( stopWatch )")
      else
        printer.println("\t\tstopTimer ( stopWatch, totalTime )")
    }
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims) {
      printer.println(s"\t\tresOld_$vecDim = res_$vecDim")
      printer.println(s"\t\tres_$vecDim = NormResidual_$vecDim@finest (  )")
      if (Knowledge.l3tmp_genForAutoTests) {
        if (Knowledge.l3tmp_printError) {
          printer.println(s"\t\tVar curError_$vecDim : Real = NormError_$vecDim@finest ( )")
          printer.println(s"\t\tprint ( curError_$vecDim )")
        } else {
          printer.println(s"\t\tprint ( res_$vecDim )")
        }
      } else {
        printer.println("\t\tprint ( '\"" + s"Residual at $vecDim:" + "\"', " + s"res_$vecDim" + ", '\"Residual reduction:\"', " + s"( resStart_$vecDim / res_$vecDim ), " + "'\"Convergence factor:\"', " + s"( res_$vecDim / resOld_$vecDim ) )")
        if (Knowledge.l3tmp_printError) {
          printer.println(s"\t\tVar curError_$vecDim : Real = NormError_$vecDim@finest ( )")
          printer.println("\t\tprint ( '\"" + s"Error at $vecDim:" + "\"', " + s"curError_$vecDim )")
        }
      }
    }
    printer.println("\t}")

    if (Knowledge.l3tmp_genForAutoTests)
      printer.println(s"\tprint ( numIt )")

    if (!Knowledge.l3tmp_genForAutoTests) {
      if (Knowledge.l3tmp_genAdvancedTimers) {
        printer.println("\tstopTimer ( timeToSolveWatch )")
        printer.println("\tprint ( '\"Total time to solve in\"', numIt, '\"steps :\"', getTotalFromTimer ( timeToSolveWatch ) )")
        printer.println("\tprint ( '\"Mean time per vCycle: \"', getMeanFromTimer ( stopWatch ) )")
      } else {
        printer.println("\tstopTimer ( timeToSolveWatch, timeToSolve )")
        printer.println("\tprint ( '\"Total time to solve in\"', numIt, '\"steps :\"', timeToSolve )")
        printer.println("\tprint ( '\"Mean time per vCycle: \"', totalTime / numIt )")
      }
    }
    printer.println(s"}")
    printer.println

    if (Knowledge.l3tmp_kelvin) {
      printer.println("Function Solve_GMRF ( ) : Unit {")
      printer.println("\tnative ( \"static int sample = 0\" )")
      printer.println("\tnative ( \"std::default_random_engine generator(mpiRank + sample++)\" )")
      printer.println("\tnative ( \"std::normal_distribution<" + (if (Knowledge.useDblPrecision) "double" else "float") + "> distribution(0.0, 1.0)\" )")
      printer.println("\tnative ( \"auto randn = std::bind ( distribution, generator )\" )")

      printer.println(s"\tVariable tau2 : Real = myGamma ( nu ) / ( myGamma ( nu + 0.5 ) * (( 4.0 * M_PI ) ** ( dim / 2.0 )) * ( kappa ** ( 2 * nu )) * sigma * sigma )")
      printer.println(s"\tloop over RHS_GMRF@finest {")
      printer.println(s"\t\tRHS_GMRF@finest = randn ( ) / ${(Knowledge.domain_numFragsTotal_x - 2 * Knowledge.l3tmp_kelvin_numHaloFrags) * (1 << Knowledge.maxLevel)}")
      printer.println(s"\t}")

      Communication.exch(printer, s"RHS_GMRF@finest")

      if (Knowledge.l3tmp_genAsyncCommunication)
        printer.println(s"\tUpResidual_GMRF@finest ( 0 )")
      else
        printer.println(s"\tUpResidual_GMRF@finest ( )")
      printer.println(s"\tVariable resStart : Real = NormResidual_GMRF_0@finest ( )")
      printer.println(s"\tVariable res : Real = resStart")
      printer.println(s"\tVariable resOld : Real = 0")
      if (Knowledge.l3tmp_genForAutoTests)
        printer.println(s"\tprint ( resStart )")
      else
        printer.println("\tprint ( '\"Starting residual:\"', resStart )")
      if (!Knowledge.l3tmp_genForAutoTests) {
        if (!Knowledge.l3tmp_genAdvancedTimers) {
          printer.println("\tVariable totalTime : Real = 0")
          printer.println("\tVariable timeToSolve : Real = 0")
        }
        printer.println("\tstartTimer ( timeToSolveWatch )")
      }
      printer.println("\tVariable numIt : Integer = 0")
      printer.println("\trepeat until res < 1.0e-8 {")
      printer.println("\t\tnumIt += 1")
      if (!Knowledge.l3tmp_genForAutoTests)
        printer.println("\t\tstartTimer ( stopWatch )")
      printer.println("\t\tVCycle_GMRF@finest (  )")
      if (Knowledge.l3tmp_genAsyncCommunication)
        printer.println(s"\tUpResidual_GMRF@finest ( 0 )")
      else
        printer.println(s"\tUpResidual_GMRF@finest ( )")
      if (!Knowledge.l3tmp_genForAutoTests) {
        if (Knowledge.l3tmp_genAdvancedTimers)
          printer.println("\t\tstopTimer ( stopWatch )")
        else
          printer.println("\t\tstopTimer ( stopWatch, totalTime )")
      }
      printer.println(s"\t\tresOld = res")
      printer.println(s"\t\tres = NormResidual_GMRF_0@finest ( )")
      if (Knowledge.l3tmp_genForAutoTests)
        printer.println("\t\tprint ( res )")
      else
        printer.println("\t\tprint ( '\"Residual:\"', res, '\"Residual reduction:\"', ( resStart / res ), '\"Convergence factor:\"', ( res / resOld ) )")
      printer.println("\t}")
      if (Knowledge.l3tmp_genForAutoTests) {
        printer.println("\tprint ( numIt )")
      } else {
        if (Knowledge.l3tmp_genAdvancedTimers) {
          printer.println("\tstopTimer ( timeToSolveWatch )")
          printer.println("\ttimeToSolve = getTotalFromTimer ( timeToSolveWatch )")
          printer.println("\ttotalTime = getMeanFromTimer ( stopWatch )")
        } else {
          printer.println("\tstopTimer ( timeToSolveWatch, timeToSolve )")
          printer.println("\ttotalTime /= numIt")
        }
        printer.println("\tprint ( '\"Total time to solve in\"', numIt, '\"steps :\"', timeToSolve )")
        printer.println("\tprint ( '\"Mean time per vCycle: \"', totalTime )")
      }

      printer.println(s"\tloop over Solution_GMRF@finest {")
      printer.println(s"\t\tSolution_GMRF@finest = exp ( Solution_GMRF@finest / sqrt ( tau2 ) )")
      printer.println(s"\t}")
      printer.println(s"}")
      printer.println
    }
  }
}