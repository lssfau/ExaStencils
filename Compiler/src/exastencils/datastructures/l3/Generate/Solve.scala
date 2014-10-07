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
      printer.println("\tprint ( '\"" + s"Starting residual at $vecDim" + "\"', " + s"resStart_$vecDim )")
    }
    printer.println("\tVariable totalTime : Real = 0")
    printer.println("\tVariable timeToSolve : Real = 0")
    printer.println("\tstartTimer ( timeToSolveWatch )")
    printer.println("\tVariable numIt : Integer = 0")
    printer.println("\trepeat until res_0 < 1.0e-8 {")
    printer.println("\t\tnumIt += 1")
    printer.println("\t\tstartTimer ( stopWatch )")
    printer.println("\t\tVCycle@finest (  )")
    if (Knowledge.l3tmp_genAsyncCommunication)
      printer.println("\t\tUpResidual@finest ( 0 )")
    else
      printer.println("\t\tUpResidual@finest ( )")
    if (Knowledge.l3tmp_genAdvancedTimers) {
      printer.println("\tstopTimer ( stopWatch )")
      printer.println("\taddFromTimer ( stopWatch, totalTime )")
    } else {
      printer.println("\t\tstopTimer ( stopWatch, totalTime )")
    }
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims) {
      printer.println(s"\t\tresOld_$vecDim = res_$vecDim")
      printer.println(s"\t\tres_$vecDim = NormResidual_$vecDim@finest (  )")
      printer.println("\t\tprint ( '\"" + s"Residual at $vecDim:" + "\"', " + s"res_$vecDim" + ", '\"Residual reduction:\"', " + s"( resStart_$vecDim / res_$vecDim ), " + "'\"Convergence factor:\"', " + s"( res_$vecDim / resOld_$vecDim ) )")
    }
    printer.println("\t}")
    if (Knowledge.l3tmp_genAdvancedTimers) {
      printer.println("\tstopTimer ( timeToSolveWatch )")
      printer.println("\taddFromTimer ( timeToSolveWatch, timeToSolve )")
    } else {
      printer.println("\tstopTimer ( timeToSolveWatch, timeToSolve )")
    }
    printer.println("\tprint ( '\"Total time to solve in\"', numIt, '\"steps :\"', timeToSolve )")
    printer.println("\tprint ( '\"Mean time per vCycle: \"', totalTime / numIt )")
    printer.println(s"}")
    printer.println

    if (Knowledge.l3tmp_kelvin) {
      printer.println("Function Solve_GMRF ( ) : Unit {")
      printer.println("\tnative ( \"static int sample = 0\" )")
      printer.println("\tnative ( \"std::default_random_engine generator(mpiRank + sample++)\" )")
      printer.println("\tnative ( \"std::normal_distribution<double> distribution(0.0, 1.0)\" )")
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
      printer.println("\tprint ( '\"Starting residual:\"', resStart )")
      printer.println("\tVariable totalTime : Real = 0")
      printer.println("\tVariable timeToSolve : Real = 0")
      printer.println("\tstartTimer ( timeToSolveWatch )")
      printer.println("\tVariable numIt : Integer = 0")
      printer.println("\trepeat until res < 1.0e-8 {")
      printer.println("\t\tnumIt += 1")
      printer.println("\t\tstartTimer ( stopWatch )")
      printer.println("\t\tVCycle_GMRF@finest (  )")
      if (Knowledge.l3tmp_genAsyncCommunication)
        printer.println(s"\tUpResidual_GMRF@finest ( 0 )")
      else
        printer.println(s"\tUpResidual_GMRF@finest ( )")
      if (Knowledge.l3tmp_genAdvancedTimers) {
        printer.println("\t\tstopTimer ( stopWatch )")
        printer.println("\t\taddFromTimer ( stopWatch, totalTime )")
      } else {
        printer.println("\t\tstopTimer ( stopWatch, totalTime )")
      }
      printer.println(s"\t\tresOld = res")
      printer.println(s"\t\tres = NormResidual_GMRF_0@finest ( )")
      printer.println("\t\tprint ( '\"Residual:\"', res, '\"Residual reduction:\"', ( resStart / res ), '\"Convergence factor:\"', ( res / resOld ) )")
      printer.println("\t}")
      if (Knowledge.l3tmp_genAdvancedTimers) {
        printer.println("\tstopTimer ( timeToSolveWatch )")
        printer.println("\taddFromTimer ( timeToSolveWatch, timeToSolve )")
      } else {
        printer.println("\tstopTimer ( timeToSolveWatch, timeToSolve )")
      }
      printer.println("\tprint ( '\"Total time to solve in\"', numIt, '\"steps :\"', timeToSolve )")
      printer.println("\tprint ( '\"Mean time per vCycle: \"', totalTime / numIt )")

      printer.println(s"\tloop over Solution_GMRF@finest {")
      printer.println(s"\t\tSolution_GMRF@finest = exp ( Solution_GMRF@finest / sqrt ( tau2 ) )")
      printer.println(s"\t}")
      printer.println(s"}")
      printer.println
    }
  }
}