package exastencils.datastructures.l3

import exastencils.knowledge._

object Solve {
  def addFunction(printer : java.io.PrintWriter) = {
    printer.println("def Solve ( ) : Unit {")
    if (Knowledge.testCommCompOverlap)
      printer.println("\tUpResidual@finest ( ( 0 ) )")
    else
      printer.println("\tUpResidual@finest ( )")
    for (vecDim <- 0 until Knowledge.numVecDims) {
      printer.println(s"\tvar resStart_$vecDim : Real = L2Residual_$vecDim@finest (  )")
      printer.println(s"\tvar res_$vecDim : Real = resStart_$vecDim")
      printer.println(s"\tvar resOld_$vecDim : Real = 0")
      printer.println("\tprint ( '\"" + s"Starting residual at $vecDim" + "\"', " + s"resStart_$vecDim )")
    }
    printer.println("\tvar totalTime : Real = 0")
    printer.println("\tvar timeToSolve : Real = 0")
    printer.println("\tstartTimer ( timeToSolveWatch )")
    printer.println("\tvar numIt : Integer = 0")
    printer.println("\trepeat until res_0 < 1.0e-8 {")
    printer.println("\t\tnumIt += 1")
    printer.println("\t\tstartTimer ( stopWatch )")
    printer.println("\t\tVCycle@finest (  )")
    if (Knowledge.testCommCompOverlap)
      printer.println("\t\tUpResidual@finest ( 0 )")
    else
      printer.println("\t\tUpResidual@finest ( )")
    if (Knowledge.testNewTimers) {
      printer.println("\tstopTimer ( stopWatch )")
      printer.println("\taddFromTimer ( stopWatch, totalTime )")
    } else {
      printer.println("\t\tstopTimer ( stopWatch, totalTime )")
    }
    for (vecDim <- 0 until Knowledge.numVecDims) {
      printer.println(s"\t\tresOld_$vecDim = res_$vecDim")
      printer.println(s"\t\tres_$vecDim = L2Residual_$vecDim@finest (  )")
      printer.println("\t\tprint ( '\"" + s"Residual at $vecDim:" + "\"', " + s"res_$vecDim" + ", '\"Residual reduction:\"', " + s"( resStart_$vecDim / res_$vecDim ), " + "'\"Convergence factor:\"', " + s"( res_$vecDim / resOld_$vecDim ) )")
    }
    printer.println("\t}")
    if (Knowledge.testNewTimers) {
      printer.println("\tstopTimer ( timeToSolveWatch )")
      printer.println("\taddFromTimer ( timeToSolveWatch, timeToSolve )")
    } else {
      printer.println("\tstopTimer ( timeToSolveWatch, timeToSolve )")
    }
    printer.println("\tprint ( '\"Total time to solve in\"', numIt, '\"steps :\"', timeToSolve )")
    printer.println("\tprint ( '\"Mean time per vCycle: \"', totalTime / numIt )")
    printer.println(s"}")
    printer.println

    if (Knowledge.kelvin) {
      printer.println("def Solve_GMRF ( ) : Unit {")
      printer.println("\tnative ( \"static int sample = 0\" )")
      printer.println("\tnative ( \"std::default_random_engine generator(mpiRank + sample++)\" )")
      printer.println("\tnative ( \"std::normal_distribution<double> distribution(0.0, 1.0)\" )")
      printer.println("\tnative ( \"auto randn = std::bind ( distribution, generator )\" )")

      printer.println(s"\tvar tau2 : Real = myGamma ( nu ) / ( myGamma ( nu + 0.5 ) * (( 4.0 * M_PI ) ** ( dim / 2.0 )) * ( kappa ** ( 2 * nu )) * sigma * sigma )")
      printer.println(s"\tloop over RHS_GMRF@finest {")
      printer.println(s"\t\tRHS_GMRF@finest = randn ( ) / ${(Knowledge.domain_numFragsTotal_x - 2 * Knowledge.numHaloFrags) * (1 << Knowledge.maxLevel)}")
      printer.println(s"\t}")

      Communication.exch(printer, s"RHS_GMRF@finest")

      if (Knowledge.testCommCompOverlap)
        printer.println(s"\tUpResidual_GMRF@finest ( 0 )")
      else
        printer.println(s"\tUpResidual_GMRF@finest ( )")
      printer.println(s"\tvar resStart : Real = L2Residual_GMRF_0@finest ( )")
      printer.println(s"\tvar res : Real = resStart")
      printer.println(s"\tvar resOld : Real = 0")
      printer.println("\tprint ( '\"Starting residual:\"', resStart )")
      printer.println("\tvar totalTime : Real = 0")
      printer.println("\tvar timeToSolve : Real = 0")
      printer.println("\tstartTimer ( timeToSolveWatch )")
      printer.println("\tvar numIt : Integer = 0")
      printer.println("\trepeat until res < 1.0e-8 {")
      printer.println("\t\tnumIt += 1")
      printer.println("\t\tstartTimer ( stopWatch )")
      printer.println("\t\tVCycle_GMRF@finest (  )")
      if (Knowledge.testCommCompOverlap)
        printer.println(s"\tUpResidual_GMRF@finest ( 0 )")
      else
        printer.println(s"\tUpResidual_GMRF@finest ( )")
      if (Knowledge.testNewTimers) {
        printer.println("\t\tstopTimer ( stopWatch )")
        printer.println("\t\taddFromTimer ( stopWatch, totalTime )")
      } else {
        printer.println("\t\tstopTimer ( stopWatch, totalTime )")
      }
      printer.println(s"\t\tresOld = res")
      printer.println(s"\t\tres = L2Residual_GMRF_0@finest ( )")
      printer.println("\t\tprint ( '\"Residual:\"', res, '\"Residual reduction:\"', ( resStart / res ), '\"Convergence factor:\"', ( res / resOld ) )")
      printer.println("\t}")
      if (Knowledge.testNewTimers) {
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