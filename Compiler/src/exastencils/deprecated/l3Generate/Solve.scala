package exastencils.deprecated.l3Generate

import exastencils.config._

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
        Util.addPrintAutoTestValueCode(printer, s"resStart_$vecDim")
      else
        printer.println("\tprint ( '" + s"Starting residual at $vecDim" + "', " + s"resStart_$vecDim )")
    }
    if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
      printer.println("\tstartTimer ( 'timeToSolve' )")

    if (Knowledge.l3tmp_genFMG) {
      if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
        printer.println("\tstartTimer ( 'fmg' )")
      printer.println(s"\tFMG@${ Knowledge.minLevel } ( )")
      if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
        printer.println("\tstopTimer ( 'fmg' )")
    }

    printer.println("\tVariable numIt : Integer = 0")
    printer.println(s"\trepeat until (res_0 < ( ${ Knowledge.l3tmp_targetResReduction } * resStart_0 ) || numIt >= 100) {")
    printer.println("\t\tnumIt += 1")
    if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile || Knowledge.l3tmp_timeoutLimit > 0)
      printer.println("\t\tstartTimer ( 'cycle' )")
    printer.println("\t\tVCycle@finest (  )")

    if (Knowledge.experimental_NeumannNormalize) {
      printer.println(s"\t\tVar integral : Real = 0.0")
      if (Knowledge.l3tmp_genCellBasedDiscr)
        printer.println(s"\t\tloop over Solution[currentSlot]@current with reduction( + : integral ) {")
      else
        printer.println(s"\t\tloop over Solution[currentSlot]@current where x > 0 && y > 0 with reduction( + : integral ) {")
      printer.println(s"\t\t\tintegral += Solution[currentSlot]@current")
      printer.println(s"\t\t}")
      val numPoints : Double = Knowledge.dimensions.map(dim =>
        Knowledge.domain_rect_numFragsTotalAsVec(dim) * Knowledge.domain_fragmentLengthAsVec(dim) * (1 << Knowledge.maxLevel) + (if (Knowledge.l3tmp_genCellBasedDiscr) 0 else -1))
        .reduce((a, b) => a * b)
      printer.println(s"\t\tintegral /= $numPoints")
      printer.println(s"\t\tloop over Solution[currentSlot]@current {")
      printer.println(s"\t\t\tSolution[currentSlot]@current -= integral")
      printer.println(s"\t\t}")
    }
    if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile || Knowledge.l3tmp_timeoutLimit > 0)
      printer.println("\t\tstopTimer ( 'cycle' )")

    if (Knowledge.l3tmp_timeoutLimit > 0) {
      printer.println(s"\t\tif (getTotalFromTimer ( 'cycle' ) > ${ Knowledge.l3tmp_timeoutLimit } ) {")
      printer.println("\t\t\tprint ( 'Aborting solve after', getTotalFromTimer ( 'cycle' ), 'ms which exceeds the limit of', " + Knowledge.l3tmp_timeoutLimit + ", 'ms' )")
      printer.println("\t\t\treturn")
      printer.println(s"\t\t}")
    }

    if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
      printer.println("\t\tstartTimer ( 'convergenceChecking' )")
    if (Knowledge.l3tmp_genAsyncCommunication)
      printer.println("\t\tUpResidual@finest ( 0 )")
    else
      printer.println("\t\tUpResidual@finest ( )")
    for (vecDim <- 0 until Knowledge.l3tmp_numVecDims) {
      printer.println(s"\t\tresOld_$vecDim = res_$vecDim")
      printer.println(s"\t\tres_$vecDim = NormResidual_$vecDim@finest (  )")
      if (Knowledge.l3tmp_genForAutoTests) {
        if (Knowledge.l3tmp_printError)
          printer.println(s"\t\tVar curError_$vecDim : Real = NormError_$vecDim@finest ( )")
        Util.addPrintAutoTestValueCode(printer, if (Knowledge.l3tmp_printError) s"curError_$vecDim" else s"res_$vecDim")
      } else {
        printer.println("\t\tprint ( '" + s"Residual at $vecDim:" + "', " + s"res_$vecDim" + ", 'Residual reduction:', " + s"( resStart_$vecDim / res_$vecDim ), " + "'Convergence factor:', " + s"( res_$vecDim / resOld_$vecDim ) )")
        if (Knowledge.l3tmp_printError) {
          printer.println(s"\t\tVar curError_$vecDim : Real = NormError_$vecDim@finest ( )")
          printer.println("\t\tprint ( '" + s"Error at $vecDim:" + "', " + s"curError_$vecDim )")
        }
      }
    }
    if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
      printer.println("\tstopTimer ( 'convergenceChecking' )")
    printer.println("\t}")

    if (Knowledge.l3tmp_genForAutoTests)
      printer.println(s"\tprint ( numIt )")

    if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
      printer.println("\tstopTimer ( 'timeToSolve' )")
    if (!Knowledge.l3tmp_genForAutoTests) {
      printer.println("\tprint ( 'Total time to solve in', numIt, 'steps :', getTotalFromTimer ( 'timeToSolve' ) )")
      printer.println("\tprint ( 'Mean time per vCycle: ', getMeanFromTimer ( 'cycle' ) )")
    }
    printer.println(s"}")
    printer.println

    if (Knowledge.l3tmp_kelvin) {
      printer.println("Function Solve_GMRF ( ) : Unit {")
      printer.println("\tnative ( static int sample = 0 )")
      printer.println("\tnative ( std::default_random_engine generator(mpiRank + sample++) )")
      printer.println("\tnative ( std::normal_distribution<" + (if (Knowledge.useDblPrecision) "double" else "float") + "> distribution(0.0, 1.0) )")
      printer.println("\tnative ( auto randn = std::bind ( distribution, generator ) )")

      printer.println(s"\tVariable tau2 : Real = myGamma ( nu ) / ( myGamma ( nu + 0.5 ) * (( 4.0 * PI ) ** ( dim / 2.0 )) * ( kappa ** ( 2 * nu )) * sigma * sigma )")
      printer.println(s"\tloop over RHS_GMRF@finest {")
      printer.println(s"\t\tRHS_GMRF@finest = randn ( ) / ${ (Knowledge.domain_rect_numFragsTotal_x - 2 * Knowledge.l3tmp_kelvin_numHaloFrags) * (1 << Knowledge.maxLevel) }")
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
        printer.println("\tprint ( 'Starting residual:', resStart )")
      if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
        printer.println("\tstartTimer ( 'timeToSolve' )")
      printer.println("\tVariable numIt : Integer = 0")
      printer.println("\trepeat until res < 1.0e-8 {")
      printer.println("\t\tnumIt += 1")
      if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
        printer.println("\t\tstartTimer ( 'cycle' )")
      printer.println("\t\tVCycle_GMRF@finest (  )")
      if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
        printer.println("\t\tstopTimer ( 'cycle' )")

      if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
        printer.println("\tstartTimer ( 'convergenceChecking' )")

      if (Knowledge.l3tmp_genAsyncCommunication)
        printer.println(s"\tUpResidual_GMRF@finest ( 0 )")
      else
        printer.println(s"\tUpResidual_GMRF@finest ( )")
      printer.println(s"\t\tresOld = res")
      printer.println(s"\t\tres = NormResidual_GMRF_0@finest ( )")
      if (Knowledge.l3tmp_genForAutoTests)
        Util.addPrintAutoTestValueCode(printer, "res")
      else
        printer.println("\t\tprint ( 'Residual:', res, 'Residual reduction:', ( resStart / res ), 'Convergence factor:', ( res / resOld ) )")

      if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
        printer.println("\tstopTimer ( 'convergenceChecking' )")

      printer.println("\t}")
      if (Knowledge.l3tmp_genForAutoTests)
        printer.println("\tprint ( numIt )")

      if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
        printer.println("\tstopTimer ( 'timeToSolve' )")
      if (!Knowledge.l3tmp_genForAutoTests) {
        printer.println("\tprint ( 'Total time to solve in', numIt, 'steps :', getTotalFromTimer ( 'timeToSolve' ) )")
        printer.println("\tprint ( 'Mean time per vCycle: ', getMeanFromTimer ( 'cycle' ) )")
      }

      printer.println(s"\tloop over Solution_GMRF@finest {")
      printer.println(s"\t\tSolution_GMRF@finest = exp ( Solution_GMRF@finest / sqrt ( tau2 ) )")
      printer.println(s"\t}")
      printer.println(s"}")
      printer.println
    }
  }
}
