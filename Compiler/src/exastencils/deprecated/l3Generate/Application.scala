package exastencils.deprecated.l3Generate

import scala.collection.mutable.ListBuffer

import exastencils.config._

object Application {
  def addFunction(printer : java.io.PrintStream) = {
    printer.println("Function Application ( ) : Unit {")

    if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
      printer.println("\tstartTimer ( 'setup' )")

    printer.println("\tinitGlobals ( )")
    printer.println("\tinitDomain ( )")
    printer.println("\tinitFieldsWithZero ( )")
    if (!Knowledge.grid_isUniform) printer.println("\tinitGeometry ( )")

    if (!Knowledge.l3tmp_genForAutoTests || Knowledge.l3tmp_printTimersToFile)
      printer.println("\tstopTimer ( 'setup' )")
    if (!Knowledge.l3tmp_genForAutoTests)
      printer.println("\tprint ( 'Total time to setup: ', getTotalFromTimer ( 'setup' ) )")

    if (Knowledge.l3tmp_genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 =>
          printer.println("\tLaplace_Coeff_0_0 = -4")
          printer.println("\tLaplace_Coeff_P1_0 = 1")
          printer.println("\tLaplace_Coeff_N1_0 = 1")
          printer.println("\tLaplace_Coeff_0_P1 = 1")
          printer.println("\tLaplace_Coeff_0_N1 = 1")

        case 3 =>
          printer.println("\tLaplace_Coeff_0_0_0 = -6")
          printer.println("\tLaplace_Coeff_P1_0_0 = 1")
          printer.println("\tLaplace_Coeff_N1_0_0 = 1")
          printer.println("\tLaplace_Coeff_0_P1_0 = 1")
          printer.println("\tLaplace_Coeff_0_N1_0 = 1")
          printer.println("\tLaplace_Coeff_0_0_P1 = 1")
          printer.println("\tLaplace_Coeff_0_0_N1 = 1")
      }
    }

    if (Knowledge.l3tmp_kelvin && !Knowledge.l3tmp_genForAutoTests) {
      printer.println("\tVariable timeSamples : Real = 0")
      printer.println("\tstartTimer ( 'timePerSample' )")
      printer.println(s"\trepeat ${ Knowledge.l3tmp_kelvin_numSamples } times {")
    }

    if (Knowledge.l3tmp_kelvin) {
      if (Knowledge.l3tmp_genStencilFields) {
        for (lvl <- Knowledge.maxLevel to Knowledge.minLevel by -1)
          printer.println(s"\tInitLaplace_GMRF@$lvl ( )")
      }
      printer.println("\tInitRHS_GMRF ( )")
      printer.println("\tInitSolution_GMRF ( )")
      printer.println("\tSolve_GMRF ( )")
    }

    if (Knowledge.l3tmp_kelvin) {
      // setup stencils for the actual PDE
      printer.println("\tloop over LaplaceCoeff@finest {")
      printer.println("\t\tLaplaceCoeff@finest[0] = TransferStencil_Center@finest * Solution_GMRF@finest")
      printer.println("\t\tLaplaceCoeff@finest[1] = TransferStencil_Right@finest * Solution_GMRF@finest")
      printer.println("\t\tLaplaceCoeff@finest[2] = TransferStencil_Left@finest * Solution_GMRF@finest")
      printer.println("\t\tLaplaceCoeff@finest[3] = TransferStencil_Up@finest * Solution_GMRF@finest")
      printer.println("\t\tLaplaceCoeff@finest[4] = TransferStencil_Down@finest * Solution_GMRF@finest")
      printer.println("\t}")

      if (Knowledge.l3tmp_genStencilFields) {
        for (lvl <- Knowledge.maxLevel - 1 to Knowledge.minLevel by -1)
          printer.println(s"\tInitLaplace@$lvl ( )")
      }
    } else {
      if (Knowledge.l3tmp_genStencilFields) {
        for (lvl <- Knowledge.maxLevel to Knowledge.minLevel by -1)
          printer.println(s"\tInitLaplace@$lvl ( )")
      }
    }

    printer.println("\tInitRHS@finest ( )")
    printer.println("\tInitSolution ( )")

    if (!Knowledge.l3tmp_genCellBasedDiscr) {
      // TODO: add other fields here if bc handling is required
      for (lvl <- Knowledge.minLevel to Knowledge.maxLevel) {
        if ("Jac" == Knowledge.l3tmp_smoother) {
          if (Knowledge.l3tmp_useSlotsForJac) {
            Communication.applyBCs(printer, s"Solution[0]@$lvl")
            Communication.applyBCs(printer, s"Solution[1]@$lvl")
          } else {
            Communication.applyBCs(printer, s"Solution@$lvl")
            Communication.applyBCs(printer, s"Solution2@$lvl")
          }
        } else {
          Communication.applyBCs(printer, s"Solution@$lvl")
        }
      }
      Communication.applyBCs(printer, s"VecP@coarsest")
    }

    printer.println("\tSolve ( )")

    if (Knowledge.l3tmp_kelvin) {
      printer.println(s"\tloop over SolutionMean@finest {")
      printer.println(s"\t\tSolutionMean@finest += ${ Fields.solution(s"finest", "")(0) }")
      printer.println(s"\t}")
    }

    if (Knowledge.l3tmp_kelvin && !Knowledge.l3tmp_genForAutoTests) {
      printer.println("\t}")
      printer.println("\tstopTimer ( 'timePerSample' )")
      printer.println("\tprint ( 'Total time to solve: ', getTotalFromTimer ( 'timePerSample' ) )")
      printer.println("\tprint ( 'Mean time per sample: ', getMeanFromTimer ( 'timePerSample' ) )")
    }

    if (Knowledge.l3tmp_kelvin) {
      printer.println(s"\tloop over SolutionMean@finest {")
      printer.println(s"\t\tSolutionMean@finest /= ${ Knowledge.l3tmp_kelvin_numSamples }")
      printer.println(s"\t}")

      printer.println(s"\tcommunicate SolutionMean@finest")
      printer.println(s"\tVariable solNorm : Real = 0.0")
      if (Knowledge.l3tmp_genCellBasedDiscr)
        printer.println(s"\tloop over SolutionMean@finest with reduction( + : solNorm ) {")
      else
        printer.println(s"\tloop over SolutionMean@finest where x > 0 && y > 0 ${ if (Knowledge.dimensionality > 2) "&& z > 0 " else "" }with reduction( + : solNorm ) {")
      printer.println(s"\t\tsolNorm += SolutionMean@finest * SolutionMean@finest")
      printer.println(s"\t}")
      printer.println(s"\tsolNorm = ( sqrt ( solNorm ) ) / ${ (Knowledge.domain_rect_numFragsTotal_x - 2 * Knowledge.l3tmp_kelvin_numHaloFrags) * (1 << Knowledge.maxLevel) - 1 }")
      printer.println("\tprint ( 'Norm of the solution: ', solNorm )")
    }

    if (Knowledge.l3tmp_printFieldAtEnd) {
      if (Knowledge.l3tmp_kelvin)
        printer.println("\tprintField ( 'Solution.dat', SolutionMean@finest )")
      else
        printer.println("\tprintField ( 'Solution.dat', Solution@finest )")
    }

    if (!Knowledge.l3tmp_genForAutoTests) {
      //printer.println("\tprintAllTimers ( )")

      if (Knowledge.l3tmp_genTimersPerFunction) {
        for (
          func <- Array(
            ("preSmoothing", "pre-smoothing"),
            ("residualUpdate", "updating residual"),
            ("restriction", "restricting"),
            ("settingSolution", "setting solution"),
            ("correction", "prolongating and correcting"),
            ("postSmoothing", "post-smoothing"))
        ) {
          if (Knowledge.l3tmp_genTimersPerLevel) {
            for (level <- Knowledge.minLevel to Knowledge.maxLevel)
              printer.println("\tprint ( '" + s"Total time spent on level $level in ${ func._2 }: " + "', " + s"getTotalFromTimer ( concat ( '${ func._1 }_', $level ) ) )")
          } else {
            printer.println("\tprint ( '" + s"Total time spent in ${ func._2 }: " + "', " + s"getTotalFromTimer ( '${ func._1 }' ) )")
          }
        }
      }
      if (Knowledge.l3tmp_genTimersForComm) {
        var timers = ListBuffer[String]()
        for (commTimer <- Communication.commTimerNames) {
          if (Knowledge.l3tmp_genCommTimersPerLevel)
            for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
              val timer = s"getTotalFromTimer ( concat ( '${ commTimer._1 }_', $level ) )"
              printer.println("\tprint ( '" + s"Total time spent communicating${ commTimer._2 } on level $level: ', $timer )")
              timers += timer
            }
          else {
            val timer = s"getTotalFromTimer ( '${ commTimer._1 }' )"
            printer.println("\tprint ( '" + s"Total time spent communicating${ commTimer._2 }: ', $timer )")
            timers += timer
          }
        }
        if (timers.size > 1) {
          printer.println("\tprint ( '" + s"Total time spent communicating: ', ${ timers.mkString(" + ") } )")
        }
      }
    }

    if (Knowledge.l3tmp_printTimersToFile) {
      if (Knowledge.l3tmp_sisc) {
        printer.println(s"\tif (getTotalFromTimer ( 'cycle' ) <= ${ Knowledge.l3tmp_timeoutLimit } ) {")
        printer.println("\t\tprintAllTimersToFile ( )")
        printer.println("\t}")
      } else {
        printer.println("\tprintAllTimersToFile ( )")
      }
    }
    if (Knowledge.experimental_timerEnableCallStacks) {
      printer.println("\tnative ( 'CallTracker::PrintCallStackToFileGlobal(callstack.cs)' )")
      printer.println("\tnative ( 'CallTracker::ClearCallStack()' )")
    }

    printer.println("\tdestroyGlobals ( )")

    printer.println("}")
    printer.println()
  }
}
