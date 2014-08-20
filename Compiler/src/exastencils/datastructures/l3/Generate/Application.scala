package exastencils.datastructures.l3

import exastencils.knowledge._

object Application {
  def addFunction(printer : java.io.PrintWriter) = {
    printer.println("Function Application ( ) : Unit {")

    printer.println("\tVariable setupTime : Real = 0")
    printer.println("\tstartTimer ( setupWatch )")

    printer.println("\tinitGlobals ( )")
    printer.println("\tinitDomain ( )")
    printer.println("\tinitFieldsWithZero ( )")

    if (Knowledge.testNewTimers) {
      printer.println("\tstopTimer ( setupWatch )")
      printer.println("\taddFromTimer ( setupWatch, setupTime )")
    } else {
      printer.println("\tstopTimer ( setupWatch, setupTime )")
    }
    printer.println("\tprint ( '\"Total time to setup: \"', setupTime )")

    if (Knowledge.genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("\tLaplace_Coeff_0_0 = -4")
          printer.println("\tLaplace_Coeff_P1_0 = 1")
          printer.println("\tLaplace_Coeff_N1_0 = 1")
          printer.println("\tLaplace_Coeff_0_P1 = 1")
          printer.println("\tLaplace_Coeff_0_N1 = 1")
        }
        case 3 => {
          printer.println("\tLaplace_Coeff_0_0_0 = -6")
          printer.println("\tLaplace_Coeff_P1_0_0 = 1")
          printer.println("\tLaplace_Coeff_N1_0_0 = 1")
          printer.println("\tLaplace_Coeff_0_P1_0 = 1")
          printer.println("\tLaplace_Coeff_0_N1_0 = 1")
          printer.println("\tLaplace_Coeff_0_0_P1 = 1")
          printer.println("\tLaplace_Coeff_0_0_N1 = 1")
        }
      }
    }

    if (Knowledge.kelvin) {
      printer.println("\tVariable timeSamples : Real = 0")
      printer.println("\tstartTimer ( timeSamplesWatch )")
      printer.println(s"\trepeat ${Knowledge.numSamples} times {")
    }

    if (Knowledge.kelvin) {
      if (Knowledge.genStencilFields) {
        for (lvl <- Knowledge.maxLevel to 0 by -1)
          printer.println(s"\tInitLaplace_GMRF@$lvl ( )")
      }
      printer.println("\tInitRHS_GMRF ( )")
      printer.println("\tInitSolution_GMRF ( )")
      printer.println("\tSolve_GMRF ( )")
    }

    if (Knowledge.kelvin) {
      // setup stencils for the actual PDE
      printer.println("\tloop over LaplaceCoeff@finest {")
      printer.println("\t\tLaplaceCoeff@finest[0] = TransferStencil_Center@finest * Solution_GMRF@finest")
      printer.println("\t\tLaplaceCoeff@finest[1] = TransferStencil_Right@finest * Solution_GMRF@finest")
      printer.println("\t\tLaplaceCoeff@finest[2] = TransferStencil_Left@finest * Solution_GMRF@finest")
      printer.println("\t\tLaplaceCoeff@finest[3] = TransferStencil_Up@finest * Solution_GMRF@finest")
      printer.println("\t\tLaplaceCoeff@finest[4] = TransferStencil_Down@finest * Solution_GMRF@finest")
      printer.println("\t}")

      if (Knowledge.genStencilFields) {
        for (lvl <- Knowledge.maxLevel - 1 to 0 by -1)
          printer.println(s"\tInitLaplace@$lvl ( )")
      }
    } else {
      if (Knowledge.genStencilFields) {
        for (lvl <- Knowledge.maxLevel to 0 by -1)
          printer.println(s"\tInitLaplace@$lvl ( )")
      }
    }

    printer.println("\tInitRHS ( )")
    printer.println("\tInitSolution ( )")
    printer.println("\tSolve ( )")

    if (Knowledge.kelvin) {
      printer.println(s"\tloop over SolutionMean@finest {")
      printer.println(s"\t\tSolutionMean@finest += ${Fields.solution(s"finest", "")(0)}")
      printer.println(s"\t}")
    }

    if (Knowledge.kelvin) {
      printer.println("\t}")
      if (Knowledge.testNewTimers) {
        printer.println("\tstopTimer ( timeSamplesWatch )")
        printer.println("\taddFromTimer ( timeSamplesWatch, timeSamples )")
      } else {
        printer.println("\tstopTimer ( timeSamplesWatch, timeSamples )")
      }
      printer.println("\tprint ( '\"Total time to solve: \"', timeSamples )")
      printer.println("\tprint ( '\"Mean time per sample: \"', " + s"timeSamples / ${Knowledge.numSamples} )")
    }

    if (Knowledge.kelvin) {
      printer.println(s"\tloop over SolutionMean@finest {")
      printer.println(s"\t\tSolutionMean@finest /= ${Knowledge.numSamples}")
      printer.println(s"\t}")

      printer.println(s"\tcommunicate SolutionMean@finest")
      printer.println(s"\tVariable solNorm : Real = 0.0")
      printer.println(s"\tloop over SolutionMean@finest where x > 0 && y > 0 ${if (Knowledge.dimensionality > 2) "&& z > 0 " else ""}with reduction( + : solNorm ) {")
      printer.println(s"\t\tsolNorm += SolutionMean@finest * SolutionMean@finest")
      printer.println(s"\t}")
      printer.println(s"\tsolNorm = ( sqrt ( solNorm ) ) / ${(Knowledge.domain_numFragsTotal_x - 2 * Knowledge.numHaloFrags) * (1 << Knowledge.maxLevel) - 1}")
      printer.println("\tprint ( '\"Norm of the solution: \"', solNorm )")
    }

    if (Knowledge.printFieldAtEnd) {
      if (Knowledge.kelvin)
        printer.println("\tprintField ( '\"Solution.dat\"', SolutionMean@finest )")
      else
        printer.println("\tprintField ( '\"Solution.dat\"', Solution@finest )")
    }

    if (Knowledge.testNewTimers) {
      if (Knowledge.genTimersPerFunction) {
        for (
          func <- Array(
            ("preSmooth", "pre-smoothing"),
            ("upResidual", "updating residual"),
            ("restriction", "restricting"),
            ("setSolution", "setting solution"),
            ("correction", "prolongating and correcting"),
            ("postSmooth", "post-smoothing"))
        ) {
          if (Knowledge.genTimersPerLevel) {
            for (level <- 0 to Knowledge.maxLevel)
              printer.println("\tprint ( '\"" + s"Total time spent on level $level in ${func._2}: " + "\"', " + s"getTotalFromTimer ( ${func._1}Timer@$level ) )")
          } else {
            printer.println("\tprint ( '\"" + s"Total time spent in ${func._2}: " + "\"', " + s"getTotalFromTimer ( ${func._1}Timer ) )")
          }
        }
      }
      if (Knowledge.genTimersForComm) {
        if (Knowledge.genCommTimersPerLevel) {
          for (level <- 0 to Knowledge.maxLevel)
            printer.println("\tprint ( '\"" + s"Total time spent communicating on level $level: " + "\"', " + s"getTotalFromTimer ( commTimer@$level ) )")
        } else {
          printer.println("\tprint ( '\"" + s"Total time spent communicating: " + "\"', " + s"getTotalFromTimer ( commTimer ) )")
        }
      }
    }

    printer.println("\tdestroyGlobals ( )")

    printer.println("}")
    printer.println
  }
}