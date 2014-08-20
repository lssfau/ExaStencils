package exastencils.datastructures.l3

import exastencils.core._
import exastencils.datastructures._
import exastencils.knowledge._
import exastencils.polyhedron._

case class Root() extends Node {
  def printToL4(filename : String) : Unit = {
    var printer = new java.io.PrintWriter(filename)

    if (Knowledge.testNewTimers) {
      Settings.additionalFiles += "Util/StopWatch.h"
      Settings.additionalFiles += "Util/StopWatch.cpp"
    }

    if (Knowledge.kelvin) {
      // TODO: set these settings via the settings file as soon as the required functionality is implemented

      Settings.additionalIncludes += "#include <random>"
      Settings.additionalIncludes += "#include <functional>"
      Settings.additionalIncludes += "#include \"Util/Gamma.h\""

      Settings.additionalFiles += "Util/Gamma.h"
      Settings.additionalFiles += "Util/Gamma.cpp"
    }

    // Domains
    Domains.addDomains(printer)

    // Layouts
    Layouts.addLayouts(printer)

    // Fields
    if (Knowledge.kelvin) {
      Fields.addFields(printer, "", "innerDom")
      Fields.addFields(printer, "_GMRF", "global")
    } else {
      Fields.addFields(printer, "", if (Knowledge.testDomainEmbedding) "innerDom" else "global")
    }

    if (Knowledge.kelvin) {
      PolyOpt.registerSideeffectFree("bcSol")
      printer.println(s"Function bcSol (xPos : Real, yPos : Real) : Real {")
      printer.println(s"\tif ( yPos >= 1.0 ) { return ( UN ) }")
      printer.println(s"\tif ( xPos >= 1.0 ) { return ( UE ) }")
      printer.println(s"\tif ( yPos <= 0.0 ) { return ( US ) }")
      printer.println(s"\tif ( xPos <= 0.0 ) { return ( UW ) }")
      printer.println(s"\treturn ( 0.0 )")
      printer.println(s"}")
      printer.println
    }

    // Coeff/StencilFields
    if (Knowledge.kelvin) {
      StencilFields.addStencilFields(printer, "", "innerDom")
      StencilFields.addStencilFields(printer, "_GMRF", "global")
    } else {
      StencilFields.addStencilFields(printer, "", if (Knowledge.testDomainEmbedding) "innerDom" else "global")
    }

    // External Fields
    if (Knowledge.testExtFields) {
      printer.println("external Field extSolution <ExtSolLayout> => Solution@(finest)")
      printer.println
    }

    // Stencils
    Stencils.addLaplaceStencil(printer, "")
    Stencils.addLaplaceStencil(printer, "_GMRF")
    Stencils.addDefaultStencils(printer)

    // Iteration Sets
    // addIterationSets(printer)

    // Globals
    Globals.addGlobals(printer)

    // CGS
    CGS.addFunction(printer, "")
    if (Knowledge.kelvin)
      CGS.addFunction(printer, "_GMRF")

    // Cycle
    Cycle.addCycle(printer, "")
    if (Knowledge.kelvin)
      Cycle.addCycle(printer, "_GMRF")

    // Smoother
    Smoothers.addFunction(printer, "")
    if (Knowledge.kelvin)
      Smoothers.addFunction(printer, "_GMRF")

    // Other MG Functions
    Residual.addUpdateFunction(printer, "")
    Residual.addReductionFunction(printer, "")
    if (Knowledge.kelvin) {
      Residual.addUpdateFunction(printer, "_GMRF")
      Residual.addReductionFunction(printer, "_GMRF")
    }

    Restriction.addFunction(printer, "")
    if (Knowledge.kelvin)
      Restriction.addFunction(printer, "_GMRF")

    Correction.addFunction(printer, "")
    if (Knowledge.kelvin)
      Correction.addFunction(printer, "_GMRF")

    // Util Functions
    Util.addFunctions(printer, "")
    if (Knowledge.kelvin)
      Util.addFunctions(printer, "_GMRF")

    // initField functions
    InitFields.addFunction(printer, "")
    if (Knowledge.kelvin)
      InitFields.addFunction(printer, "_GMRF")

    // Solver
    Solve.addFunction(printer)

    // Application
    Application.addFunction(printer)

    printer.close()
  }
}
