package exastencils.deprecated.l3Generate

import exastencils.base.l3.L3_Root
import exastencils.config._
import exastencils.datastructures._
import exastencils.polyhedron._

import scala.collection.mutable.ListBuffer

case class Root() {
  def printToL4(filename : String) : Unit = {
    val file = new java.io.File(filename)
    if (!file.getParentFile.exists()) file.getParentFile.mkdirs()
    val printer = new java.io.PrintWriter(filename)

    if (Knowledge.l3tmp_kelvin) {
      Settings.additionalIncludes += "random"
      Settings.additionalIncludes += "functional"
      Settings.additionalIncludes += "Util/Gamma.h"

      Settings.additionalFiles += "Util/Gamma.h"
      Settings.additionalFiles += "Util/Gamma.cpp"
    }

    // Domains
    Domains.addDomains(printer)

    // Layouts
    Layouts.addLayouts(printer)

    // Fields
    if (Knowledge.l3tmp_kelvin) {
      Fields.addFields(printer, "", "innerDom")
      Fields.addFields(printer, "_GMRF", "global")
    } else if (Knowledge.domain_useCase == "L-Shape") {
      Fields.addFields(printer, "", "LShaped")
    } else if (Knowledge.domain_useCase == "X-Shape") {
      Fields.addFields(printer, "", "XShaped")
    } else if (Knowledge.domain_useCase == "2-L-Shape") {
      Fields.addFields(printer, "", "LShapedUpper")
      Fields.addFields(printer, "", "LShapedLower")
    } else {
      Fields.addFields(printer, "", if (Knowledge.l3tmp_genEmbeddedDomain) "innerDom" else "global")
    }

    if (Knowledge.l3tmp_kelvin) {
      IR_PolyOpt.registerSideeffectFree("bcSol")
      printer.println(s"Function bcSol (xPos : Real, yPos : Real) : Real {")
      printer.println(s"\tif ( yPos >= 1.0 ) { return ( UN ) }")
      printer.println(s"\tif ( xPos >= 1.0 ) { return ( UE ) }")
      printer.println(s"\tif ( yPos <= 0.0 ) { return ( US ) }")
      printer.println(s"\tif ( xPos <= 0.0 ) { return ( UW ) }")
      printer.println(s"\treturn ( 0.0 )")
      printer.println(s"}")
      printer.println()
    }

    // Coeff/StencilFields
    if (Knowledge.l3tmp_kelvin) {
      StencilFields.addStencilFields(printer, "", "innerDom")
      StencilFields.addStencilFields(printer, "_GMRF", "global")
    } else if (Knowledge.domain_useCase == "L-Shape") {
      StencilFields.addStencilFields(printer, "", "global")
    } else if (Knowledge.domain_useCase == "2-L-Shape") {
      StencilFields.addStencilFields(printer, "", "LShapedUpper")
      StencilFields.addStencilFields(printer, "", "LShapedLower")
    } else {
      StencilFields.addStencilFields(printer, "", if (Knowledge.l3tmp_genEmbeddedDomain) "innerDom" else "global")
    }

    // External Fields
    if (Knowledge.l3tmp_genExtFields) {
      printer.println("external Field extSolution <ExtSolLayout> => Solution@(finest)")
      printer.println()
    }

    // Stencils
    Stencils.addLaplaceStencil(printer, "")
    if (Knowledge.l3tmp_kelvin)
      Stencils.addLaplaceStencil(printer, "_GMRF")
    Stencils.addDefaultStencils(printer)

    // Iteration Sets
    // addIterationSets(printer)

    // Globals
    Globals.addGlobals(printer)

    // CGS
    CGS.addFunction(printer, "")
    if (Knowledge.l3tmp_kelvin)
      CGS.addFunction(printer, "_GMRF")

    // Cycle
    Cycle.addCycle(printer, "")
    if (Knowledge.l3tmp_kelvin)
      Cycle.addCycle(printer, "_GMRF")

    // Smoother
    Smoothers.addFunction(printer, "")
    if (Knowledge.l3tmp_kelvin)
      Smoothers.addFunction(printer, "_GMRF")

    // Other MG Functions
    Residual.addUpdateFunction(printer, "")
    Residual.addReductionFunction(printer, "")
    if (Knowledge.l3tmp_kelvin) {
      Residual.addUpdateFunction(printer, "_GMRF")
      Residual.addReductionFunction(printer, "_GMRF")
    }

    // Error function
    if (Knowledge.l3tmp_printError)
      Error.addReductionFunction(printer, "")

    Restriction.addFunction(printer, "")
    if (Knowledge.l3tmp_kelvin)
      Restriction.addFunction(printer, "_GMRF")

    Correction.addFunction(printer, "")
    if (Knowledge.l3tmp_kelvin)
      Correction.addFunction(printer, "_GMRF")

    // Util Functions
    Util.addFunctions(printer, "")
    if (Knowledge.l3tmp_kelvin)
      Util.addFunctions(printer, "_GMRF")

    // initField functions
    InitFields.addFunction(printer, "")
    if (Knowledge.l3tmp_kelvin)
      InitFields.addFunction(printer, "_GMRF")

    // FMG functions
    if (Knowledge.l3tmp_genFMG) {
      FMG.addFunctions(printer)
    }

    // Solver
    Solve.addFunction(printer)

    // Application
    Application.addFunction(printer)

    printer.close()
  }
}
