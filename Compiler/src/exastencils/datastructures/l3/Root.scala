package exastencils.datastructures.l3

import exastencils.core._
import exastencils.datastructures._
import exastencils.knowledge._

case class Root() extends Node {
  var kelvin : Boolean = false

  var smoother : String = "Jac" // Jac | GS | RBGS
  var cgs : String = "CG" // CG
  var numPre : Int = 2 // has to be divisible by 2 for Jac
  var numPost : Int = 4 // has to be divisible by 2 for Jac
  var omega : Double = (if ("Jac" == smoother) 0.8 else 1.0)
  var testBC : Boolean = true && !kelvin // NOTE: the tested bc will only be reasonable for 2D cases
  var testExtFields : Boolean = false
  var printFieldAtEnd : Boolean = false
  var genSetableStencil : Boolean = false
  var omegaViaGlobals : Boolean = false
  var initSolWithRand : Boolean = !testBC && !kelvin
  var genRBSetsWithConditions : Boolean = true
  var useVecFields : Boolean = false // attempts to solve Poisson's equation for (numVecDims)D vectors; atm all three components are solved independently
  var numVecDims = (if (useVecFields) 2 else 1)
  var genStencilFields : Boolean = true || kelvin
  var useSlotsForJac : Boolean = true
  var testStencilStencil : Boolean = true || kelvin

  def solutionFields(level : String, postfix : String = "") = {
    if (useVecFields)
      (0 until numVecDims).toArray.map(d => s"Solution${if (useSlotsForJac) "[0]" else ""}@$level[$d]")
    else
      Array(s"Solution$postfix${if (useSlotsForJac) "[0]" else ""}@$level")
  }
  def solution2Fields(level : String, postfix : String = "") = {
    if (useVecFields)
      (0 until numVecDims).toArray.map(d => s"Solution${if (useSlotsForJac) "[1]" else "2"}@$level[$d]")
    else
      Array(s"Solution${if (useSlotsForJac) s"$postfix[1]" else s"2$postfix"}@$level")
  }
  def residualFields(level : String, postfix : String = "") = {
    if (useVecFields)
      (0 until numVecDims).toArray.map(d => s"Residual$postfix@$level[$d]")
    else
      Array(s"Residual$postfix@$level")
  }
  def rhsFields(level : String, postfix : String = "") = {
    if (useVecFields)
      (0 until numVecDims).toArray.map(d => s"RHS$postfix@$level[$d]")
    else
      Array(s"RHS$postfix@$level")
  }

  def stencilAccess(postfix : String) : String = {
    if (testStencilStencil) {
      if (genStencilFields)
        s"( CorrectionStencil@current * ( ToCoarser ( Laplace$postfix@finer ) * RestrictionStencil@current ) )"
      else
        s"( CorrectionStencil@current * ( Laplace$postfix@finer * RestrictionStencil@current ) )"
    } else
      s"Laplace$postfix@current"
  }

  def addDomains(printer : java.io.PrintWriter) = {
    Knowledge.dimensionality match {
      case 2 => {
        if (kelvin) {
          printer.println(s"Domain global< [ ${0.0 - (1.0 - 0.0) / (Knowledge.domain_numFragsTotal_x - 2)}, ${0.0 - (1.0 - 0.0) / (Knowledge.domain_numFragsTotal_y - 2)} ] " +
            s"to [ ${1.0 + (1.0 - 0.0) / (Knowledge.domain_numFragsTotal_x - 2)}, ${1.0 + (1.0 - 0.0) / (Knowledge.domain_numFragsTotal_y - 2)} ] >")
          printer.println("Domain innerDom< [ 0, 0 ] to [ 1, 1 ] >")
        } else {
          printer.println("Domain global< [ 0, 0 ] to [ 1, 1 ] >")
        }
      }
      case 3 => {
        printer.println("Domain global< [ 0, 0, 0 ] to [ 1, 1, 1 ] >")
      }
    }
    printer.println
  }

  def addLayouts(printer : java.io.PrintWriter) = {
    Knowledge.dimensionality match {
      case 2 => {
        printer.println("Layout BasicComm {")
        printer.println("\tghostLayers = [ 1, 1 ] with communication")
        printer.println("\tduplicateLayers = [ 1, 1 ] with communication")
        printer.println("\t// innerPoints = []")
        printer.println("}")
        printer.println("Layout NoComm {")
        printer.println("\tghostLayers = [ 0, 0 ]")
        printer.println("\tduplicateLayers = [ 1, 1 ]")
        printer.println("\t// innerPoints = []")
        printer.println("}")
        if (testExtFields) {
          printer.println("Layout ExtSolLayout {")
          printer.println("\tghostLayers = [ 0, 0 ]")
          printer.println("\tduplicateLayers = [ 0, 0 ]")
          printer.println(s"\tinnerPoints = [ ${(Knowledge.domain_fragLengthPerDim(0) * (1 << Knowledge.maxLevel)) + 1}, ${(Knowledge.domain_fragLengthPerDim(1) * (1 << Knowledge.maxLevel)) + 1} ]")
          printer.println("}")
        }
      }

      case 3 => {
        printer.println("Layout BasicComm {")
        printer.println("\tghostLayers = [ 1, 1, 1 ] with communication")
        printer.println("\tduplicateLayers = [ 1, 1, 1 ] with communication")
        printer.println("\t// innerPoints = []")
        printer.println("}")
        printer.println("Layout NoComm {")
        printer.println("\tghostLayers = [ 0, 0, 0 ]")
        printer.println("\tduplicateLayers = [ 1, 1, 1 ]")
        printer.println("\t// innerPoints = []")
        printer.println("}")
        if (testExtFields) {
          printer.println("Layout ExtSolLayout {")
          printer.println("\tghostLayers = [ 0, 0, 0 ]")
          printer.println("\tduplicateLayers = [ 0, 0, 0 ]")
          printer.println(s"\tinnerPoints = [ ${(Knowledge.domain_fragLengthPerDim(0) * (1 << Knowledge.maxLevel)) + 1}, ${(Knowledge.domain_fragLengthPerDim(1) * (1 << Knowledge.maxLevel)) + 1}, ${(Knowledge.domain_fragLengthPerDim(2) * (1 << Knowledge.maxLevel)) + 1} ]")
          printer.println("}")
        }
      }
    }
    printer.println
  }

  def addFields(printer : java.io.PrintWriter, postfix : String, domain : String) = {
    var fieldDatatype = (if (useVecFields) s"Array[Real][$numVecDims]" else "Real")
    if (testBC || (kelvin && "" == postfix)) {
      var bc = (
        if (kelvin && "" == postfix) "bcSol(xPos, yPos)"
        else "sin ( M_PI * xPos ) * sinh ( M_PI * yPos )")
      if ("Jac" == smoother) {
        if (useSlotsForJac) {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >[2]@(coarsest to (finest - 1))")
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, $bc >[2]@finest")
        } else {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@(coarsest to (finest - 1))")
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, $bc >@finest")
          printer.println(s"Field Solution2$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@(coarsest to (finest - 1))")
          printer.println(s"Field Solution2$postfix< $fieldDatatype, $domain, BasicComm, $bc >@finest")
        }
      } else {
        printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@(coarsest to (finest - 1))")
        printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, $bc >@finest")
      }
    } else {
      if ("Jac" == smoother) {
        if (useSlotsForJac) {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >[2]@all")
        } else {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@all")
          printer.println(s"Field Solution2$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@all")
        }
      } else {
        printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@all")
      }
    }
    printer.println(s"Field Residual$postfix< $fieldDatatype, $domain, BasicComm, None >@all")
    if (kelvin && "_GMRF" == postfix) {
      printer.println(s"Field RHS$postfix< $fieldDatatype, $domain, NoComm, 0.0 >@finest")
      printer.println(s"Field RHS$postfix< $fieldDatatype, $domain, NoComm, None >@(coarsest to (finest - 1))")
    } else
      printer.println(s"Field RHS$postfix< $fieldDatatype, $domain, NoComm, None >@all")
    if ("CG" == cgs) {
      printer.println(s"Field VecP$postfix< $fieldDatatype, $domain, BasicComm, None >@coarsest")
      printer.println(s"Field VecGradP$postfix< $fieldDatatype, $domain, NoComm, None >@coarsest")
    }
    printer.println
  }

  def addStencilFields(printer : java.io.PrintWriter, postfix : String, domain : String) = {
    if (genStencilFields) {
      if (testStencilStencil)
        printer.println(s"Field LaplaceCoeff$postfix< Array[Real][${var res = 1; for (i <- 0 until Knowledge.dimensionality) res *= 3; res}], $domain, BasicComm, 0.0 >@all")
      else
        printer.println(s"Field LaplaceCoeff$postfix< Array[Real][${2 * Knowledge.dimensionality + 1}], $domain, NoComm, None >@all")
      printer.println(s"StencilField Laplace$postfix< LaplaceCoeff$postfix => LaplaceStencil$postfix >@all")
      printer.println
    }
  }

  def addLaplaceStencil(printer : java.io.PrintWriter, postfix : String) = {
    if (genStencilFields)
      printer.println(s"Stencil LaplaceStencil$postfix@all {")
    else
      printer.println(s"Stencil Laplace$postfix@all {")
    if (genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("\t[ 0,  0] => Laplace_Coeff_0_0")
          printer.println("\t[ 1,  0] => Laplace_Coeff_P1_0")
          printer.println("\t[-1,  0] => Laplace_Coeff_N1_0")
          printer.println("\t[ 0,  1] => Laplace_Coeff_0_P1")
          printer.println("\t[ 0, -1] => Laplace_Coeff_0_N1")
        }
        case 3 =>
          printer.println("\t[ 0,  0,  0] => Laplace_Coeff_0_0_0")
          printer.println("\t[ 1,  0,  0] => Laplace_Coeff_P1_0_0")
          printer.println("\t[-1,  0,  0] => Laplace_Coeff_N1_0_0")
          printer.println("\t[ 0,  1,  0] => Laplace_Coeff_0_P1_0")
          printer.println("\t[ 0, -1,  0] => Laplace_Coeff_0_N1_0")
          printer.println("\t[ 0,  0,  1] => Laplace_Coeff_0_0_P1")
          printer.println("\t[ 0,  0, -1] => Laplace_Coeff_0_0_N1")
      }
    } else {
      Knowledge.dimensionality match {
        case 2 => {
          if (kelvin && "_GMRF" == postfix)
            printer.println("\t[ 0,  0] => (4.0 + kappa)")
          else
            printer.println("\t[ 0,  0] => 4.0")
          printer.println("\t[ 1,  0] => -1.0")
          printer.println("\t[-1,  0] => -1.0")
          printer.println("\t[ 0,  1] => -1.0")
          printer.println("\t[ 0, -1] => -1.0")
          if (testStencilStencil) {
            printer.println("\t[-1, -1] => 0.0")
            printer.println("\t[-1,  1] => 0.0")
            printer.println("\t[ 1, -1] => 0.0")
            printer.println("\t[ 1,  1] => 0.0")
          }
        }
        case 3 =>
          printer.println("\t[ 0,  0,  0] => 6.0")
          printer.println("\t[ 1,  0,  0] => -1.0")
          printer.println("\t[-1,  0,  0] => -1.0")
          printer.println("\t[ 0,  1,  0] => -1.0")
          printer.println("\t[ 0, -1,  0] => -1.0")
          printer.println("\t[ 0,  0,  1] => -1.0")
          printer.println("\t[ 0,  0, -1] => -1.0")
          if (testStencilStencil) {
            printer.println("\t[ 0, -1,  1] => 0.0")
            printer.println("\t[ 0, -1, -1] => 0.0")
            printer.println("\t[ 0,  1,  1] => 0.0")
            printer.println("\t[ 0,  1, -1] => 0.0")
            printer.println("\t[-1,  0,  1] => 0.0")
            printer.println("\t[-1,  0, -1] => 0.0")
            printer.println("\t[ 1,  0,  1] => 0.0")
            printer.println("\t[ 1,  0, -1] => 0.0")
            printer.println("\t[-1, -1,  0] => 0.0")
            printer.println("\t[-1,  1,  0] => 0.0")
            printer.println("\t[ 1, -1,  0] => 0.0")
            printer.println("\t[ 1,  1,  0] => 0.0")

            printer.println("\t[-1, -1,  1] => 0.0")
            printer.println("\t[-1, -1, -1] => 0.0")
            printer.println("\t[-1,  1,  1] => 0.0")
            printer.println("\t[-1,  1, -1] => 0.0")
            printer.println("\t[ 1, -1,  1] => 0.0")
            printer.println("\t[ 1, -1, -1] => 0.0")
            printer.println("\t[ 1,  1,  1] => 0.0")
            printer.println("\t[ 1,  1, -1] => 0.0")
          }
      }
    }
    printer.println("}")
  }

  def addDefaultStencils(printer : java.io.PrintWriter) = {
    Knowledge.dimensionality match {
      case 2 => {
        printer.println("Stencil CorrectionStencil@all {")
        printer.println("\t[    0,     0] => 0.25")
        printer.println("\t[x % 2,     0] => 0.25")
        printer.println("\t[    0, y % 2] => 0.25")
        printer.println("\t[x % 2, y % 2] => 0.25")
        printer.println("}")
      }
      case 3 => {
        printer.println("Stencil CorrectionStencil@all {")
        printer.println("\t[    0,     0,     0] => 0.0625")
        printer.println("\t[x % 2,     0,     0] => 0.0625")
        printer.println("\t[    0, y % 2,     0] => 0.0625")
        printer.println("\t[x % 2, y % 2,     0] => 0.0625")
        printer.println("\t[    0,     0, z % 2] => 0.0625")
        printer.println("\t[x % 2,     0, z % 2] => 0.0625")
        printer.println("\t[    0, y % 2, z % 2] => 0.0625")
        printer.println("\t[x % 2, y % 2, z % 2] => 0.0625")
        printer.println("}")
      }
    }

    Knowledge.dimensionality match {
      case 2 => {
        printer.println("Stencil RestrictionStencil@all {")
        printer.println("\t[ 0,  0] => 1.0")

        printer.println("\t[ 0, -1] => 0.5")
        printer.println("\t[ 0,  1] => 0.5")
        printer.println("\t[-1,  0] => 0.5")
        printer.println("\t[ 1,  0] => 0.5")

        printer.println("\t[-1, -1] => 0.25")
        printer.println("\t[-1,  1] => 0.25")
        printer.println("\t[ 1, -1] => 0.25")
        printer.println("\t[ 1,  1] => 0.25")
        printer.println("}")
      }
      case 3 => {
        printer.println("Stencil RestrictionStencil@all {")
        printer.println("\t[ 0,  0,  0] => 1.0")

        printer.println("\t[ 0,  0, -1] => 0.5")
        printer.println("\t[ 0,  0,  1] => 0.5")
        printer.println("\t[ 0, -1,  0] => 0.5")
        printer.println("\t[ 0,  1,  0] => 0.5")
        printer.println("\t[-1,  0,  0] => 0.5")
        printer.println("\t[ 1,  0,  0] => 0.5")

        printer.println("\t[ 0, -1,  1] => 0.25")
        printer.println("\t[ 0, -1, -1] => 0.25")
        printer.println("\t[ 0,  1,  1] => 0.25")
        printer.println("\t[ 0,  1, -1] => 0.25")
        printer.println("\t[-1,  0,  1] => 0.25")
        printer.println("\t[-1,  0, -1] => 0.25")
        printer.println("\t[ 1,  0,  1] => 0.25")
        printer.println("\t[ 1,  0, -1] => 0.25")
        printer.println("\t[-1, -1,  0] => 0.25")
        printer.println("\t[-1,  1,  0] => 0.25")
        printer.println("\t[ 1, -1,  0] => 0.25")
        printer.println("\t[ 1,  1,  0] => 0.25")

        printer.println("\t[-1, -1,  1] => 0.125")
        printer.println("\t[-1, -1, -1] => 0.125")
        printer.println("\t[-1,  1,  1] => 0.125")
        printer.println("\t[-1,  1, -1] => 0.125")
        printer.println("\t[ 1, -1,  1] => 0.125")
        printer.println("\t[ 1, -1, -1] => 0.125")
        printer.println("\t[ 1,  1,  1] => 0.125")
        printer.println("\t[ 1,  1, -1] => 0.125")
        printer.println("}")
      }
    }
    printer.println

    if (kelvin) {
      printer.println("Stencil TransferStencil_Center@all {")
      printer.println("\t[ 0,  0] => 2.0")
      printer.println("\t[-1,  0] => 0.5")
      printer.println("\t[ 1,  0] => 0.5")
      printer.println("\t[ 0,  1] => 0.5")
      printer.println("\t[ 0, -1] => 0.5")
      printer.println("}")
      printer.println("Stencil TransferStencil_Left@all {")
      printer.println("\t[ 0,  0] => -0.5")
      printer.println("\t[-1,  0] => -0.5")
      printer.println("}")
      printer.println("Stencil TransferStencil_Right@all {")
      printer.println("\t[ 0,  0] => -0.5")
      printer.println("\t[ 1,  0] => -0.5")
      printer.println("}")
      printer.println("Stencil TransferStencil_Up@all {")
      printer.println("\t[ 0,  0] => -0.5")
      printer.println("\t[ 0,  1] => -0.5")
      printer.println("}")
      printer.println("Stencil TransferStencil_Down@all {")
      printer.println("\t[ 0,  0] => -0.5")
      printer.println("\t[ 0, -1] => -0.5")
      printer.println("}")
    }
  }

  def addIterationSets(printer : java.io.PrintWriter) = {
    Knowledge.dimensionality match {
      case 2 => {
        printer.println("Set inner [1, 1] - [1, 1] steps [1, 1]")
        printer.println("Set innerForFieldsWithoutGhostLayers [0, 0] - [0, 0] steps [1, 1] // this concept might need some improvement")
        printer.println("Set domain [0, 0] - [0, 0] steps [1, 1]")
        if ("RBGS" == smoother) {
          if (genRBSetsWithConditions) {
            printer.println("Set red [1, 1] - [1, 1] steps [1, 1] with 0 == ((x + y) % 2)")
            printer.println("Set black [1, 1] - [1, 1] steps [1, 1] with 1 == ((x + y) % 2)")
          } else {
            printer.println("Set red [1 + (y % 2), 1] - [1, 1] steps [2, 1]")
            printer.println("Set black [2 - (y % 2), 1] - [1, 1] steps [2, 1]")
          }
        }
      }
      case 3 => {
        printer.println("Set inner [1, 1, 1] - [1, 1, 1] steps [1, 1, 1]")
        printer.println("Set innerForFieldsWithoutGhostLayers [0, 0, 0] - [0, 0, 0] steps [1, 1, 1] // this concept might need some improvement")
        printer.println("Set domain [0, 0, 0] - [0, 0, 0] steps [1, 1, 1]")
        if ("RBGS" == smoother) {
          if (genRBSetsWithConditions) {
            printer.println("Set red [1, 1, 1] - [1, 1, 1] steps [1, 1, 1] with 0 == ((x + y + z) % 2)")
            printer.println("Set black [1, 1, 1] - [1, 1, 1] steps [1, 1, 1] with 1 == ((x + y + z) % 2)")
          } else {
            printer.println("Set red [1 + ((y + z) % 2), 1, 1] - [1, 1, 1] steps [2, 1, 1]")
            printer.println("Set black [2 - ((y + z) % 2), 1, 1] - [1, 1, 1] steps [2, 1, 1]")
          }
        }
      }
    }
    printer.println
  }

  def addGlobals(printer : java.io.PrintWriter) = {
    printer.println("Globals {")
    if (omegaViaGlobals)
      printer.println(s"\tvar omega : Real = $omega")
    if (genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("\tvar Laplace_Coeff_0_0 : Real")
          printer.println("\tvar Laplace_Coeff_P1_0 : Real")
          printer.println("\tvar Laplace_Coeff_N1_0 : Real")
          printer.println("\tvar Laplace_Coeff_0_P1 : Real")
          printer.println("\tvar Laplace_Coeff_0_N1 : Real")
        }
        case 3 => {
          printer.println("\tvar Laplace_Coeff_0_0_0 : Real")
          printer.println("\tvar Laplace_Coeff_P1_0_0 : Real")
          printer.println("\tvar Laplace_Coeff_N1_0_0 : Real")
          printer.println("\tvar Laplace_Coeff_0_P1_0 : Real")
          printer.println("\tvar Laplace_Coeff_0_N1_0 : Real")
          printer.println("\tvar Laplace_Coeff_0_0_P1 : Real")
          printer.println("\tvar Laplace_Coeff_0_0_N1 : Real")
        }
      }
    }
    if (kelvin) {
      // Dir BC
      printer.println("\tvar UN : Real = 1")
      printer.println("\tvar US : Real = 10")
      printer.println("\tvar UE : Real = 5")
      printer.println("\tvar UW : Real = 3")
      // other parameters
      printer.println("\tvar alpha : Integer = 2")
      printer.println("\tvar sigma : Real = 0.3")
      printer.println("\tvar lambda : Real = 0.1")
      printer.println("\tvar nu : Real = 1 // alpha - dim/2")
      printer.println("\tvar kappa : Real = sqrt( 8 * nu ) / ( lambda )")
      printer.println("\tvar dim : Real = 2")
    }
    printer.println("}")
    printer.println
  }

  def addCGS(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"def VCycle$postfix@coarsest ( ) : Unit {")
    for (vecDim <- 0 until numVecDims)
      printer.println(s"\tVCycle${postfix}_$vecDim@current ( )")
    printer.println(s"}")

    for (vecDim <- 0 until numVecDims) {
      printer.println(s"def VCycle${postfix}_$vecDim@coarsest ( ) : Unit {")
      printer.println(s"\tUpResidual$postfix@current ( )")
      printer.println(s"\tcommunicate Residual$postfix@current")

      printer.println(s"\tvar res : Real = L2Residual${postfix}_$vecDim@current ( )")
      printer.println(s"\tvar initialRes : Real = res")

      printer.println(s"\tloop over inner on VecP$postfix@current {")
      printer.println(s"\t\tVecP$postfix@current = ${residualFields("current", postfix)(vecDim)}")
      printer.println(s"\t}")

      printer.println(s"\trepeat up 512 {")
      printer.println(s"\t\tcommunicate VecP$postfix@current")

      printer.println(s"\t\tloop over inner on VecP$postfix@current {")
      if (testStencilStencil && !genStencilFields)
        printer.println(s"\t\t\tVecGradP$postfix@current = ${stencilAccess(postfix)} * VecP$postfix@current")
      else
        printer.println(s"\t\t\tVecGradP$postfix@current = Laplace$postfix@current * VecP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar alphaDenom : Real = 0")
      printer.println(s"\t\tloop over inner on VecP$postfix@current with reduction( + : alphaDenom ) {")
      printer.println(s"\t\t\talphaDenom += VecP$postfix@current * VecGradP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar alpha : Real = res * res / alphaDenom")

      printer.println(s"\t\tloop over inner on Solution$postfix@current {")
      printer.println(s"\t\t\t${solutionFields("current", postfix)(vecDim)} += alpha * VecP$postfix@current")
      printer.println(s"\t\t\t${residualFields("current", postfix)(vecDim)} -= alpha * VecGradP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar nextRes : Real = L2Residual${postfix}_$vecDim@current ( )")

      printer.println(s"\t\tif ( nextRes <= 0.001 * initialRes ) {")
      printer.println(s"\t\t\treturn ( )")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar beta : Real = (nextRes * nextRes) / (res * res)")

      printer.println(s"\t\tloop over inner on VecP$postfix@current {")
      printer.println(s"\t\t\tVecP$postfix@current = ${residualFields("current", postfix)(vecDim)} + beta * VecP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tres = nextRes")
      printer.println(s"\t}")
      printer.println(s"}")
    }
    printer.println
  }

  def addCycle(printer : java.io.PrintWriter, postfix : String) = {
    if ("Jac" == smoother) {
      numPre /= 2
      numPost /= 2
    }
    printer.println(s"def VCycle$postfix@((coarsest + 1) to finest) ( ) : Unit {")
    printer.println(s"\trepeat up $numPre {")
    printer.println(s"\t\tSmoother$postfix@current ( )")
    printer.println(s"\t}")
    printer.println(s"\tUpResidual$postfix@current ( )")
    printer.println(s"\tRestriction$postfix@current ( )")
    printer.println(s"\tSetSolution$postfix@coarser ( 0 )")
    printer.println(s"\tVCycle$postfix@coarser ( )")
    printer.println(s"\tCorrection$postfix@current ( )")
    printer.println(s"\trepeat up $numPost {")
    printer.println(s"\t\tSmoother$postfix@current ( )")
    printer.println(s"\t}")
    printer.println(s"}")
    printer.println
  }

  def addSmoother(printer : java.io.PrintWriter, postfix : String) = {
    val omegaToPrint = (if (omegaViaGlobals) "omega" else omega)
    smoother match {
      case "Jac" => {
        if (testStencilStencil && !genStencilFields)
          printer.println(s"def Smoother$postfix@finest ( ) : Unit {")
        else
          printer.println(s"def Smoother$postfix@((coarsest + 1) to finest) ( ) : Unit {")

        printer.println(s"\tcommunicate Solution$postfix${if (useSlotsForJac) "[0]" else ""}@current")
        printer.println(s"\tloop over inner on Solution$postfix@current {")
        for (vecDim <- 0 until numVecDims)
          printer.println(s"\t\t${solution2Fields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( Laplace$postfix@current ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - Laplace$postfix@current * ${solutionFields(s"current", postfix)(vecDim)} ) )")
        printer.println(s"\t}")
        printer.println(s"\tcommunicate Solution${if (useSlotsForJac) s"$postfix[1]" else s"2$postfix"}@current")
        printer.println(s"\tloop over inner on Solution$postfix@current {")
        for (vecDim <- 0 until numVecDims)
          printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solution2Fields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( Laplace$postfix@current ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - Laplace$postfix@current * ${solution2Fields(s"current", postfix)(vecDim)} ) )")
        printer.println(s"\t}")
        printer.println(s"}")

        if (testStencilStencil && !genStencilFields) {
          printer.println(s"def Smoother$postfix@((coarsest + 1) to (finest - 1)) ( ) : Unit {")
          printer.println(s"\tcommunicate Solution$postfix${if (useSlotsForJac) "[0]" else ""}@current")
          printer.println(s"\tloop over inner on Solution$postfix@current {")
          for (vecDim <- 0 until numVecDims)
            printer.println(s"\t\t${solution2Fields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( ${stencilAccess(postfix)} ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - ( ${stencilAccess(postfix)} * ${solutionFields(s"current", postfix)(vecDim)} ) ) )")
          printer.println(s"\t}")
          printer.println(s"\tcommunicate Solution${if (useSlotsForJac) s"$postfix[1]" else s"2$postfix"}@current")
          printer.println(s"\tloop over inner on Solution$postfix@current {")
          for (vecDim <- 0 until numVecDims)
            printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solution2Fields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( ${stencilAccess(postfix)} ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - ( ${stencilAccess(postfix)} * ${solution2Fields(s"current", postfix)(vecDim)} ) ) )")
          printer.println(s"\t}")
          printer.println(s"}")
        }
      }
      case "RBGS" => {
        if (testStencilStencil && !genStencilFields)
          printer.println(s"def Smoother$postfix@finest ( ) : Unit {")
        else
          printer.println(s"def Smoother$postfix@((coarsest + 1) to finest) ( ) : Unit {")

        printer.println(s"\tcommunicate Solution$postfix@current")
        printer.println(s"\tloop over red on Solution$postfix@current {")
        for (vecDim <- 0 until numVecDims)
          printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( Laplace$postfix@current ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - Laplace$postfix@current * ${solutionFields(s"current", postfix)(vecDim)} ) )")
        printer.println(s"\t}")
        printer.println(s"\tcommunicate Solution$postfix@current")
        printer.println(s"\tloop over black on Solution$postfix@current {")
        for (vecDim <- 0 until numVecDims)
          printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( Laplace$postfix@current ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - Laplace$postfix@current * ${solutionFields(s"current", postfix)(vecDim)} ) )")
        printer.println(s"\t}")
        printer.println(s"}")

        if (testStencilStencil && !genStencilFields) {
          printer.println(s"def Smoother$postfix@((coarsest + 1) to (finest - 1)) ( ) : Unit {")
          printer.println(s"\tcommunicate Solution$postfix@current")
          printer.println(s"\tloop over red on Solution$postfix@current {")
          for (vecDim <- 0 until numVecDims)
            printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( ${stencilAccess(postfix)} ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - ${stencilAccess(postfix)} * ${solutionFields(s"current", postfix)(vecDim)} ) )")
          printer.println(s"\t}")
          printer.println(s"\tcommunicate Solution$postfix@current")
          printer.println(s"\tloop over black on Solution$postfix@current {")
          for (vecDim <- 0 until numVecDims)
            printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( ${stencilAccess(postfix)} ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - ${stencilAccess(postfix)} * ${solutionFields(s"current", postfix)(vecDim)} ) )")
          printer.println(s"\t}")
          printer.println(s"}")
        }
      }
      case "GS" => {
        if (testStencilStencil && !genStencilFields)
          printer.println(s"def Smoother$postfix@finest ( ) : Unit {")
        else
          printer.println(s"def Smoother$postfix@((coarsest + 1) to finest) ( ) : Unit {")

        printer.println(s"\tcommunicate Solution$postfix@current")
        printer.println(s"\tloop over inner on Solution$postfix@current {")
        for (vecDim <- 0 until numVecDims)
          printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( Laplace$postfix@current ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - Laplace$postfix@current * ${solutionFields(s"current", postfix)(vecDim)} ) )")
        printer.println(s"\t}")
        printer.println(s"}")

        if (testStencilStencil && !genStencilFields) {
          printer.println(s"def Smoother$postfix@((coarsest + 1) to (finest - 1)) ( ) : Unit {")
          printer.println(s"\tcommunicate Solution$postfix@current")
          printer.println(s"\tloop over inner on Solution$postfix@current {")
          for (vecDim <- 0 until numVecDims)
            printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( ${stencilAccess(postfix)} ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - ${stencilAccess(postfix)} * ${solutionFields(s"current", postfix)(vecDim)} ) )")
          printer.println(s"\t}")
          printer.println(s"}")
        }
      }
    }
    printer.println
  }

  def addUpResidual(printer : java.io.PrintWriter, postfix : String) = {
    if (testStencilStencil && !genStencilFields)
      printer.println(s"def UpResidual$postfix@finest ( ) : Unit {")
    else
      printer.println(s"def UpResidual$postfix@all ( ) : Unit {")
    printer.println(s"\tcommunicate Solution$postfix@current")
    printer.println(s"\tloop over inner on Residual$postfix@current {")
    for (vecDim <- 0 until numVecDims)
      printer.println(s"\t\t${residualFields(s"current", postfix)(vecDim)} = ${rhsFields(s"current", postfix)(vecDim)} - (Laplace$postfix@current * ${solutionFields(s"current", postfix)(vecDim)})")
    printer.println(s"\t}")
    printer.println(s"}")

    if (testStencilStencil && !genStencilFields) {
      printer.println(s"def UpResidual$postfix@(coarsest to (finest - 1)) ( ) : Unit {")
      printer.println(s"\tcommunicate Solution$postfix@current")
      printer.println(s"\tloop over inner on Residual$postfix@current {")
      for (vecDim <- 0 until numVecDims)
        printer.println(s"\t\t${residualFields(s"current", postfix)(vecDim)} = ${rhsFields(s"current", postfix)(vecDim)} - (${stencilAccess(postfix)} * ${solutionFields(s"current", postfix)(vecDim)})")
      printer.println(s"\t}")
      printer.println(s"}")
    }
  }

  def addRestriction(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"def Restriction$postfix@((coarsest + 1) to finest) ( ) : Unit {")
    printer.println(s"\tcommunicate Residual$postfix@current")
    printer.println(s"\tloop over innerForFieldsWithoutGhostLayers on RHS$postfix@coarser {")
    for (vecDim <- 0 until numVecDims)
      printer.println(s"\t\t${rhsFields(s"coarser", postfix)(vecDim)} = RestrictionStencil@current * ToCoarser ( ${residualFields(s"current", postfix)(vecDim)} )")
    printer.println(s"\t}")
    printer.println(s"}")
  }

  def addCorrection(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"def Correction$postfix@((coarsest + 1) to finest) ( ) : Unit {")
    printer.println(s"\tcommunicate Solution$postfix@current")
    printer.println(s"\tloop over inner on Solution$postfix@current {")
    for (vecDim <- 0 until numVecDims)
      printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} += CorrectionStencil@current * ToFiner ( ${solutionFields(s"coarser", postfix)(vecDim)} )")
    printer.println(s"\t}")
    printer.println(s"}")
    printer.println
  }

  def addUtilFunctions(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"def SetSolution$postfix@all (value : Real) : Unit {")
    printer.println(s"\tloop over domain on Solution$postfix@current {")
    for (vecDim <- 0 until numVecDims)
      printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = value")
    printer.println(s"\t}")
    printer.println(s"}")

    for (vecDim <- 0 until numVecDims) {
      printer.println(s"def L2Residual${postfix}_$vecDim@(coarsest and finest) ( ) : Real {")
      printer.println(s"\tcommunicate Residual$postfix@current")
      printer.println(s"\tvar res : Real = 0")
      printer.println(s"\tloop over inner on Residual$postfix@current with reduction( + : res ) {")
      printer.println(s"\t\t// FIXME: this counts duplicated values multiple times")
      printer.println(s"\t\tres += ${residualFields(s"current", postfix)(vecDim)} * ${residualFields(s"current", postfix)(vecDim)}")
      printer.println(s"\t}")
      printer.println(s"\treturn ( sqrt ( res ) )")
      printer.println(s"}")
      printer.println
    }
  }

  def addInitFields(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"def InitSolution$postfix ( ) : Unit {")
    if (initSolWithRand) {
      // FIXME: this loop needs to be marked as non-parallelizable somehow
      // FIXME: make results more reproducible via sth like 'std::srand((unsigned int)fragments[f]->id)'
      printer.println(s"\tloop over inner on Solution$postfix@finest {")
      for (vecDim <- 0 until numVecDims) {
        printer.println(s"\t\t${solutionFields(s"finest", postfix)(vecDim)} = native('((double)std::rand()/RAND_MAX)')")
      }
      printer.println(s"\t}")
    } else {
      printer.println(s"\tloop over inner on Solution$postfix@finest {")
      for (vecDim <- 0 until numVecDims) {
        printer.println(s"\t\t${solutionFields(s"finest", postfix)(vecDim)} = 0")
      }
      printer.println(s"\t}")
    }

    printer.println(s"}")

    printer.println(s"def InitRHS$postfix ( ) : Unit {")
    printer.println(s"\tloop over innerForFieldsWithoutGhostLayers on RHS$postfix@finest {")
    for (vecDim <- 0 until numVecDims) {
      printer.println(s"\t\t${rhsFields(s"finest", postfix)(vecDim)} = 0")
    }
    printer.println(s"\t}")
    printer.println(s"}")

    if (genStencilFields) {
      if (testStencilStencil) {
        printer.println(s"def InitLaplace$postfix@finest ( ) : Unit {")
        printer.println(s"\tloop over inner on LaplaceCoeff$postfix@current {")
      } else {
        printer.println(s"def InitLaplace$postfix@all ( ) : Unit {")
        printer.println(s"\tloop over innerForFieldsWithoutGhostLayers on LaplaceCoeff$postfix@current {")
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

      if (testStencilStencil) {
        printer.println(s"def InitLaplace$postfix@(coarsest to (finest - 1)) ( ) : Unit {")
        printer.println(s"\tcommunicate LaplaceCoeff$postfix@finer")
        printer.println(s"\tloop over inner on LaplaceCoeff$postfix@current {")
        if (false && kelvin) { // hack injection
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

  def printToL4(filename : String) : Unit = {
    var printer = new java.io.PrintWriter(filename)

    if (kelvin) {
      Settings.additionalIncludes += "#include <random>"
      Settings.additionalIncludes += "#include <functional>"
      Settings.additionalIncludes += "#include \"Util/Gamma.h\""
    }

    // Domains
    addDomains(printer)

    // Layouts
    addLayouts(printer)

    // Fields
    addFields(printer, "", "innerDom")
    if (kelvin)
      addFields(printer, "_GMRF", "global")

    if (kelvin) {
      printer.println(s"def bcSol (xPos : Real, yPos : Real) : Real {")
      printer.println(s"\tif ( yPos >= 1.0 ) { return ( UN ) }")
      printer.println(s"\tif ( xPos >= 1.0 ) { return ( UE ) }")
      printer.println(s"\tif ( yPos <= 0.0 ) { return ( US ) }")
      printer.println(s"\tif ( xPos <= 0.0 ) { return ( UW ) }")
      printer.println(s"\treturn ( 0.0 )")
      printer.println(s"}")
      printer.println
    }

    // Coeff/StencilFields
    addStencilFields(printer, "", "innerDom")
    if (kelvin)
      addStencilFields(printer, "_GMRF", "global")

    // External Fields
    if (testExtFields) {
      printer.println("external Field extSolution <ExtSolLayout> => Solution@(finest)")
      printer.println
    }

    // Stencils
    addLaplaceStencil(printer, "")
    addLaplaceStencil(printer, "_GMRF")
    addDefaultStencils(printer)

    // Iteration Sets
    addIterationSets(printer)

    // Globals
    addGlobals(printer)

    // CGS
    addCGS(printer, "")
    if (kelvin)
      addCGS(printer, "_GMRF")

    // Cycle
    addCycle(printer, "")
    if (kelvin)
      addCycle(printer, "_GMRF")

    // Smoother
    addSmoother(printer, "")
    if (kelvin)
      addSmoother(printer, "_GMRF")

    // Other MG Functions
    addUpResidual(printer, "")
    if (kelvin)
      addUpResidual(printer, "_GMRF")

    addRestriction(printer, "")
    if (kelvin)
      addRestriction(printer, "_GMRF")

    addCorrection(printer, "")
    if (kelvin)
      addCorrection(printer, "_GMRF")

    // Util Functions
    addUtilFunctions(printer, "")
    if (kelvin)
      addUtilFunctions(printer, "_GMRF")

    // initField functions
    addInitFields(printer, "")
    if (kelvin)
      addInitFields(printer, "_GMRF")

    // Kelvin
    if (kelvin) {
      printer.println("def solve_GMRF ( ) : Unit {")
      printer.println("\tnative ( \"std::srand(mpiRank)\" )")
      printer.println("\tnative ( \"std::default_random_engine generator\" )")
      printer.println("\tnative ( \"std::normal_distribution<double> distribution(0.0, 1.0)\" )")
      printer.println("\tnative ( \"auto randn = std::bind ( distribution, generator )\" )")

      printer.println(s"\tvar tau2 : Real = myGamma ( nu ) / ( myGamma ( nu + 0.5 ) * (( 4.0 * M_PI ) ** ( dim / 2.0 )) * ( kappa ** ( 2 * nu )) * sigma * sigma )")
      printer.println(s"\tloop over innerForFieldsWithoutGhostLayers on RHS_GMRF@finest {")
      printer.println(s"\t\tRHS_GMRF@finest = randn ( ) / ${Knowledge.domain_numFragsTotal_x * (1 << Knowledge.maxLevel)}")
      printer.println(s"\t}")
      printer.println(s"\tcommunicate RHS_GMRF@finest")
      printer.println(s"\tUpResidual_GMRF@finest ( )")
      printer.println(s"\tvar res0 : Real = L2Residual_GMRF_0@finest ( )")
      printer.println(s"\tvar res : Real = res0")
      printer.println(s"\tvar resold : Real = 0")
      printer.println("\tprint ( '\"Starting residual:\"', res0 )")
      printer.println(s"\trepeat up 10 {")
      printer.println(s"\t\tresold = res")
      printer.println(s"\t\tVCycle_GMRF@finest ( )")
      printer.println(s"\t\tUpResidual_GMRF@finest ( )")
      printer.println(s"\t\tres = L2Residual_GMRF_0@finest ( )")
      printer.println("\t\tprint ( '\"Residual:\"', res, '\"Residual reduction:\"', ( res0 / res ), '\"Convergence factor:\"', ( res / resold ) )")
      printer.println(s"\t}")
      printer.println(s"\tloop over inner on Solution_GMRF@finest {")
      printer.println(s"\t\tSolution_GMRF@finest = exp ( Solution_GMRF@finest / sqrt ( tau2 ) )")
      printer.println(s"\t}")
      printer.println(s"}")
      printer.println
    }

    // Application
    printer.println("def Application ( ) : Unit {")

    printer.println("\tvar setupTime : Real = 0")
    printer.println("\tstartTimer ( setupWatch )")

    printer.println("\tinitGlobals ( )")
    printer.println("\tinitDomain ( )")
    printer.println("\tinitFieldsWithZero ( )")

    printer.println("\tstopTimer ( setupWatch, setupTime )")
    printer.println("\tprint ( '\"Total time to setup: \"', setupTime )")

    if (genSetableStencil) {
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

    if (kelvin) {
      if (genStencilFields) {
        for (lvl <- Knowledge.maxLevel to 0 by -1)
          printer.println(s"\tInitLaplace_GMRF@$lvl ( )")
      }
      printer.println("\tInitRHS_GMRF ( )")
      printer.println("\tInitSolution_GMRF ( )")
      printer.println("\tsolve_GMRF ( )")
    }

    if (kelvin) {
      // setup stencils for the actual PDE
      printer.println("\tloop over inner on LaplaceCoeff@finest {")
      printer.println("\t\tLaplaceCoeff@finest[0] = TransferStencil_Center@finest * Solution_GMRF@finest")
      printer.println("\t\tLaplaceCoeff@finest[1] = TransferStencil_Right@finest * Solution_GMRF@finest")
      printer.println("\t\tLaplaceCoeff@finest[2] = TransferStencil_Left@finest * Solution_GMRF@finest")
      printer.println("\t\tLaplaceCoeff@finest[3] = TransferStencil_Up@finest * Solution_GMRF@finest")
      printer.println("\t\tLaplaceCoeff@finest[4] = TransferStencil_Down@finest * Solution_GMRF@finest")
      printer.println("\t}")

      if (genStencilFields) {
        for (lvl <- Knowledge.maxLevel - 1 to 0 by -1)
          printer.println(s"\tInitLaplace@$lvl ( )")
      }
    } else {
      if (genStencilFields) {
        for (lvl <- Knowledge.maxLevel to 0 by -1)
          printer.println(s"\tInitLaplace@$lvl ( )")
      }
    }

    printer.println("\tInitRHS ( )")
    printer.println("\tInitSolution ( )")
    printer.println("\tUpResidual@finest ( )")
    for (vecDim <- 0 until numVecDims) {
      printer.println(s"\tvar resStart_$vecDim : Real = L2Residual_$vecDim@finest (  )")
      printer.println(s"\tvar res_$vecDim : Real = resStart_$vecDim")
      printer.println(s"\tvar resOld_$vecDim : Real = 0")
      printer.println("\tprint ( '\"" + s"Starting residual at $vecDim" + "\"', " + s"resStart_$vecDim )")
    }
    printer.println("\tvar totalTime : Real = 0")
    printer.println("\tvar timeToSolve : Real = 0")
    printer.println("\tstartTimer ( timeToSolveWatch )")
    printer.println("\trepeat up 10 {")
    printer.println("\t\tstartTimer ( stopWatch )")
    printer.println("\t\tVCycle@finest (  )")
    printer.println("\t\tUpResidual@finest ( )")
    printer.println("\t\tstopTimer ( stopWatch, totalTime )")
    for (vecDim <- 0 until numVecDims) {
      printer.println(s"\t\tresOld_$vecDim = res_$vecDim")
      printer.println(s"\t\tres_$vecDim = L2Residual_$vecDim@finest (  )")
      printer.println("\t\tprint ( '\"" + s"Residual at $vecDim:" + "\"', " + s"res_$vecDim" + ", '\"Residual reduction:\"', " + s"( resStart_$vecDim / res_$vecDim ), " + "'\"Convergence factor:\"', " + s"( res_$vecDim / resOld_$vecDim ) )")
    }
    printer.println("\t}")
    printer.println("\tstopTimer ( timeToSolveWatch, timeToSolve )")
    printer.println("\tprint ( '\"Total time to solve: \"', timeToSolve )")
    printer.println("\tprint ( '\"Mean time per vCycle: \"', totalTime / 10 )")

    if (kelvin) {
      printer.println(s"\tvar solNorm : Real = 0.0")
      printer.println(s"\tloop over inner on Solution@finest with reduction( + : solNorm ) {")
      printer.println(s"\t\t// FIXME: this counts duplicated values multiple times")
      printer.println(s"\t\tsolNorm += ${solutionFields(s"finest", "")(0)} * ${solutionFields(s"finest", "")(0)}")
      printer.println(s"\t}")
      printer.println(s"\tsolNorm = ( sqrt ( solNorm ) ) / ${(Knowledge.domain_numFragsTotal_x - 2) * (1 << Knowledge.maxLevel) - 1}")
      printer.println("\tprint ( '\"Norm of the solution: \"', solNorm )")
    }

    if (printFieldAtEnd)
      printer.println("\tprintField ( '\"Solution.dat\"', Solution@finest )")

    printer.println("\tdestroyGlobals ( )")

    printer.println("}")
    printer.println

    printer.close()
  }
}
