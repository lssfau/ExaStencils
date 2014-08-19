package exastencils.datastructures.l3

import exastencils.core._
import exastencils.datastructures._
import exastencils.knowledge._
import exastencils.polyhedron._

case class Root() extends Node {
  def solutionFields(level : String, postfix : String = "") = {
    if (Knowledge.useVecFields)
      (0 until Knowledge.numVecDims).toArray.map(d => s"Solution${if (Knowledge.useSlotsForJac) "[0]" else ""}@$level[$d]")
    else
      Array(s"Solution$postfix${if (Knowledge.useSlotsForJac) "[0]" else ""}@$level")
  }
  def solution2Fields(level : String, postfix : String = "") = {
    if (Knowledge.useVecFields)
      (0 until Knowledge.numVecDims).toArray.map(d => s"Solution${if (Knowledge.useSlotsForJac) "[1]" else "2"}@$level[$d]")
    else
      Array(s"Solution${if (Knowledge.useSlotsForJac) s"$postfix[1]" else s"2$postfix"}@$level")
  }
  def residualFields(level : String, postfix : String = "") = {
    if (Knowledge.useVecFields)
      (0 until Knowledge.numVecDims).toArray.map(d => s"Residual$postfix@$level[$d]")
    else
      Array(s"Residual$postfix@$level")
  }
  def rhsFields(level : String, postfix : String = "") = {
    if (Knowledge.useVecFields)
      (0 until Knowledge.numVecDims).toArray.map(d => s"RHS$postfix@$level[$d]")
    else
      Array(s"RHS$postfix@$level")
  }

  def stencilAccess(postfix : String) : String = {
    if (Knowledge.testStencilStencil) {
      if (Knowledge.genStencilFields)
        s"( CorrectionStencil@current * ( ToCoarser ( Laplace$postfix@finer ) * RestrictionStencil@current ) )"
      else
        s"( CorrectionStencil@current * ( Laplace$postfix@finer * RestrictionStencil@current ) )"
    } else
      s"Laplace$postfix@current"
  }

  def addDomains(printer : java.io.PrintWriter) = {
    Knowledge.dimensionality match {
      case 2 => {
        if (Knowledge.kelvin) {
          printer.println(s"Domain global< [ ${0.0 - Knowledge.numHaloFrags * (1.0 - 0.0) / (Knowledge.domain_numFragsTotal_x - 2 * Knowledge.numHaloFrags)}, ${0.0 - Knowledge.numHaloFrags * (1.0 - 0.0) / (Knowledge.domain_numFragsTotal_y - 2 * Knowledge.numHaloFrags)} ] " +
            s"to [ ${1.0 + Knowledge.numHaloFrags * (1.0 - 0.0) / (Knowledge.domain_numFragsTotal_x - 2 * Knowledge.numHaloFrags)}, ${1.0 + Knowledge.numHaloFrags * (1.0 - 0.0) / (Knowledge.domain_numFragsTotal_y - 2 * Knowledge.numHaloFrags)} ] >")
          printer.println("Domain innerDom< [ 0, 0 ] to [ 1, 1 ] >")
        } else {
          printer.println("Domain global< [ 0, 0 ] to [ 1, 1 ] >")
          if (Knowledge.testDomainEmbedding)
            printer.println(s"Domain innerDom< [ ${0.0 + 1.0 / Knowledge.domain_numFragsTotal_x}, ${0.0 + 1.0 / Knowledge.domain_numFragsTotal_y} ] " +
              s"to [ ${1.0 - 1.0 / Knowledge.domain_numFragsTotal_x}, ${1.0 - 1.0 / Knowledge.domain_numFragsTotal_y} ] >")
        }
      }
      case 3 => {
        printer.println("Domain global< [ 0, 0, 0 ] to [ 1, 1, 1 ] >")
        if (Knowledge.testDomainEmbedding)
          printer.println(s"Domain innerDom< [ ${0.0 + 1.0 / Knowledge.domain_numFragsTotal_x}, ${0.0 + 1.0 / Knowledge.domain_numFragsTotal_y} ], ${0.0 + 1.0 / Knowledge.domain_numFragsTotal_z} ] " +
            s"to [ ${1.0 - 1.0 / Knowledge.domain_numFragsTotal_x}, ${1.0 - 1.0 / Knowledge.domain_numFragsTotal_y} ], ${1.0 - 1.0 / Knowledge.domain_numFragsTotal_z} ] >")
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
        if (Knowledge.testExtFields) {
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
        if (Knowledge.testExtFields) {
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
    var fieldDatatype = (if (Knowledge.useVecFields) s"Array[Real][${Knowledge.numVecDims}]" else "Real")
    if (Knowledge.testBC || (Knowledge.kelvin && "" == postfix)) {
      var bc = (
        if (Knowledge.kelvin && "" == postfix) "bcSol(xPos, yPos)"
        else "sin ( M_PI * xPos ) * sinh ( M_PI * yPos )")
      if ("Jac" == Knowledge.smoother) {
        if (Knowledge.useSlotsForJac) {
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
      if ("Jac" == Knowledge.smoother) {
        if (Knowledge.useSlotsForJac) {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >[2]@all")
        } else {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@all")
          printer.println(s"Field Solution2$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@all")
        }
      } else {
        printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@all")
      }
    }

    if (Knowledge.kelvin && "" == postfix)
      printer.println(s"Field SolutionMean< $fieldDatatype, $domain, NoComm, None >@all")

    printer.println(s"Field Residual$postfix< $fieldDatatype, $domain, BasicComm, None >@all")
    if (Knowledge.kelvin && "_GMRF" == postfix) {
      printer.println(s"Field RHS$postfix< $fieldDatatype, $domain, NoComm, 0.0 >@finest")
      printer.println(s"Field RHS$postfix< $fieldDatatype, $domain, NoComm, None >@(coarsest to (finest - 1))")
    } else
      printer.println(s"Field RHS$postfix< $fieldDatatype, $domain, NoComm, None >@all")
    if ("CG" == Knowledge.cgs) {
      printer.println(s"Field VecP$postfix< $fieldDatatype, $domain, BasicComm, None >@coarsest")
      printer.println(s"Field VecGradP$postfix< $fieldDatatype, $domain, NoComm, None >@coarsest")
    }
    printer.println
  }

  def addStencilFields(printer : java.io.PrintWriter, postfix : String, domain : String) = {
    if (Knowledge.genStencilFields) {
      if (Knowledge.testStencilStencil)
        printer.println(s"Field LaplaceCoeff$postfix< Array[Real][${var res = 1; for (i <- 0 until Knowledge.dimensionality) res *= 3; res}], $domain, BasicComm, 0.0 >@all")
      else
        printer.println(s"Field LaplaceCoeff$postfix< Array[Real][${2 * Knowledge.dimensionality + 1}], $domain, NoComm, None >@all")
      printer.println(s"StencilField Laplace$postfix< LaplaceCoeff$postfix => LaplaceStencil$postfix >@all")
      printer.println
    }
  }

  def addLaplaceStencil(printer : java.io.PrintWriter, postfix : String) = {
    if (Knowledge.genStencilFields)
      printer.println(s"Stencil LaplaceStencil$postfix@all {")
    else
      printer.println(s"Stencil Laplace$postfix@all {")
    if (Knowledge.genSetableStencil) {
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
          if (Knowledge.kelvin && "_GMRF" == postfix)
            printer.println("\t[ 0,  0] => (4.0 + kappa)")
          else
            printer.println("\t[ 0,  0] => 4.0")
          printer.println("\t[ 1,  0] => -1.0")
          printer.println("\t[-1,  0] => -1.0")
          printer.println("\t[ 0,  1] => -1.0")
          printer.println("\t[ 0, -1] => -1.0")
          if (Knowledge.testStencilStencil) {
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
          if (Knowledge.testStencilStencil) {
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

    if (Knowledge.kelvin) {
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
        printer.println("Set inner [0, 0]")
        if ("RBGS" == Knowledge.smoother) {
          if (Knowledge.genRBSetsWithConditions) {
            printer.println("Set red [0, 0] with 0 == ((x + y) % 2)")
            printer.println("Set black [0, 0] with 1 == ((x + y) % 2)")
          } else {
            printer.println("Set red [0 + (y % 2), 0] - [0, 0] steps [2, 1]")
            printer.println("Set black [1 - (y % 2), 0] - [0, 0] steps [2, 1]")
          }
        }
      }
      case 3 => {
        printer.println("Set inner [0, 0, 0]")
        if ("RBGS" == Knowledge.smoother) {
          if (Knowledge.genRBSetsWithConditions) {
            printer.println("Set red [0, 0, 0] with 0 == ((x + y + z) % 2)")
            printer.println("Set black [0, 0, 0] with 1 == ((x + y + z) % 2)")
          } else {
            printer.println("Set red [0 + ((y + z) % 2), 0, 0] - [0, 0, 0] steps [2, 1, 1]")
            printer.println("Set black [1 - ((y + z) % 2), 0, 0] - [0, 0, 0] steps [2, 1, 1]")
          }
        }
      }
    }
    printer.println
  }

  def addGlobals(printer : java.io.PrintWriter) = {
    printer.println("Globals {")
    if (Knowledge.omegaViaGlobals)
      printer.println(s"\tvar omega : Real = ${Knowledge.omega}")
    if (Knowledge.genSetableStencil) {
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
    if (Knowledge.kelvin) {
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
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\tVCycle${postfix}_$vecDim@current ( )")
    printer.println(s"}")

    for (vecDim <- 0 until Knowledge.numVecDims) {
      printer.println(s"def VCycle${postfix}_$vecDim@coarsest ( ) : Unit {")
      if (Knowledge.testCommCompOverlap)
        printer.println(s"\tUpResidual$postfix@current ( 0 )")
      else
        printer.println(s"\tUpResidual$postfix@current ( )")
      if (Knowledge.genTimersForComm)
        printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
      printer.println(s"\tcommunicate Residual$postfix@current")
      if (Knowledge.genTimersForComm)
        printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")

      printer.println(s"\tvar res : Real = L2Residual${postfix}_$vecDim@current ( )")
      printer.println(s"\tvar initialRes : Real = res")

      printer.println(s"\tloop over VecP$postfix@current {")
      printer.println(s"\t\tVecP$postfix@current = ${residualFields("current", postfix)(vecDim)}")
      printer.println(s"\t}")

      printer.println(s"\trepeat up 512 {")
      if (Knowledge.genTimersForComm)
        printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
      printer.println(s"\t\tcommunicate VecP$postfix@current")
      if (Knowledge.genTimersForComm)
        printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")

      printer.println(s"\t\tloop over VecP$postfix@current {")
      if (Knowledge.testStencilStencil && !Knowledge.genStencilFields)
        printer.println(s"\t\t\tVecGradP$postfix@current = ${stencilAccess(postfix)} * VecP$postfix@current")
      else
        printer.println(s"\t\t\tVecGradP$postfix@current = Laplace$postfix@current * VecP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar alphaDenom : Real = 0")
      printer.println(s"\t\tloop over VecP$postfix@current with reduction( + : alphaDenom ) {")
      printer.println(s"\t\t\talphaDenom += VecP$postfix@current * VecGradP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar alpha : Real = res * res / alphaDenom")

      printer.println(s"\t\tloop over Solution$postfix@current {")
      printer.println(s"\t\t\t${solutionFields("current", postfix)(vecDim)} += alpha * VecP$postfix@current")
      printer.println(s"\t\t\t${residualFields("current", postfix)(vecDim)} -= alpha * VecGradP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar nextRes : Real = L2Residual${postfix}_$vecDim@current ( )")

      printer.println(s"\t\tif ( nextRes <= 0.001 * initialRes ) {")
      printer.println(s"\t\t\treturn ( )")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar beta : Real = (nextRes * nextRes) / (res * res)")

      printer.println(s"\t\tloop over VecP$postfix@current {")
      printer.println(s"\t\t\tVecP$postfix@current = ${residualFields("current", postfix)(vecDim)} + beta * VecP$postfix@current")
      printer.println(s"\t\t}")

      printer.println(s"\t\tres = nextRes")
      printer.println(s"\t}")
      printer.println(s"}")
    }
    printer.println
  }

  def addCycle(printer : java.io.PrintWriter, postfix : String) = {
    if ("Jac" == Knowledge.smoother) {
      Knowledge.numPre /= 2
      Knowledge.numPost /= 2
    }
    printer.println(s"def VCycle$postfix@((coarsest + 1) to finest) ( ) : Unit {")

    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstartTimer ( preSmoothTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")
    printer.println(s"\trepeat up ${Knowledge.numPre} {")
    printer.println(s"\t\tSmoother$postfix@current ( )")
    printer.println(s"\t}")
    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstopTimer ( preSmoothTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")

    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstartTimer ( upResidualTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")
    if (Knowledge.testCommCompOverlap)
      printer.println(s"\tUpResidual$postfix@current ( 1 )")
    else
      printer.println(s"\tUpResidual$postfix@current ( )")
    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstopTimer ( upResidualTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")

    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstartTimer ( restrictionTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tRestriction$postfix@current ( )")
    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstopTimer ( restrictionTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")

    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstartTimer ( setSolutionTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tSetSolution$postfix@coarser ( 0 )")
    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstopTimer ( setSolutionTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")

    printer.println(s"\tVCycle$postfix@coarser ( )")

    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstartTimer ( correctionTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tCorrection$postfix@current ( )")
    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstopTimer ( correctionTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")

    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstartTimer ( postSmoothTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")
    printer.println(s"\trepeat up ${Knowledge.numPost} {")
    printer.println(s"\t\tSmoother$postfix@current ( )")
    printer.println(s"\t}")
    if (Knowledge.genTimersPerFunction)
      printer.println(s"\tstopTimer ( postSmoothTimer$postfix${if (Knowledge.genTimersPerLevel) "@current" else ""} )")

    printer.println(s"}")
    printer.println
  }

  def addSmoother(printer : java.io.PrintWriter, postfix : String) = {
    val omegaToPrint = (if (Knowledge.omegaViaGlobals) "omega" else Knowledge.omega)
    Knowledge.smoother match {
      case "Jac" => {
        if (Knowledge.testStencilStencil && !Knowledge.genStencilFields)
          printer.println(s"def Smoother$postfix@finest ( ) : Unit {")
        else
          printer.println(s"def Smoother$postfix@((coarsest + 1) to finest) ( ) : Unit {")

        if (Knowledge.genTimersForComm)
          printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
        printer.println(s"\tcommunicate Solution$postfix${if (Knowledge.useSlotsForJac) "[0]" else ""}@current")
        if (Knowledge.genTimersForComm)
          printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
        printer.println(s"\tloop over Solution$postfix@current {")
        for (vecDim <- 0 until Knowledge.numVecDims)
          printer.println(s"\t\t${solution2Fields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( Laplace$postfix@current ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - Laplace$postfix@current * ${solutionFields(s"current", postfix)(vecDim)} ) )")
        printer.println(s"\t}")
        if (Knowledge.genTimersForComm)
          printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
        printer.println(s"\tcommunicate Solution${if (Knowledge.useSlotsForJac) s"$postfix[1]" else s"2$postfix"}@current")
        if (Knowledge.genTimersForComm)
          printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
        printer.println(s"\tloop over Solution$postfix@current {")
        for (vecDim <- 0 until Knowledge.numVecDims)
          printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solution2Fields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( Laplace$postfix@current ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - Laplace$postfix@current * ${solution2Fields(s"current", postfix)(vecDim)} ) )")
        printer.println(s"\t}")
        printer.println(s"}")

        if (Knowledge.testStencilStencil && !Knowledge.genStencilFields) {
          printer.println(s"def Smoother$postfix@((coarsest + 1) to (finest - 1)) ( ) : Unit {")
          if (Knowledge.genTimersForComm)
            printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
          printer.println(s"\tcommunicate Solution$postfix${if (Knowledge.useSlotsForJac) "[0]" else ""}@current")
          if (Knowledge.genTimersForComm)
            printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
          printer.println(s"\tloop over Solution$postfix@current {")
          for (vecDim <- 0 until Knowledge.numVecDims)
            printer.println(s"\t\t${solution2Fields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( ${stencilAccess(postfix)} ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - ( ${stencilAccess(postfix)} * ${solutionFields(s"current", postfix)(vecDim)} ) ) )")
          printer.println(s"\t}")
          if (Knowledge.genTimersForComm)
            printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
          printer.println(s"\tcommunicate Solution${if (Knowledge.useSlotsForJac) s"$postfix[1]" else s"2$postfix"}@current")
          if (Knowledge.genTimersForComm)
            printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
          printer.println(s"\tloop over Solution$postfix@current {")
          for (vecDim <- 0 until Knowledge.numVecDims)
            printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solution2Fields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( ${stencilAccess(postfix)} ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - ( ${stencilAccess(postfix)} * ${solution2Fields(s"current", postfix)(vecDim)} ) ) )")
          printer.println(s"\t}")
          printer.println(s"}")
        }
      }
      case "RBGS" => {
        if (Knowledge.testStencilStencil && !Knowledge.genStencilFields)
          printer.println(s"def Smoother$postfix@finest ( ) : Unit {")
        else
          printer.println(s"def Smoother$postfix@((coarsest + 1) to finest) ( ) : Unit {")

        if (Knowledge.genTimersForComm)
          printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
        printer.println(s"\tcommunicate Solution$postfix@current")
        if (Knowledge.genTimersForComm)
          printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")

        Knowledge.dimensionality match {
          case 2 => {
            if (Knowledge.genRBSetsWithConditions)
              printer.println(s"\tloop over Solution$postfix@current where 0 == ((x + y) % 2) {")
            else
              printer.println(s"\tloop over Solution$postfix@current starting [ 0 + (y % 2), 0 ] ending [ 0, 0 ] stepping [ 2, 1 ] {")
          }
          case 3 => {
            if (Knowledge.genRBSetsWithConditions)
              printer.println(s"\tloop over Solution$postfix@current where 0 == ((x + y + z) % 2) {")
            else
              printer.println(s"\tloop over Solution$postfix@current starting [ 0 + ((y + z) % 2), 0 ] ending [ 0, 0, 0 ] stepping [ 2, 1, 1 ] {")
          }
        }
        for (vecDim <- 0 until Knowledge.numVecDims)
          printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( Laplace$postfix@current ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - Laplace$postfix@current * ${solutionFields(s"current", postfix)(vecDim)} ) )")
        printer.println(s"\t}")

        if (Knowledge.genTimersForComm)
          printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
        printer.println(s"\tcommunicate Solution$postfix@current")
        if (Knowledge.genTimersForComm)
          printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")

        Knowledge.dimensionality match {
          case 2 => {
            if (Knowledge.genRBSetsWithConditions)
              printer.println(s"\tloop over Solution$postfix@current where 1 == ((x + y) % 2) {")
            else
              printer.println(s"\tloop over Solution$postfix@current starting [ 1 - (y % 2), 0 ] ending [ 0, 0 ] stepping [ 2, 1 ] {")
          }
          case 3 => {
            if (Knowledge.genRBSetsWithConditions)
              printer.println(s"\tloop over Solution$postfix@current where 1 == ((x + y + z) % 2) {")
            else
              printer.println(s"\tloop over Solution$postfix@current starting [ 1 - ((y + z) % 2), 0 ] ending [ 0, 0, 0 ] stepping [ 2, 1, 1 ] {")
          }
        }
        for (vecDim <- 0 until Knowledge.numVecDims)
          printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( Laplace$postfix@current ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - Laplace$postfix@current * ${solutionFields(s"current", postfix)(vecDim)} ) )")
        printer.println(s"\t}")
        printer.println(s"}")

        if (Knowledge.testStencilStencil && !Knowledge.genStencilFields) {
          printer.println(s"def Smoother$postfix@((coarsest + 1) to (finest - 1)) ( ) : Unit {")
          if (Knowledge.genTimersForComm)
            printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
          printer.println(s"\tcommunicate Solution$postfix@current")
          if (Knowledge.genTimersForComm)
            printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")

          Knowledge.dimensionality match {
            case 2 => {
              if (Knowledge.genRBSetsWithConditions)
                printer.println(s"\tloop over Solution$postfix@current where 0 == ((x + y) % 2) {")
              else
                printer.println(s"\tloop over Solution$postfix@current starting [ 0 + (y % 2), 0 ] ending [ 0, 0 ] stepping [ 2, 1 ] {")
            }
            case 3 => {
              if (Knowledge.genRBSetsWithConditions)
                printer.println(s"\tloop over Solution$postfix@current where 0 == ((x + y + z) % 2) {")
              else
                printer.println(s"\tloop over Solution$postfix@current starting [ 0 + ((y + z) % 2), 0 ] ending [ 0, 0, 0 ] stepping [ 2, 1, 1 ] {")
            }
          }
          for (vecDim <- 0 until Knowledge.numVecDims)
            printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( ${stencilAccess(postfix)} ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - ${stencilAccess(postfix)} * ${solutionFields(s"current", postfix)(vecDim)} ) )")
          printer.println(s"\t}")

          if (Knowledge.genTimersForComm)
            printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
          printer.println(s"\tcommunicate Solution$postfix@current")
          if (Knowledge.genTimersForComm)
            printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")

          Knowledge.dimensionality match {
            case 2 => {
              if (Knowledge.genRBSetsWithConditions)
                printer.println(s"\tloop over Solution$postfix@current where 1 == ((x + y) % 2) {")
              else
                printer.println(s"\tloop over Solution$postfix@current starting [ 1 - (y % 2), 0 ] ending [ 0, 0 ] stepping [ 2, 1 ] {")
            }
            case 3 => {
              if (Knowledge.genRBSetsWithConditions)
                printer.println(s"\tloop over Solution$postfix@current where 1 == ((x + y + z) % 2) {")
              else
                printer.println(s"\tloop over Solution$postfix@current starting [ 1 - ((y + z) % 2), 0 ] ending [ 0, 0, 0 ] stepping [ 2, 1, 1 ] {")
            }
          }
          printer.println(s"\tloop over black on Solution$postfix@current {")
          for (vecDim <- 0 until Knowledge.numVecDims)
            printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( ${stencilAccess(postfix)} ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - ${stencilAccess(postfix)} * ${solutionFields(s"current", postfix)(vecDim)} ) )")
          printer.println(s"\t}")
          printer.println(s"}")
        }
      }
      case "GS" => {
        if (Knowledge.testStencilStencil && !Knowledge.genStencilFields)
          printer.println(s"def Smoother$postfix@finest ( ) : Unit {")
        else
          printer.println(s"def Smoother$postfix@((coarsest + 1) to finest) ( ) : Unit {")

        if (Knowledge.genTimersForComm)
          printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
        printer.println(s"\tcommunicate Solution$postfix@current")
        if (Knowledge.genTimersForComm)
          printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
        printer.println(s"\tloop over Solution$postfix@current {")
        for (vecDim <- 0 until Knowledge.numVecDims)
          printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( Laplace$postfix@current ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - Laplace$postfix@current * ${solutionFields(s"current", postfix)(vecDim)} ) )")
        printer.println(s"\t}")
        printer.println(s"}")

        if (Knowledge.testStencilStencil && !Knowledge.genStencilFields) {
          printer.println(s"def Smoother$postfix@((coarsest + 1) to (finest - 1)) ( ) : Unit {")
          if (Knowledge.genTimersForComm)
            printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
          printer.println(s"\tcommunicate Solution$postfix@current")
          if (Knowledge.genTimersForComm)
            printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
          printer.println(s"\tloop over Solution$postfix@current {")
          for (vecDim <- 0 until Knowledge.numVecDims)
            printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = ${solutionFields(s"current", postfix)(vecDim)} + ( ( ( 1.0 / diag ( ${stencilAccess(postfix)} ) ) * $omegaToPrint ) * ( ${rhsFields(s"current", postfix)(vecDim)} - ${stencilAccess(postfix)} * ${solutionFields(s"current", postfix)(vecDim)} ) )")
          printer.println(s"\t}")
          printer.println(s"}")
        }
      }
    }
    printer.println
  }

  def addUpResidual(printer : java.io.PrintWriter, postfix : String) = {
    val levels = if (Knowledge.testStencilStencil && !Knowledge.genStencilFields) "finest" else "all"
    val params = if (Knowledge.testCommCompOverlap) "startComm : Integer" else ""

    printer.println(s"def UpResidual$postfix@$levels ( $params ) : Unit {")
    if (Knowledge.genTimersForComm)
      printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tcommunicate Solution$postfix@current")
    if (Knowledge.genTimersForComm)
      printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
    if (Knowledge.testFragLoops)
      printer.println(s"\tloop over fragments {")
    printer.println(s"\tloop over Residual$postfix@current {")
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\t\t${residualFields(s"current", postfix)(vecDim)} = ${rhsFields(s"current", postfix)(vecDim)} - (Laplace$postfix@current * ${solutionFields(s"current", postfix)(vecDim)})")
    printer.println(s"\t}")
    if (Knowledge.testFragLoops)
      printer.println(s"\t}")

    if (Knowledge.testCommCompOverlap) {
      printer.println(s"\tif ( levels@current > levels@coarsest ) {") // TODO: merge to one condition as soon as supported by l4 parser
      printer.println(s"\tif ( startComm > 0 ) {")
      printer.println(s"\t\tbegin communicate Residual$postfix@current")
      printer.println(s"\t}")
      printer.println(s"\t}")
    }
    printer.println(s"}")

    if (Knowledge.testStencilStencil && !Knowledge.genStencilFields) {
      printer.println(s"def UpResidual$postfix@(coarsest to (finest - 1)) ( $params ) : Unit {")
      if (Knowledge.genTimersForComm)
        printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
      printer.println(s"\tcommunicate Solution$postfix@current")
      if (Knowledge.genTimersForComm)
        printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
      printer.println(s"\tloop over Residual$postfix@current {")
      for (vecDim <- 0 until Knowledge.numVecDims)
        printer.println(s"\t\t${residualFields(s"current", postfix)(vecDim)} = ${rhsFields(s"current", postfix)(vecDim)} - (${stencilAccess(postfix)} * ${solutionFields(s"current", postfix)(vecDim)})")
      printer.println(s"\t}")
      if (Knowledge.testCommCompOverlap) {
        printer.println(s"\tif ( levels@current > levels@coarsest ) {") // TODO: merge to one condition as soon as supported by l4 parser
        printer.println(s"\tif ( startComm > 0 ) {")
        printer.println(s"\t\tbegin communicate Residual$postfix@current")
        printer.println(s"\t}")
        printer.println(s"\t}")
      }
      printer.println(s"}")
    }
  }

  def addRestriction(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"def Restriction$postfix@((coarsest + 1) to finest) ( ) : Unit {")
    if (Knowledge.testCommCompOverlap)
      printer.println(s"\tfinish communicate Residual$postfix@current")
    else {
      if (Knowledge.genTimersForComm)
        printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
      printer.println(s"\tcommunicate Residual$postfix@current")
      if (Knowledge.genTimersForComm)
        printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
    }
    if (Knowledge.testFragLoops)
      printer.println(s"\tloop over fragments {")
    printer.println(s"\tloop over RHS$postfix@coarser {")
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\t\t${rhsFields(s"coarser", postfix)(vecDim)} = RestrictionStencil@current * ToCoarser ( ${residualFields(s"current", postfix)(vecDim)} )")
    if (Knowledge.testFragLoops)
      printer.println(s"\t}")
    printer.println(s"\t}")
    printer.println(s"}")
  }

  def addCorrection(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"def Correction$postfix@((coarsest + 1) to finest) ( ) : Unit {")
    if (Knowledge.genTimersForComm)
      printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
    printer.println(s"\tcommunicate Solution$postfix@current")
    if (Knowledge.genTimersForComm)
      printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
    if (Knowledge.testFragLoops)
      printer.println(s"\tloop over fragments {")
    printer.println(s"\tloop over Solution$postfix@current {")
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} += CorrectionStencil@current * ToFiner ( ${solutionFields(s"coarser", postfix)(vecDim)} )")
    printer.println(s"\t}")
    if (Knowledge.testFragLoops)
      printer.println(s"\t}")
    printer.println(s"}")
    printer.println
  }

  def addUtilFunctions(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"def SetSolution$postfix@all (value : Real) : Unit {")
    printer.println(s"\tloop over Solution$postfix@current {")
    for (vecDim <- 0 until Knowledge.numVecDims)
      printer.println(s"\t\t${solutionFields(s"current", postfix)(vecDim)} = value")
    printer.println(s"\t}")
    printer.println(s"}")

    for (vecDim <- 0 until Knowledge.numVecDims) {
      printer.println(s"def L2Residual${postfix}_$vecDim@(coarsest and finest) ( ) : Real {")
      if (Knowledge.genTimersForComm)
        printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
      printer.println(s"\tcommunicate Residual$postfix@current")
      if (Knowledge.genTimersForComm)
        printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
      printer.println(s"\tvar res : Real = 0")
      if (Knowledge.testFragLoops)
        printer.println(s"\tloop over fragments with reduction( + : res ) {")
      printer.println(s"\tloop over Residual$postfix@current with reduction( + : res ) {")
      printer.println(s"\t\t// FIXME: this counts duplicated values multiple times")
      printer.println(s"\t\tres += ${residualFields(s"current", postfix)(vecDim)} * ${residualFields(s"current", postfix)(vecDim)}")
      printer.println(s"\t}")
      if (Knowledge.testFragLoops)
        printer.println(s"\t}")
      printer.println(s"\treturn ( sqrt ( res ) )")
      printer.println(s"}")
      printer.println
    }
  }

  def addInitFields(printer : java.io.PrintWriter, postfix : String) = {
    printer.println(s"def InitSolution$postfix ( ) : Unit {")
    if (Knowledge.initSolWithRand) {
      // FIXME: this loop needs to be marked as non-parallelizable somehow
      // FIXME: make results more reproducible via sth like 'std::srand((unsigned int)fragments[f]->id)'
      printer.println(s"\tloop over Solution$postfix@finest {")
      for (vecDim <- 0 until Knowledge.numVecDims) {
        printer.println(s"\t\t${solutionFields(s"finest", postfix)(vecDim)} = native('((double)std::rand()/RAND_MAX)')")
      }
      printer.println(s"\t}")
    } else {
      printer.println(s"\tloop over Solution$postfix@finest {")
      for (vecDim <- 0 until Knowledge.numVecDims) {
        printer.println(s"\t\t${solutionFields(s"finest", postfix)(vecDim)} = 0")
      }
      printer.println(s"\t}")
    }

    printer.println(s"}")

    printer.println(s"def InitRHS$postfix ( ) : Unit {")
    printer.println(s"\tloop over RHS$postfix@finest {")
    for (vecDim <- 0 until Knowledge.numVecDims) {
      printer.println(s"\t\t${rhsFields(s"finest", postfix)(vecDim)} = 0")
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
        if (Knowledge.genTimersForComm)
          printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
        printer.println(s"\tcommunicate LaplaceCoeff$postfix@finer")
        if (Knowledge.genTimersForComm)
          printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
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

  def addSolveFunction(printer : java.io.PrintWriter) = {
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
      if (Knowledge.genTimersForComm)
        printer.println(s"\tstartTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")
      printer.println(s"\tcommunicate RHS_GMRF@finest")
      if (Knowledge.genTimersForComm)
        printer.println(s"\tstopTimer ( commTimer${if (Knowledge.genCommTimersPerLevel) "@current" else ""} )")

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
    addDomains(printer)

    // Layouts
    addLayouts(printer)

    // Fields
    if (Knowledge.kelvin) {
      addFields(printer, "", "innerDom")
      addFields(printer, "_GMRF", "global")
    } else {
      addFields(printer, "", if (Knowledge.testDomainEmbedding) "innerDom" else "global")
    }

    if (Knowledge.kelvin) {
      PolyOpt.registerSideeffectFree("bcSol")
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
    if (Knowledge.kelvin) {
      addStencilFields(printer, "", "innerDom")
      addStencilFields(printer, "_GMRF", "global")
    } else {
      addStencilFields(printer, "", if (Knowledge.testDomainEmbedding) "innerDom" else "global")
    }

    // External Fields
    if (Knowledge.testExtFields) {
      printer.println("external Field extSolution <ExtSolLayout> => Solution@(finest)")
      printer.println
    }

    // Stencils
    addLaplaceStencil(printer, "")
    addLaplaceStencil(printer, "_GMRF")
    addDefaultStencils(printer)

    // Iteration Sets
    // addIterationSets(printer)

    // Globals
    addGlobals(printer)

    // CGS
    addCGS(printer, "")
    if (Knowledge.kelvin)
      addCGS(printer, "_GMRF")

    // Cycle
    addCycle(printer, "")
    if (Knowledge.kelvin)
      addCycle(printer, "_GMRF")

    // Smoother
    addSmoother(printer, "")
    if (Knowledge.kelvin)
      addSmoother(printer, "_GMRF")

    // Other MG Functions
    addUpResidual(printer, "")
    if (Knowledge.kelvin)
      addUpResidual(printer, "_GMRF")

    addRestriction(printer, "")
    if (Knowledge.kelvin)
      addRestriction(printer, "_GMRF")

    addCorrection(printer, "")
    if (Knowledge.kelvin)
      addCorrection(printer, "_GMRF")

    // Util Functions
    addUtilFunctions(printer, "")
    if (Knowledge.kelvin)
      addUtilFunctions(printer, "_GMRF")

    // initField functions
    addInitFields(printer, "")
    if (Knowledge.kelvin)
      addInitFields(printer, "_GMRF")

    // Solver
    addSolveFunction(printer)

    // Application
    printer.println("def Application ( ) : Unit {")

    printer.println("\tvar setupTime : Real = 0")
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
      printer.println("\tvar timeSamples : Real = 0")
      printer.println("\tstartTimer ( timeSamplesWatch )")
      printer.println(s"\trepeat up ${Knowledge.numSamples} {")
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
      printer.println(s"\t\tSolutionMean@finest += ${solutionFields(s"finest", "")(0)}")
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

      printer.println(s"\tvar solNorm : Real = 0.0")
      printer.println(s"\tloop over SolutionMean@finest with reduction( + : solNorm ) {")
      printer.println(s"\t\t// FIXME: this counts duplicated values multiple times")
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

    printer.close()
  }
}
