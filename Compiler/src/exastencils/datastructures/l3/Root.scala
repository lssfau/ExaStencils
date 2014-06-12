package exastencils.datastructures.l3

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.multiGrid._

case class Root() extends Node {
  var smoother : String = "Jac" // Jac | GS | RBGS
  var cgs : String = "CG" // CG
  var numPre : Int = 2 // has to be divisible by 2 for Jac
  var numPost : Int = 4 // has to be divisible by 2 for Jac
  var omega : Double = (if ("Jac" == smoother) 0.8 else 1.0)
  var testBC : Boolean = true // NOTE: the tested bc will only be reasonable for 2D cases
  var testExtFields : Boolean = false
  var printFieldAtEnd : Boolean = false
  var genSetableStencil : Boolean = false
  var omegaViaGlobals : Boolean = false
  var initSolWithRand : Boolean = !testBC
  var genRBSetsWithConditions : Boolean = true
  var useVecFields : Boolean = false // attempts to solve Poisson's equation for (numVecDims)D vectors; atm all three components are solved independently
  var numVecDims = (if (useVecFields) 2 else 1)
  var genStencilFields : Boolean = false
  var useSlotsForJac : Boolean = true

  def solutionFields(level : String) = {
    if (useVecFields)
      (0 until numVecDims).toArray.map(d => s"Solution${if (useSlotsForJac) "[0]" else ""}@$level[$d]")
    else
      Array(s"Solution${if (useSlotsForJac) "[0]" else ""}@$level")
  }
  def solution2Fields(level : String) = {
    if (useVecFields)
      (0 until numVecDims).toArray.map(d => s"Solution${if (useSlotsForJac) "[1]" else "2"}@$level[$d]")
    else
      Array(s"Solution${if (useSlotsForJac) "[1]" else "2"}@$level")
  }
  def residualFields(level : String) = {
    if (useVecFields)
      (0 until numVecDims).toArray.map(d => s"Residual@$level[$d]")
    else
      Array(s"Residual@$level")
  }
  def rhsFields(level : String) = {
    if (useVecFields)
      (0 until numVecDims).toArray.map(d => s"RHS@$level[$d]")
    else
      Array(s"RHS@$level")
  }

  def printToL4(filename : String) : Unit = {
    var printer = new java.io.PrintWriter(filename)

    // Domains
    Knowledge.dimensionality match {
      case 2 => {
        printer.println("Domain global< [ 0, 0 ] to [ 1, 1 ] >")
      }
      case 3 => {
        printer.println("Domain global< [ 0, 0, 0 ] to [ 1, 1, 1 ] >")
      }
    }
    printer.println

    // Layouts
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

    // Fields
    var fieldDatatype = (if (useVecFields) s"Array[Real][$numVecDims]" else "Real")
    if (testBC) {
      if ("Jac" == smoother) {
        if (useSlotsForJac) {
          printer.println(s"Field Solution< $fieldDatatype, global, BasicComm, 0.0 >[2]@(coarsest to (finest - 1))")
          printer.println(s"Field Solution< $fieldDatatype, global, BasicComm, sin ( M_PI * xPos ) * sinh ( M_PI * yPos ) >[2]@finest")
        } else {
          printer.println(s"Field Solution< $fieldDatatype, global, BasicComm, 0.0 >@(coarsest to (finest - 1))")
          printer.println(s"Field Solution< $fieldDatatype, global, BasicComm, sin ( M_PI * xPos ) * sinh ( M_PI * yPos ) >@finest")
          printer.println(s"Field Solution2< $fieldDatatype, global, BasicComm, 0.0 >@(coarsest to (finest - 1))")
          printer.println(s"Field Solution2< $fieldDatatype, global, BasicComm, sin ( M_PI * xPos ) * sinh ( M_PI * yPos ) >@finest")
        }
      } else {
        printer.println(s"Field Solution< $fieldDatatype, global, BasicComm, 0.0 >@(coarsest to (finest - 1))")
        printer.println(s"Field Solution< $fieldDatatype, global, BasicComm, sin ( M_PI * xPos ) * sinh ( M_PI * yPos ) >@finest")
      }
    } else {
      if ("Jac" == smoother) {
        if (useSlotsForJac) {
          printer.println(s"Field Solution< $fieldDatatype, global, BasicComm, 0.0 >[2]@all")
        } else {
          printer.println(s"Field Solution< $fieldDatatype, global, BasicComm, 0.0 >@all")
          printer.println(s"Field Solution2< $fieldDatatype, global, BasicComm, 0.0 >@all")
        }
      } else {
        printer.println(s"Field Solution< $fieldDatatype, global, BasicComm, 0.0 >@all")
      }
    }
    printer.println(s"Field Residual< $fieldDatatype, global, BasicComm, None >@all")
    printer.println(s"Field RHS< $fieldDatatype, global, NoComm, None >@all")
    if ("CG" == cgs) {
      printer.println(s"Field VecP< $fieldDatatype, global, BasicComm, None >@coarsest")
      printer.println(s"Field VecGradP< $fieldDatatype, global, NoComm, None >@coarsest")
    }
    printer.println

    // Coeff/StencilFields
    if (genStencilFields) {
      printer.println(s"Field LaplCoeff< Array[Real][${2 * Knowledge.dimensionality + 1}], global, NoComm, None >@all")
      printer.println(s"StencilField Lapl< LaplCoeff => LaplStencil >@all")
      printer.println
    }

    // External Fields
    if (testExtFields) {
      printer.println("external Field extSolution <ExtSolLayout> => Solution@(finest)")
      printer.println
    }

    // Stencils
    if (genStencilFields)
      printer.println("Stencil LaplStencil@all {")
    else
      printer.println("Stencil Lapl@all {")
    if (genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("\t[ 0,  0] => Lapl_Coeff_0_0")
          printer.println("\t[ 1,  0] => Lapl_Coeff_P1_0")
          printer.println("\t[-1,  0] => Lapl_Coeff_N1_0")
          printer.println("\t[ 0,  1] => Lapl_Coeff_0_P1")
          printer.println("\t[ 0, -1] => Lapl_Coeff_0_N1")
        }
        case 3 =>
          printer.println("\t[ 0,  0,  0] => Lapl_Coeff_0_0_0")
          printer.println("\t[ 1,  0,  0] => Lapl_Coeff_P1_0_0")
          printer.println("\t[-1,  0,  0] => Lapl_Coeff_N1_0_0")
          printer.println("\t[ 0,  1,  0] => Lapl_Coeff_0_P1_0")
          printer.println("\t[ 0, -1,  0] => Lapl_Coeff_0_N1_0")
          printer.println("\t[ 0,  0,  1] => Lapl_Coeff_0_0_P1")
          printer.println("\t[ 0,  0, -1] => Lapl_Coeff_0_0_N1")
      }
    } else {
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("\t[ 0,  0] => 4")
          printer.println("\t[ 1,  0] => -1")
          printer.println("\t[-1,  0] => -1")
          printer.println("\t[ 0,  1] => -1")
          printer.println("\t[ 0, -1] => -1")
        }
        case 3 =>
          printer.println("\t[ 0,  0,  0] => 6")
          printer.println("\t[ 1,  0,  0] => -1")
          printer.println("\t[-1,  0,  0] => -1")
          printer.println("\t[ 0,  1,  0] => -1")
          printer.println("\t[ 0, -1,  0] => -1")
          printer.println("\t[ 0,  0,  1] => -1")
          printer.println("\t[ 0,  0, -1] => -1")
      }
    }
    printer.println("}")

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

    // Iteration Sets
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

    // Globals
    printer.println("Globals {")
    if (omegaViaGlobals)
      printer.println(s"\tvar omega : Real = $omega")
    if (genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("\tvar Lapl_Coeff_0_0 : Real")
          printer.println("\tvar Lapl_Coeff_P1_0 : Real")
          printer.println("\tvar Lapl_Coeff_N1_0 : Real")
          printer.println("\tvar Lapl_Coeff_0_P1 : Real")
          printer.println("\tvar Lapl_Coeff_0_N1 : Real")
        }
        case 3 => {
          printer.println("\tvar Lapl_Coeff_0_0_0 : Real")
          printer.println("\tvar Lapl_Coeff_P1_0_0 : Real")
          printer.println("\tvar Lapl_Coeff_N1_0_0 : Real")
          printer.println("\tvar Lapl_Coeff_0_P1_0 : Real")
          printer.println("\tvar Lapl_Coeff_0_N1_0 : Real")
          printer.println("\tvar Lapl_Coeff_0_0_P1 : Real")
          printer.println("\tvar Lapl_Coeff_0_0_N1 : Real")
        }
      }
    }
    printer.println("}")
    printer.println

    // CGS
    printer.println(s"def VCycle@coarsest ( ) : Unit {")
    for (vecDim <- 0 until numVecDims)
      printer.println(s"\tVCycle_$vecDim@(current) ( )")
    printer.println(s"}")

    for (vecDim <- 0 until numVecDims) {
      printer.println(s"def VCycle_$vecDim@coarsest ( ) : Unit {")
      printer.println(s"\tUpResidual@(current) ( )")
      printer.println(s"\tcommunicate Residual@(current)")

      printer.println(s"\tvar res : Real = L2Residual_$vecDim@(current) ( )")
      printer.println(s"\tvar initialRes : Real = res")

      printer.println(s"\tloop over inner on VecP@(current) {")
      printer.println(s"\t\tVecP@(current) = ${residualFields("current")(vecDim)}")
      printer.println(s"\t}")

      printer.println(s"\trepeat up 512 {")
      printer.println(s"\t\tcommunicate VecP@(current)")

      printer.println(s"\t\tloop over inner on VecP@(current) {")
      printer.println(s"\t\t\tVecGradP@(current) = Lapl@(current) * VecP@(current)")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar alphaDenom : Real = 0")
      printer.println(s"\t\tloop over inner on VecP@(current) with reduction( + : alphaDenom ) {")
      printer.println(s"\t\t\talphaDenom += VecP@(current) * VecGradP@(current)")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar alpha : Real = res * res / alphaDenom")

      printer.println(s"\t\tloop over inner on Solution@(current) {")
      printer.println(s"\t\t\t${solutionFields("current")(vecDim)} += alpha * VecP@(current)")
      printer.println(s"\t\t\t${residualFields("current")(vecDim)} -= alpha * VecGradP@(current)")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar nextRes : Real = L2Residual_$vecDim@(current) ( )")

      printer.println(s"\t\tif ( nextRes <= 0.001 * initialRes ) {")
      printer.println(s"\t\t\treturn ( )")
      printer.println(s"\t\t}")

      printer.println(s"\t\tvar beta : Real = (nextRes * nextRes) / (res * res)")

      printer.println(s"\t\tloop over inner on VecP@(current) {")
      printer.println(s"\t\t\tVecP@(current) = ${residualFields("current")(vecDim)} + beta * VecP@(current)")
      printer.println(s"\t\t}")

      printer.println(s"\t\tres = nextRes")
      printer.println(s"\t}")
      printer.println(s"}")
    }
    printer.println

    // Cycle
    if ("Jac" == smoother) {
      numPre /= 2
      numPost /= 2
    }
    printer.println("def VCycle@((coarsest + 1) to finest) ( ) : Unit {")
    printer.println(s"\trepeat up $numPre {")
    printer.println("\t\tSmoother@(current) ( )")
    printer.println("\t}")
    printer.println("\tUpResidual@(current) ( )")
    printer.println("\tRestrict@(current) ( )")
    printer.println("\tset@(coarser) ( 0 )")
    printer.println("\tVCycle@(coarser) ( )")
    printer.println("\tinterpolatecorr@(current) ( )")
    printer.println(s"\trepeat up $numPost {")
    printer.println("\t\tSmoother@(current) ( )")
    printer.println("\t}")
    printer.println("}")
    printer.println

    // Smoother
    val omegaToPrint = (if (omegaViaGlobals) "omega" else omega)
    smoother match {
      case "Jac" => {
        printer.println(s"def Smoother@((coarsest + 1) to finest) ( ) : Unit {")
        printer.println(s"\tcommunicate Solution${if (useSlotsForJac) "[0]" else ""}@(current)")
        printer.println(s"\tloop over inner on Solution@(current) {")
        for (vecDim <- 0 until numVecDims)
          printer.println(s"\t\t${solution2Fields("current")(vecDim)} = ${solutionFields("current")(vecDim)} + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * $omegaToPrint ) * ( ${rhsFields("current")(vecDim)} - Lapl@(current) * ${solutionFields("current")(vecDim)} ) )")
        printer.println(s"\t}")
        printer.println(s"\tcommunicate Solution${if (useSlotsForJac) "[1]" else "2"}@(current)")
        printer.println(s"\tloop over inner on Solution@(current) {")
        for (vecDim <- 0 until numVecDims)
          printer.println(s"\t\t${solutionFields("current")(vecDim)} = ${solution2Fields("current")(vecDim)} + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * $omegaToPrint ) * ( ${rhsFields("current")(vecDim)} - Lapl@(current) * ${solution2Fields("current")(vecDim)} ) )")
        printer.println(s"\t}")
        printer.println(s"}")
      }
      case "RBGS" => {
        printer.println("def Smoother@((coarsest + 1) to finest) ( ) : Unit {")
        printer.println("\tcommunicate Solution@(current)")
        printer.println("\tloop over red on Solution@(current) {")
        for (vecDim <- 0 until numVecDims)
          printer.println(s"\t\tSolution@(current) = ${solutionFields("current")(vecDim)} + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * $omegaToPrint ) * ( ${rhsFields("current")(vecDim)} - Lapl@(current) * ${solutionFields("current")(vecDim)} ) )")
        printer.println("\t}")
        printer.println("\tcommunicate Solution@(current)")
        printer.println("\tloop over black on Solution@(current) {")
        for (vecDim <- 0 until numVecDims)
          printer.println(s"\t\tSolution@(current) = ${solutionFields("current")(vecDim)} + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * $omegaToPrint ) * ( ${rhsFields("current")(vecDim)} - Lapl@(current) * ${solutionFields("current")(vecDim)} ) )")
        printer.println("\t}")
        printer.println("}")
      }
      case "GS" => {
        printer.println("def Smoother@((coarsest + 1) to finest) ( ) : Unit {")
        printer.println("\tcommunicate Solution@(current)")
        printer.println("\tloop over inner on Solution@(current) {")
        for (vecDim <- 0 until numVecDims)
          printer.println(s"\t\tSolution@(current) = ${solutionFields("current")(vecDim)} + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * $omegaToPrint ) * ( ${rhsFields("current")(vecDim)} - Lapl@(current) * ${solutionFields("current")(vecDim)} ) )")
        printer.println("\t}")
        printer.println("}")
      }
    }
    printer.println

    // Other MG Functions
    printer.println("def UpResidual@all ( ) : Unit {")
    printer.println("\tcommunicate Solution@(current)")
    printer.println("\tloop over inner on Residual@(current) {")
    for (vecDim <- 0 until numVecDims)
      printer.println(s"\t\t${residualFields("current")(vecDim)} = ${rhsFields("current")(vecDim)} - (Lapl@(current) * ${solutionFields("current")(vecDim)})")
    printer.println("\t}")
    printer.println("}")

    printer.println("def Restrict @((coarsest + 1) to finest) ( ) : Unit {")
    printer.println("\tcommunicate Residual@(current)")
    printer.println("\tloop over innerForFieldsWithoutGhostLayers on RHS@(coarser) {")
    for (vecDim <- 0 until numVecDims)
      printer.println(s"\t\t${rhsFields("coarser")(vecDim)} = ToCoarser ( RestrictionStencil@(current) * ${residualFields("current")(vecDim)} )")
    printer.println("\t}")
    printer.println("}")

    printer.println("def interpolatecorr@((coarsest + 1) to finest) ( ) : Unit {")
    printer.println("\tcommunicate Solution@(current)")
    printer.println("\tloop over inner on Solution@(current) {")
    for (vecDim <- 0 until numVecDims)
      printer.println(s"\t\t${solutionFields("current")(vecDim)} += ToFiner ( CorrectionStencil@(current) * ${solutionFields("coarser")(vecDim)} )")
    printer.println("\t}")
    printer.println("}")
    printer.println

    // Util Functions
    printer.println("def set@all (value : Real) : Unit {")
    printer.println("\tloop over domain on Solution@(current) {")
    for (vecDim <- 0 until numVecDims)
      printer.println(s"\t\t${solutionFields("current")(vecDim)} = value")
    printer.println("\t}")
    printer.println("}")

    for (vecDim <- 0 until numVecDims) {
      printer.println(s"def L2Residual_$vecDim@(coarsest and finest) ( ) : Real {")
      printer.println("\tcommunicate Residual@(current)")
      printer.println("\tvar res : Real = 0")
      printer.println("\tloop over inner on Residual@(current) with reduction( + : res ) {")
      printer.println("\t\t// FIXME: this counts duplicated values multiple times")
      printer.println(s"\t\tres += ${residualFields("current")(vecDim)} * ${residualFields("current")(vecDim)}")
      printer.println("\t}")
      printer.println("\treturn ( sqrt ( res ) )")
      printer.println("}")
      printer.println
    }

    // initField functions
    printer.println("def initSolution ( ) : Unit {")
    if (initSolWithRand) {
      // FIXME: this loop needs to be marked as non-parallelizable somehow
      // FIXME: make results more reproducible via sth like 'std::srand((unsigned int)fragments[f]->id)'
      printer.println(s"\tloop over inner on Solution@finest {")
      for (vecDim <- 0 until numVecDims) {
        printer.println(s"\t\t${solutionFields("finest")(vecDim)} = native('((double)std::rand()/RAND_MAX)')")
      }
      printer.println(s"\t}")
    } else {
      printer.println(s"\tloop over inner on Solution@finest {")
      for (vecDim <- 0 until numVecDims) {
        printer.println(s"\t\t${solutionFields("finest")(vecDim)} = 0")
      }
      printer.println(s"\t}")
    }

    printer.println("}")

    printer.println("def initRHS ( ) : Unit {")
    printer.println(s"\tloop over innerForFieldsWithoutGhostLayers on RHS@finest {")
    for (vecDim <- 0 until numVecDims) {
      printer.println(s"\t\t${rhsFields("finest")(vecDim)} = 0")
    }
    printer.println(s"\t}")
    printer.println("}")

    if (genStencilFields) {
      printer.println("def initLapl@all ( ) : Unit {")
      printer.println("\tloop over innerForFieldsWithoutGhostLayers on LaplCoeff@current {")
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("\t\tLaplCoeff@current[0] = 4")
          printer.println("\t\tLaplCoeff@current[1] = -1")
          printer.println("\t\tLaplCoeff@current[2] = -1")
          printer.println("\t\tLaplCoeff@current[3] = -1")
          printer.println("\t\tLaplCoeff@current[4] = -1")
          //    printer.println("\t\tLaplCoeff@current[ 0,  0] = 4")
          //    printer.println("\t\tLaplCoeff@current[ 1,  0] = -1")
          //    printer.println("\t\tLaplCoeff@current[-1,  0] = -1")
          //    printer.println("\t\tLaplCoeff@current[ 0,  1] = -1")
          //    printer.println("\t\tLaplCoeff@current[ 0, -1] = -1")
        }
        case 3 => {
          printer.println("\t\tLaplCoeff@current[0] = 6")
          printer.println("\t\tLaplCoeff@current[1] = -1")
          printer.println("\t\tLaplCoeff@current[2] = -1")
          printer.println("\t\tLaplCoeff@current[3] = -1")
          printer.println("\t\tLaplCoeff@current[4] = -1")
          printer.println("\t\tLaplCoeff@current[5] = -1")
          printer.println("\t\tLaplCoeff@current[6] = -1")
          //    printer.println("\t\tLaplCoeff@current[ 0,  0,  0] = 6")
          //    printer.println("\t\tLaplCoeff@current[ 1,  0,  0] = -1")
          //    printer.println("\t\tLaplCoeff@current[-1,  0,  0] = -1")
          //    printer.println("\t\tLaplCoeff@current[ 0,  1,  0] = -1")
          //    printer.println("\t\tLaplCoeff@current[ 0, -1,  0] = -1")
          //    printer.println("\t\tLaplCoeff@current[ 0,  0,  1] = -1")
          //    printer.println("\t\tLaplCoeff@current[ 0,  0, -1] = -1")
        }
      }
      printer.println("\t}")
      printer.println("}")
    }

    printer.println

    // Application
    printer.println("def Application ( ) : Unit {")

    if (genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("\tLapl_Coeff_0_0 = -4")
          printer.println("\tLapl_Coeff_P1_0 = 1")
          printer.println("\tLapl_Coeff_N1_0 = 1")
          printer.println("\tLapl_Coeff_0_P1 = 1")
          printer.println("\tLapl_Coeff_0_N1 = 1")
        }
        case 3 => {
          printer.println("\tLapl_Coeff_0_0_0 = -6")
          printer.println("\tLapl_Coeff_P1_0_0 = 1")
          printer.println("\tLapl_Coeff_N1_0_0 = 1")
          printer.println("\tLapl_Coeff_0_P1_0 = 1")
          printer.println("\tLapl_Coeff_0_N1_0 = 1")
          printer.println("\tLapl_Coeff_0_0_P1 = 1")
          printer.println("\tLapl_Coeff_0_0_N1 = 1")
        }
      }
    }

    if (genStencilFields) {
      for (lvl <- 0 to Knowledge.maxLevel)
        printer.println(s"\tinitLapl@$lvl ( )")
    }

    printer.println("\tinitRHS ( )")
    printer.println("\tinitSolution ( )")
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

    if (printFieldAtEnd)
      printer.println("printField('\"Solution.dat\"', Solution@finest)")
    printer.println("}")
    printer.println

    printer.close()
  }
}
