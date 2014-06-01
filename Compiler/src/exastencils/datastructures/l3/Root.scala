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
  var testBC : Boolean = false // NOTE: the tested bc will only be reasonable for 2D cases
  var testExtFields : Boolean = false
  var printFieldAtEnd : Boolean = false
  var genSetableStencil : Boolean = false
  var omegaViaGlobals : Boolean = false
  var initSolWithRand : Boolean = !testBC
  var genRBSetsWithConditions : Boolean = true

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
    if (testBC) {
      printer.println("Field Solution< Real, global, BasicComm, 0.0 >@(coarsest to (finest - 1))")
      printer.println("Field Solution< Real, global, BasicComm, sin ( M_PI * xPos ) * sinh ( M_PI * yPos ) >@finest")
      if ("Jac" == smoother) {
        printer.println("Field Solution2< Real, global, BasicComm, 0.0 >@(coarsest to (finest - 1))")
        printer.println("Field Solution2< Real, global, BasicComm, sin ( M_PI * xPos ) * sinh ( M_PI * yPos ) >@finest")
      }
    } else {
      printer.println("Field Solution< Real, global, BasicComm, 0 >@all")
      if ("Jac" == smoother)
        printer.println("Field Solution2< Real, global, BasicComm, 0 >@all")
    }
    printer.println("Field Residual< Real, global, BasicComm, None >@all")
    printer.println("Field RHS< Real, global, NoComm, None >@all")
    if ("CG" == cgs) {
      printer.println("Field VecP< Real, global, BasicComm, None >@coarsest")
      printer.println("Field VecGradP< Real, global, NoComm, None >@coarsest")
    }
    printer.println

    // External Fields
    if (testExtFields) {
      printer.println("external Field extSolution <ExtSolLayout> => Solution@(finest)")
      printer.println
    }

    // Stencils
    if (genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("Stencil Lapl@all {")
          printer.println("\t[ 0,  0] => Lapl_Coeff_0_0")
          printer.println("\t[ 1,  0] => Lapl_Coeff_P1_0")
          printer.println("\t[-1,  0] => Lapl_Coeff_N1_0")
          printer.println("\t[ 0,  1] => Lapl_Coeff_0_P1")
          printer.println("\t[ 0, -1] => Lapl_Coeff_0_N1")
          printer.println("}")
        }
        case 3 =>
          printer.println("Stencil Lapl@all {")
          printer.println("\t[ 0,  0,  0] => Lapl_Coeff_0_0_0")
          printer.println("\t[ 1,  0,  0] => Lapl_Coeff_P1_0_0")
          printer.println("\t[-1,  0,  0] => Lapl_Coeff_N1_0_0")
          printer.println("\t[ 0,  1,  0] => Lapl_Coeff_0_P1_0")
          printer.println("\t[ 0, -1,  0] => Lapl_Coeff_0_N1_0")
          printer.println("\t[ 0,  0,  1] => Lapl_Coeff_0_0_P1")
          printer.println("\t[ 0,  0, -1] => Lapl_Coeff_0_0_N1")
          printer.println("}")
      }
    } else {
      Knowledge.dimensionality match {
        case 2 => {
          printer.println("Stencil Lapl@all {")
          printer.println("\t[ 0,  0] => 4")
          printer.println("\t[ 1,  0] => -1")
          printer.println("\t[-1,  0] => -1")
          printer.println("\t[ 0,  1] => -1")
          printer.println("\t[ 0, -1] => -1")
          printer.println("}")
        }
        case 3 =>
          printer.println("Stencil Lapl@all {")
          printer.println("\t[ 0,  0,  0] => 6")
          printer.println("\t[ 1,  0,  0] => -1")
          printer.println("\t[-1,  0,  0] => -1")
          printer.println("\t[ 0,  1,  0] => -1")
          printer.println("\t[ 0, -1,  0] => -1")
          printer.println("\t[ 0,  0,  1] => -1")
          printer.println("\t[ 0,  0, -1] => -1")
          printer.println("}")
      }
    }

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
    printer.println("""def VCycle@coarsest ( ) : Unit {
	UpResidual@(current) ( )
	communicate Residual@(current)

	var res : Real = L2Residual@(current) ( )
	var initialRes : Real = res

	loop over inner on VecP@(current) {
		VecP@(current) = Residual@(current)
	}

	repeat up 512 {
		communicate VecP@(current)

		loop over inner on VecP@(current) {
			VecGradP@(current) = Lapl@(current) * VecP@(current)
		}

		var alphaDenom : Real = 0
		loop over inner on VecP@(current) with reduction( + : alphaDenom ) {
			alphaDenom += VecP@(current) * VecGradP@(current)
		}

		var alpha : Real = res * res / alphaDenom

		loop over inner on Solution@(current) {
			Solution@(current) += alpha * VecP@(current)
			Residual@(current) -= alpha * VecGradP@(current)
		}

		var nextRes : Real = L2Residual@(current) ( )

		if ( nextRes <= 0.001 * initialRes ) {
			return ( )
		}

		var beta : Real = (nextRes * nextRes) / (res * res)

		loop over inner on VecP@(current) {
			VecP@(current) = Residual@(current) + beta * VecP@(current)
		}

		res = nextRes
	}
}""")
    printer.println

    // Cycle
    if ("Jac" == smoother) {
      numPre /= 2
      numPost /= 2
    }
    printer.println("def VCycle@((coarsest + 1) to finest) ( ) : Unit {")
    printer.println(s"	repeat up $numPre {")
    printer.println("		Smoother@(current) ( )")
    printer.println("	}")
    printer.println("	UpResidual@(current) ( )")
    printer.println("	Restrict@(current) ( )")
    printer.println("	set@(coarser) ( 0 )")
    printer.println("	VCycle@(coarser) ( )")
    printer.println("	interpolatecorr@(current) ( )")
    printer.println(s"	repeat up $numPost {")
    printer.println("		Smoother@(current) ( )")
    printer.println("	}")
    printer.println("}")
    printer.println

    // Smoother
    val omegaToPrint = (if (omegaViaGlobals) "omega" else omega)
    smoother match {
      case "Jac" => {
        printer.println("def Smoother@((coarsest + 1) to finest) ( ) : Unit {")
        printer.println("\tcommunicate Solution@(current)")
        printer.println("\tloop over inner on Solution@(current) {")
        printer.println(s"\t\tSolution2@(current) = Solution@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * $omegaToPrint ) * ( RHS@(current) - Lapl@(current) * Solution@(current) ) )")
        printer.println("\t}")
        printer.println("\tcommunicate Solution2@(current)")
        printer.println("\tloop over inner on Solution@(current) {")
        printer.println(s"\t\tSolution@(current) = Solution2@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * $omegaToPrint ) * ( RHS@(current) - Lapl@(current) * Solution2@(current) ) )")
        printer.println("\t}")
        printer.println("}")
      }
      case "RBGS" => {
        printer.println("def Smoother@((coarsest + 1) to finest) ( ) : Unit {")
        printer.println("\tcommunicate Solution@(current)")
        printer.println("\tloop over red on Solution@(current) {")
        printer.println(s"\t\tSolution@(current) = Solution@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * $omegaToPrint ) * ( RHS@(current) - Lapl@(current) * Solution@(current) ) )")
        printer.println("\t}")
        printer.println("\tcommunicate Solution@(current)")
        printer.println("\tloop over black on Solution@(current) {")
        printer.println(s"\t\tSolution@(current) = Solution@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * $omegaToPrint ) * ( RHS@(current) - Lapl@(current) * Solution@(current) ) )")
        printer.println("\t}")
        printer.println("}")
      }
      case "GS" => {
        printer.println("def Smoother@((coarsest + 1) to finest) ( ) : Unit {")
        printer.println("\tcommunicate Solution@(current)")
        printer.println("\tloop over inner on Solution@(current) {")
        printer.println(s"\t\tSolution@(current) = Solution@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * $omegaToPrint ) * ( RHS@(current) - Lapl@(current) * Solution@(current) ) )")
        printer.println("\t}")
        printer.println("}")
      }
    }
    printer.println

    // Other MG Functions
    printer.println("""def UpResidual@all ( ) : Unit {
	communicate Solution@(current)
	loop over inner on Residual@(current) {
		Residual@(current) = RHS@(current) - (Lapl@(current) * Solution@(current))
	}
}""")

    printer.println("""def Restrict @((coarsest + 1) to finest) ( ) : Unit { 
	communicate Residual@(current)
	loop over innerForFieldsWithoutGhostLayers on RHS@(coarser) {
		RHS@(coarser) = ToCoarser ( RestrictionStencil@(current) * Residual@(current) )
    }
}""")

    printer.println("""def interpolatecorr@((coarsest + 1) to finest) ( ) : Unit { 
	communicate Solution@(current)
	loop over inner on Solution@(current) {
		Solution@(current) += ToFiner ( CorrectionStencil@(current) * Solution@(coarser) )
	}
}""")
    printer.println

    // Util Functions
    printer.println("""
def set@all (value : Real) : Unit {
	loop over domain on Solution@(current) {
		Solution@(current) = value
	}
}""")

    printer.println("""def L2Residual@(coarsest and finest) ( ) : Real {
	communicate Residual@(current)
	var res : Real = 0
	loop over inner on Residual@(current) with reduction( + : res ) {
		// FIXME: this counts duplicated values multiple times
		res += Residual@(current) * Residual@(current)
	}
	return ( sqrt ( res ) )
}""")
    printer.println

    // initField functions
    printer.println("def initSolution ( ) : Unit {")
    if (initSolWithRand) {
      // FIXME: this loop needs to be marked as non-parallelizable somehow
      // FIXME: make results more reproducible via sth like 'std::srand((unsigned int)fragments[f]->id)'
      printer.println("\tloop over inner on Solution@finest {")
      printer.println("\t\tSolution@finest = native('((double)std::rand()/RAND_MAX)')")
      printer.println("\t}")
    } else {
      printer.println("\tloop over inner on Solution@finest {")
      printer.println("\t\tSolution@finest = 0")
      printer.println("\t}")
    }
    printer.println("}")

    printer.println("def initRHS ( ) : Unit {")
    printer.println("\tloop over innerForFieldsWithoutGhostLayers on RHS@finest {")
    printer.println("\t\tRHS@finest = 0")
    printer.println("\t}")
    printer.println("}")
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
    printer.println("""
	initRHS ( )
	initSolution ( )
	UpResidual@finest ( )
	var res0 : Real = L2Residual@finest (  )
	var res : Real = res0
	var resold : Real = 0
	print ( '"startingres"', res0 )
	var totalTime : Real = 0
	var timeToSolve : Real = 0
	startTimer ( timeToSolveWatch )
	repeat up 10 {
		resold = res
		startTimer ( stopWatch )
		VCycle@finest (  )
		UpResidual@finest ( )
		stopTimer ( stopWatch, totalTime )
		res = L2Residual@finest (  )
		print ( '"Residual:"', res, '"Residual reduction:"', ( res0 / res ), '"Convergence factor:"', ( res / resold ) )
	}
	stopTimer ( timeToSolveWatch, timeToSolve )
	print ( '"Total time to solve: "', timeToSolve )
	print ( '"Mean time per vCycle: "', totalTime / 10 )
        """)

    if (printFieldAtEnd)
      printer.println("printField('\"Solution.dat\"', Solution@finest)")
    printer.println("}")
    printer.println

    printer.close()
  }
}
