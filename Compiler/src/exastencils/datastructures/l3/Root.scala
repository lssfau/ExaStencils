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
  var testBC : Boolean = true
  var testExtFields : Boolean = false
  var printFieldAtEnd : Boolean = false

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
        printer.println("""Layout BasicComm {
	ghostLayers = [ 1, 1 ] with communication
	duplicateLayers = [ 1, 1 ] with communication
	// innerPoints = []
}""")
        printer.println("""Layout NoComm {
	ghostLayers = [ 0, 0 ]
	duplicateLayers = [ 1, 1 ]
	// innerPoints = []
}""")
      }

      case 3 => {
        printer.println("""Layout BasicComm {
	ghostLayers = [ 1, 1, 1 ] with communication
	duplicateLayers = [ 1, 1, 1 ] with communication
	// innerPoints = []
}""")
        printer.println("""Layout NoComm {
	ghostLayers = [ 0, 0, 0 ]
	duplicateLayers = [ 1, 1, 1 ]
	// innerPoints = []
}""")
      }
    }
    printer.println

    // Fields 
    if (testBC) {
      printer.println("Field Solution< Real, global, BasicComm, 0.0 >@(coarsest to (finest - 1))")
      printer.println("Field Solution2< Real, global, BasicComm, 0.0 >@(coarsest to (finest - 1))")
      if ("Jac" == smoother) {
        printer.println("Field Solution< Real, global, BasicComm, sin ( M_PI * xPos ) * sinh ( M_PI * yPos ) >@finest")
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
      printer.println("external Field extSolution <bla> => Solution @(finest)")
      printer.println
    }

    // Stencils
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
          printer.println("Set red [1 + (y % 2), 1] - [1, 1] steps [2, 1]")
          printer.println("Set black [2 - (y % 2), 1] - [1, 1] steps [2, 1]")
        }
      }
      case 3 => {
        printer.println("Set inner [1, 1, 1] - [1, 1, 1] steps [1, 1, 1]")
        printer.println("Set innerForFieldsWithoutGhostLayers [0, 0, 0] - [0, 0, 0] steps [1, 1, 1] // this concept might need some improvement")
        printer.println("Set domain [0, 0, 0] - [0, 0, 0] steps [1, 1, 1]")
        if ("RBGS" == smoother) {
          printer.println("Set red [1 + ((y + z) % 2), 1, 1] - [1, 1, 1] steps [2, 1, 1]")
          printer.println("Set black [2 - ((y + z) % 2), 1, 1] - [1, 1, 1] steps [2, 1, 1]")
        }
      }
    }
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
    smoother match {
      case "Jac" => printer.println("""// Jacobi
def Smoother@((coarsest + 1) to finest) ( ) : Unit {
	communicate Solution@(current)
	loop over inner on Solution@(current) {
		Solution2@(current) = Solution@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * 0.8 ) * ( RHS@(current) - Lapl@(current) * Solution@(current) ) )
	}
	communicate Solution2@(current)
	loop over inner on Solution@(current) {
		Solution@(current) = Solution2@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * 0.8 ) * ( RHS@(current) - Lapl@(current) * Solution2@(current) ) )
	}
}""")
      case "RBGS" => printer.println("""// RBGS
def Smoother@((coarsest + 1) to finest) ( ) : Unit {
	communicate Solution@(current)
	loop over red on Solution@(current) {
		Solution@(current) = Solution@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * 1.0 ) * ( RHS@(current) - Lapl@(current) * Solution@(current) ) )
	}
	communicate Solution@(current)
	loop over black on Solution@(current) {
		Solution@(current) = Solution@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * 1.0 ) * ( RHS@(current) - Lapl@(current) * Solution@(current) ) )
	}
}""")
      case "GS" => printer.println("""// GS
def Smoother@((coarsest + 1) to finest) ( ) : Unit {
	communicate Solution@(current)
	loop over inner on Solution@(current) {
		Solution@(current) = Solution@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * 1.0 ) * ( RHS@(current) - Lapl@(current) * Solution@(current) ) )
	}
}""")
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

    // Application
    printer.println("""def Application ( ) : Unit {
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
