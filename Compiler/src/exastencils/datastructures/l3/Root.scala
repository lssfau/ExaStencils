package exastencils.datastructures.l3

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.multiGrid._

case class Root() extends Node {

  def printToL4(filename : String) : Unit = {
    var printer = new java.io.PrintWriter(filename)

    // Domains
    printer.println("Domain global< [ 0, 0, 0 ] to [ 1, 1, 1 ] >")
    printer.println

    // Layouts
    printer.write("""
Layout BasicComm {
	ghostLayers = [ 1, 1, 1 ] with communication
	duplicateLayers = [ 1, 1, 1 ] with communication
	// innerPoints = []
}

Layout NoComm {
	ghostLayers = [ 0, 0, 0 ]
	duplicateLayers = [ 1, 1, 1 ]
	// innerPoints = []
}""")
    printer.println

    // Fields 
    printer.println("Field Solution< Real, global, BasicComm, 0.0 >@(coarsest to (finest - 1))")
    printer.println("Field Solution2< Real, global, BasicComm, 0.0 >@(coarsest to (finest - 1))")
    printer.println("Field Solution< Real, global, BasicComm, sin ( M_PI * xPos ) * sinh ( M_PI * yPos ) >@finest")
    printer.println("Field Solution2< Real, global, BasicComm, sin ( M_PI * xPos ) * sinh ( M_PI * yPos ) >@finest")
    printer.println("Field Residual< Real, global, BasicComm, None >@all")
    printer.println("Field RHS< Real, global, NoComm, None >@all")
    printer.println("Field VecP< Real, global, BasicComm, None >@coarsest")
    printer.println("Field VecGradP< Real, global, NoComm, None >@coarsest")
    printer.println

    // External Fields
    printer.println("external Field extSolution <bla> => Solution @(finest)")
    printer.println

    // Stencils
    printer.write("""
Stencil Lapl@all {
	[ 0,  0,  0] => 6
	[ 1,  0,  0] => -1
	[-1,  0,  0] => -1
	[ 0,  1,  0] => -1
	[ 0, -1,  0] => -1
	[ 0,  0,  1] => -1
	[ 0,  0, -1] => -1
}

Stencil CorrectionStencil@all {
	[    0,     0,     0] => 0.0625
	[x % 2,     0,     0] => 0.0625
	[    0, y % 2,     0] => 0.0625
	[x % 2, y % 2,     0] => 0.0625
	[    0,     0, z % 2] => 0.0625
	[x % 2,     0, z % 2] => 0.0625
	[    0, y % 2, z % 2] => 0.0625
	[x % 2, y % 2, z % 2] => 0.0625
}

Stencil RestrictionStencil@all {
	[ 0,  0,  0] => 1.0

	[ 0,  0, -1] => 0.5
	[ 0,  0,  1] => 0.5
	[ 0, -1,  0] => 0.5
	[ 0,  1,  0] => 0.5
	[-1,  0,  0] => 0.5
	[ 1,  0,  0] => 0.5

	[ 0, -1,  1] => 0.25
	[ 0, -1, -1] => 0.25
	[ 0,  1,  1] => 0.25
	[ 0,  1, -1] => 0.25
	[-1,  0,  1] => 0.25
	[-1,  0, -1] => 0.25
	[ 1,  0,  1] => 0.25
	[ 1,  0, -1] => 0.25
	[-1, -1,  0] => 0.25
	[-1,  1,  0] => 0.25
	[ 1, -1,  0] => 0.25
	[ 1,  1,  0] => 0.25

	[-1, -1,  1] => 0.125
	[-1, -1, -1] => 0.125
	[-1,  1,  1] => 0.125
	[-1,  1, -1] => 0.125
	[ 1, -1,  1] => 0.125
	[ 1, -1, -1] => 0.125
	[ 1,  1,  1] => 0.125
	[ 1,  1, -1] => 0.125
}""")
    printer.println

    // Iteration Sets
    printer.write("""
Set inner [1, 1, 1] - [1, 1, 1] steps [1, 1, 1]
Set innerForFieldsWithoutGhostLayers [0, 0, 0] - [0, 0, 0] steps [1, 1, 1] // this concept might need some improvement
Set domain [0, 0, 0] - [0, 0, 0] steps [1, 1, 1]
// 2D
//Set red [1 + (y % 2), 1, 1] - [1, 1, 1] steps [2, 1, 1]
//Set black [2 - (y % 2), 1, 1] - [1, 1, 1] steps [2, 1, 1]
// 3D
Set red [1 + ((y + z) % 2), 1, 1] - [1, 1, 1] steps [2, 1, 1]
Set black [2 - ((y + z) % 2), 1, 1] - [1, 1, 1] steps [2, 1, 1]""")
    printer.println

    // CGS
    printer.write("""
def VCycle@coarsest ( ) : Unit {
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
    printer.write("""
def VCycle@((coarsest + 1) to finest) ( ) : Unit {
	repeat up 2 {
		Smoother@(current) ( )
	}
	UpResidual@(current) ( )
	Restrict@(current) ( )
	set@(coarser) ( 0 )
	VCycle@(coarser) ( )
	interpolatecorr@(current) ( )
	repeat up 2 {
		Smoother@(current) ( )
	}
}""")
    printer.println

    // Smoother
    printer.write("""
// Jacobi
def Smoother@((coarsest + 1) to finest) ( ) : Unit {
	communicate Solution@(current)
	loop over inner on Solution@(current) {
		Solution2@(current) = Solution@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * 0.8 ) * ( RHS@(current) - Lapl@(current) * Solution@(current) ) )
	}
	communicate Solution2@(current)
	loop over inner on Solution@(current) {
		Solution@(current) = Solution2@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * 0.8 ) * ( RHS@(current) - Lapl@(current) * Solution2@(current) ) )
	}
}

// RBGS
//def Smoother@((coarsest + 1) to finest) ( ) : Unit {
//	communicate Solution@(current)
//	loop over red on Solution@(current) {
//		Solution@(current) = Solution@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * 1.0 ) * ( RHS@(current) - Lapl@(current) * Solution@(current) ) )
//	}
//	communicate Solution@(current)
//	loop over black on Solution@(current) {
//		Solution@(current) = Solution@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * 1.0 ) * ( RHS@(current) - Lapl@(current) * Solution@(current) ) )
//	}
//}

// GS
//def Smoother@((coarsest + 1) to finest) ( ) : Unit {
//	communicate Solution@(current)
//	loop over inner on Solution@(current) {
//		Solution@(current) = Solution@(current) + ( ( ( 1.0 / diag ( Lapl@(current) ) ) * 1.0 ) * ( RHS@(current) - Lapl@(current) * Solution@(current) ) )
//	}
//}""")
    printer.println

    // Other MG Functions
    printer.write("""
def UpResidual@all ( ) : Unit {
	communicate Solution@(current)
	loop over inner on Residual@(current) {
		Residual@(current) = RHS@(current) - (Lapl@(current) * Solution@(current))
	}
}

def Restrict @((coarsest + 1) to finest) ( ) : Unit { 
	communicate Residual@(current)
	loop over innerForFieldsWithoutGhostLayers on RHS@(coarser) {
		RHS@(coarser) = ToCoarser ( RestrictionStencil@(current) * Residual@(current) )
    }
}

def interpolatecorr@((coarsest + 1) to finest) ( ) : Unit { 
	communicate Solution@(current)
	loop over inner on Solution@(current) {
		Solution@(current) += ToFiner ( CorrectionStencil@(current) * Solution@(coarser) )
	}
}""")
    printer.println

    // Util Functions
    printer.write("""
def set@all (value : Real) : Unit {
	loop over domain on Solution@(current) {
		Solution@(current) = value
	}
}

def L2Residual@(coarsest and finest) ( ) : Real {
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
    printer.write("""
def Application ( ) : Unit {
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
	
	printField('"Solution.dat"', Solution@finest)
}
""")
    printer.println

    printer.close()
  }
}
