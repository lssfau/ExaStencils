package exastencils.datastructures.l3

import exastencils.knowledge._

object Functions {
  def solFunction = {
    if (Knowledge.l3tmp_genNonZeroRhs) {
      Knowledge.l3tmp_exactSolution match {
        case "Polynomial" =>
          Knowledge.dimensionality match {
            case 2 => "( xPos * xPos * xPos + yPos * yPos * yPos )"
            case 3 => "( xPos * xPos * xPos + yPos * yPos * yPos + zPos * zPos * zPos )"
          }
      }
    } else {
      Knowledge.l3tmp_exactSolution match {
        case "Zero" => "0.0"
        case "Polynomial" =>
          Knowledge.dimensionality match {
            case 2 => "( xPos * xPos - yPos * yPos )"
            case 3 => "( xPos * xPos - 0.5 * yPos * yPos - 0.5 * zPos * zPos )"
          }
        case "Trigonometric" =>
          Knowledge.dimensionality match {
            case 2 =>
              if (Knowledge.useDblPrecision)
                "( sin ( M_PI * xPos ) * sinh ( M_PI * yPos ) )"
              else
                "( sinf ( M_PI * xPos ) * sinhf ( M_PI * yPos ) )"
            case 3 =>
              if (Knowledge.useDblPrecision)
                "( sin ( M_PI * xPos ) * sin ( M_PI * yPos ) * sinh ( sqrt ( 2.0 ) * M_PI * zPos ) )"
              else
                "( sinf ( M_PI * xPos ) * sinf ( M_PI * yPos ) * sinhf ( sqrt ( 2.0 ) * M_PI * zPos ) )"
          }
      }
    }
  }

  def rhsFunction = {
    if (Knowledge.l3tmp_genNonZeroRhs) {
      Knowledge.l3tmp_exactSolution match {
        case "Polynomial" =>
          Knowledge.dimensionality match {
            case 2 => "( -6.0 * ( xPos + yPos ) )"
            case 3 => "( -6.0 * ( xPos + yPos + zPos ) )"
          }
      }
    } else {
      "0.0"
    }
  }
}