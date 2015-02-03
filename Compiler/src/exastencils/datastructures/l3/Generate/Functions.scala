package exastencils.datastructures.l3

import exastencils.knowledge._

object Functions {
  def solFunction = {
    if (!Knowledge.l3tmp_genFunctionBC) {
      "0.0"
    } else {
      Knowledge.l3tmp_functionBC match {
        case "Polynomial" => Knowledge.dimensionality match {
          case 2 => "( xPos * xPos - yPos * yPos )"
          case 3 => "( xPos * xPos - 0.5 * yPos * yPos - 0.5 * zPos * zPos )"
        }
        case "Trigonometric" => Knowledge.dimensionality match {
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
        case "InvSqrt" => Knowledge.dimensionality match {
          case 2 => "( 1.0 / sqrt ( xPos * xPos + yPos * yPos ) )"
          case 3 => "( 1.0 / sqrt ( xPos * xPos + yPos * yPos + zPos * zPos ) )"
        }
      }
    }
  }

  def rhsFunction = {
    "0.0"
  }
}