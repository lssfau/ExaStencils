package exastencils.datastructures.l3

import exastencils.knowledge._

object Functions {
  def solFunction : String = {
    if (Knowledge.l3tmp_genNonZeroRhs) {
      Knowledge.l3tmp_exactSolution match {
        case "Polynomial" =>
          Knowledge.dimensionality match {
            case 2 => "( xPos * xPos * xPos + yPos * yPos * yPos )"
            case 3 => "( xPos * xPos * xPos + yPos * yPos * yPos + zPos * zPos * zPos )"
          }
        case "Trigonometric" if Knowledge.experimental_Neumann =>
          Knowledge.dimensionality match {
            case 2 => "( cos ( 2.0 * PI * xPos ) * cos ( 2.0 * PI * yPos ) )"
            case 3 => "( cos ( 2.0 * PI * xPos ) * cos ( 2.0 * PI * yPos ) * cos ( 2.0 * PI * zPos ) )"
          }
        case "Kappa" if (!Knowledge.l3tmp_genStencilFields) =>
          Knowledge.dimensionality match {
            case 2 => "( kappa * ( (xPos - xPos ** 2) * (yPos - yPos ** 2) ) )"
            case 3 => "( kappa * ( (xPos - xPos ** 2) * (yPos - yPos ** 2) * (zPos - zPos ** 2) ) )"
          }
        case "Kappa" if (Knowledge.l3tmp_genStencilFields) =>
          Knowledge.dimensionality match {
            case 2 => "( 1.0 - exp ( -1.0 * kappa * ( (xPos - xPos ** 2) * (yPos - yPos ** 2) ) ) )"
            case 3 => "( 1.0 - exp ( -1.0 * kappa * ( (xPos - xPos ** 2) * (yPos - yPos ** 2) * (zPos - zPos ** 2) ) ) )"
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
                "( sin ( PI * xPos ) * sinh ( PI * yPos ) )"
              else
                "( sinf ( PI * xPos ) * sinhf ( PI * yPos ) )"
            case 3 =>
              if (Knowledge.useDblPrecision)
                "( sin ( PI * xPos ) * sin ( PI * yPos ) * sinh ( sqrt ( 2.0 ) * PI * zPos ) )"
              else
                "( sinf ( PI * xPos ) * sinf ( PI * yPos ) * sinhf ( sqrt ( 2.0 ) * PI * zPos ) )"
          }
      }
    }
  }

  def rhsFunction : String = {
    if (Knowledge.l3tmp_genNonZeroRhs) {
      Knowledge.l3tmp_exactSolution match {
        case "Polynomial" =>
          Knowledge.dimensionality match {
            case 2 => "( -6.0 * ( xPos + yPos ) )"
            case 3 => "( -6.0 * ( xPos + yPos + zPos ) )"
          }
        case "Trigonometric" if Knowledge.experimental_Neumann =>
          Knowledge.dimensionality match {
            case 2 => "( 8.0 * PI * PI * cos ( 2.0 * PI * xPos ) * cos ( 2.0 * PI * yPos ) )"
            case 3 => "( 12.0 * PI * PI * cos ( 2.0 * PI * xPos ) * cos ( 2.0 * PI * yPos ) * cos ( 2.0 * PI * zPos ) )"
          }
        case "Kappa" =>
          Knowledge.dimensionality match {
            case 2 => "( 2.0 * kappa * ( (xPos - xPos ** 2) + (yPos - yPos ** 2) ) )"
            case 3 => "( 2.0 * kappa * ( (xPos - xPos ** 2) * (yPos - yPos ** 2) + (xPos - xPos ** 2) * (zPos - zPos ** 2) + (yPos - yPos ** 2) * (zPos - zPos ** 2) ) )"
          }
      }
    } else {
      "0.0"
    }
  }
}