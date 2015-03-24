package exastencils.datastructures.l3

import exastencils.knowledge._

object Functions {
  def solFunction : String = {
    if (Knowledge.l3tmp_genNonZeroRhs) {
      Knowledge.l3tmp_exactSolution match {
        case "Polynomial" =>
          Knowledge.dimensionality match {
            case 2 => "( geometricCoordinate_x() * geometricCoordinate_x() * geometricCoordinate_x() + geometricCoordinate_y() * geometricCoordinate_y() * geometricCoordinate_y() )"
            case 3 => "( geometricCoordinate_x() * geometricCoordinate_x() * geometricCoordinate_x() + geometricCoordinate_y() * geometricCoordinate_y() * geometricCoordinate_y() + geometricCoordinate_z() * geometricCoordinate_z() * geometricCoordinate_z() )"
          }
        case "Trigonometric" if Knowledge.experimental_Neumann =>
          Knowledge.dimensionality match {
            case 2 => "( cos ( 2.0 * PI * geometricCoordinate_x() ) * cos ( 2.0 * PI * geometricCoordinate_y() ) )"
            case 3 => "( cos ( 2.0 * PI * geometricCoordinate_x() ) * cos ( 2.0 * PI * geometricCoordinate_y() ) * cos ( 2.0 * PI * geometricCoordinate_z() ) )"
          }
        case "Kappa" if (!Knowledge.l3tmp_genStencilFields) =>
          Knowledge.dimensionality match {
            case 2 => "( kappa * ( (geometricCoordinate_x() - geometricCoordinate_x() ** 2) * (geometricCoordinate_y() - geometricCoordinate_y() ** 2) ) )"
            case 3 => "( kappa * ( (geometricCoordinate_x() - geometricCoordinate_x() ** 2) * (geometricCoordinate_y() - geometricCoordinate_y() ** 2) * (geometricCoordinate_z() - geometricCoordinate_z() ** 2) ) )"
          }
        case "Kappa" if (Knowledge.l3tmp_genStencilFields) =>
          Knowledge.dimensionality match {
            case 2 => "( 1.0 - exp ( -1.0 * kappa * ( (geometricCoordinate_x() - geometricCoordinate_x() ** 2) * (geometricCoordinate_y() - geometricCoordinate_y() ** 2) ) ) )"
            case 3 => "( 1.0 - exp ( -1.0 * kappa * ( (geometricCoordinate_x() - geometricCoordinate_x() ** 2) * (geometricCoordinate_y() - geometricCoordinate_y() ** 2) * (geometricCoordinate_z() - geometricCoordinate_z() ** 2) ) ) )"
          }
      }
    } else {
      Knowledge.l3tmp_exactSolution match {
        case "Zero" => "0.0"
        case "Polynomial" =>
          Knowledge.dimensionality match {
            case 2 => "( geometricCoordinate_x() * geometricCoordinate_x() - geometricCoordinate_y() * geometricCoordinate_y() )"
            case 3 => "( geometricCoordinate_x() * geometricCoordinate_x() - 0.5 * geometricCoordinate_y() * geometricCoordinate_y() - 0.5 * geometricCoordinate_z() * geometricCoordinate_z() )"
          }
        case "Trigonometric" =>
          Knowledge.dimensionality match {
            case 2 =>
              if (Knowledge.useDblPrecision)
                "( sin ( PI * geometricCoordinate_x() ) * sinh ( PI * geometricCoordinate_y() ) )"
              else
                "( sinf ( PI * geometricCoordinate_x() ) * sinhf ( PI * geometricCoordinate_y() ) )"
            case 3 =>
              if (Knowledge.useDblPrecision)
                "( sin ( PI * geometricCoordinate_x() ) * sin ( PI * geometricCoordinate_y() ) * sinh ( sqrt ( 2.0 ) * PI * geometricCoordinate_z() ) )"
              else
                "( sinf ( PI * geometricCoordinate_x() ) * sinf ( PI * geometricCoordinate_y() ) * sinhf ( sqrt ( 2.0 ) * PI * geometricCoordinate_z() ) )"
          }
      }
    }
  }

  def rhsFunction : String = {
    if (Knowledge.l3tmp_genNonZeroRhs) {
      Knowledge.l3tmp_exactSolution match {
        case "Polynomial" =>
          Knowledge.dimensionality match {
            case 2 => "( -6.0 * ( geometricCoordinate_x() + geometricCoordinate_y() ) )"
            case 3 => "( -6.0 * ( geometricCoordinate_x() + geometricCoordinate_y() + geometricCoordinate_z() ) )"
          }
        case "Trigonometric" if Knowledge.experimental_Neumann =>
          Knowledge.dimensionality match {
            case 2 => "( 8.0 * PI * PI * cos ( 2.0 * PI * geometricCoordinate_x() ) * cos ( 2.0 * PI * geometricCoordinate_y() ) )"
            case 3 => "( 12.0 * PI * PI * cos ( 2.0 * PI * geometricCoordinate_x() ) * cos ( 2.0 * PI * geometricCoordinate_y() ) * cos ( 2.0 * PI * geometricCoordinate_z() ) )"
          }
        case "Kappa" =>
          Knowledge.dimensionality match {
            case 2 => "( 2.0 * kappa * ( (geometricCoordinate_x() - geometricCoordinate_x() ** 2) + (geometricCoordinate_y() - geometricCoordinate_y() ** 2) ) )"
            case 3 => "( 2.0 * kappa * ( (geometricCoordinate_x() - geometricCoordinate_x() ** 2) * (geometricCoordinate_y() - geometricCoordinate_y() ** 2) + (geometricCoordinate_x() - geometricCoordinate_x() ** 2) * (geometricCoordinate_z() - geometricCoordinate_z() ** 2) + (geometricCoordinate_y() - geometricCoordinate_y() ** 2) * (geometricCoordinate_z() - geometricCoordinate_z() ** 2) ) )"
          }
      }
    } else {
      "0.0"
    }
  }
}