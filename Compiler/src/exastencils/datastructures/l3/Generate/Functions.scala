package exastencils.datastructures.l3

import exastencils.knowledge._
import scala.collection.mutable.ListBuffer

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
        case "Kappa" =>
          Knowledge.dimensionality match {
            case 2 => "( kappa * ( (geometricCoordinate_x() - geometricCoordinate_x() ** 2) * (geometricCoordinate_y() - geometricCoordinate_y() ** 2) ) )"
            case 3 => "( kappa * ( (geometricCoordinate_x() - geometricCoordinate_x() ** 2) * (geometricCoordinate_y() - geometricCoordinate_y() ** 2) * (geometricCoordinate_z() - geometricCoordinate_z() ** 2) ) )"
          }
        case "Kappa_VC" =>
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
        case "Kappa" | "Kappa_VC" =>
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

object MainStencilCoefficients {
  def getEntries(postfix : String) : ListBuffer[(String, String)] = {
    var entries : ListBuffer[(String, String)] = ListBuffer()

    // specific problems with specialized stencils
    if ("Kappa_VC" == Knowledge.l3tmp_exactSolution) {
      Knowledge.dimensionality match {
        case 2 => {
          entries += (("[ 0,  0]", s"( getCoefficient ( geometricCoordinate_x() + 0.5 * gridWidth_x@current(), geometricCoordinate_y()) + getCoefficient ( geometricCoordinate_x() - 0.5 * gridWidth_x@current(), geometricCoordinate_y() ) ) / ( gridWidth_x@current() * gridWidth_x@current() ) + ( getCoefficient ( geometricCoordinate_x(), geometricCoordinate_y() + 0.5 * gridWidth_y@current() ) + getCoefficient ( geometricCoordinate_x(), geometricCoordinate_y() - 0.5 * gridWidth_y@current() ) ) / ( gridWidth_y@current() * gridWidth_y@current() )"))
          entries += (("[ 1,  0]", s"-1.0 * getCoefficient ( geometricCoordinate_x() + 0.5 * gridWidth_x@current(), geometricCoordinate_y() ) / ( gridWidth_x@current() * gridWidth_x@current() )"))
          entries += (("[-1,  0]", s"-1.0 * getCoefficient ( geometricCoordinate_x() - 0.5 * gridWidth_x@current(), geometricCoordinate_y() ) / ( gridWidth_x@current() * gridWidth_x@current() )"))
          entries += (("[ 0,  1]", s"-1.0 * getCoefficient ( geometricCoordinate_x(), geometricCoordinate_y() + 0.5 * gridWidth_y@current() ) / ( gridWidth_y@current() * gridWidth_y@current() )"))
          entries += (("[ 0, -1]", s"-1.0 * getCoefficient ( geometricCoordinate_x(), geometricCoordinate_y() - 0.5 * gridWidth_y@current() ) / ( gridWidth_y@current() * gridWidth_y@current() )"))
        }
        case 3 => {
          entries += (("[ 0,  0,  0]", s"( getCoefficient ( geometricCoordinate_x() + 0.5 * gridWidth_x@current(), geometricCoordinate_y(), geometricCoordinate_z() ) + getCoefficient ( geometricCoordinate_x() - 0.5 * gridWidth_x@current(), geometricCoordinate_y(), geometricCoordinate_z() ) ) / ( gridWidth_x@current() * gridWidth_x@current() ) + ( getCoefficient ( geometricCoordinate_x(), geometricCoordinate_y() + 0.5 * gridWidth_y@current(), geometricCoordinate_z() ) + getCoefficient ( geometricCoordinate_x(), geometricCoordinate_y() - 0.5 * gridWidth_y@current(), geometricCoordinate_z() ) ) / ( gridWidth_y@current() * gridWidth_y@current() ) + ( getCoefficient ( geometricCoordinate_x(), geometricCoordinate_y(), geometricCoordinate_z() + 0.5 * gridWidth_z@current() ) + getCoefficient ( geometricCoordinate_x(), geometricCoordinate_y(), geometricCoordinate_z() - 0.5 * gridWidth_z@current() ) ) / ( gridWidth_z@current() * gridWidth_z@current() )"))
          entries += (("[ 1,  0,  0]", s"-1.0 * getCoefficient ( geometricCoordinate_x() + 0.5 * gridWidth_x@current(), geometricCoordinate_y(), geometricCoordinate_z() ) / ( gridWidth_x@current() * gridWidth_x@current() )"))
          entries += (("[-1,  0,  0]", s"-1.0 * getCoefficient ( geometricCoordinate_x() - 0.5 * gridWidth_x@current(), geometricCoordinate_y(), geometricCoordinate_z() ) / ( gridWidth_x@current() * gridWidth_x@current() )"))
          entries += (("[ 0,  1,  0]", s"-1.0 * getCoefficient ( geometricCoordinate_x(), geometricCoordinate_y() + 0.5 * gridWidth_y@current(), geometricCoordinate_z() ) / ( gridWidth_y@current() * gridWidth_y@current() )"))
          entries += (("[ 0, -1,  0]", s"-1.0 * getCoefficient ( geometricCoordinate_x(), geometricCoordinate_y() - 0.5 * gridWidth_y@current(), geometricCoordinate_z() ) / ( gridWidth_y@current() * gridWidth_y@current() )"))
          entries += (("[ 0,  0,  1]", s"-1.0 * getCoefficient ( geometricCoordinate_x(), geometricCoordinate_y(), geometricCoordinate_z() + 0.5 * gridWidth_z@current() ) / ( gridWidth_z@current() * gridWidth_z@current() )"))
          entries += (("[ 0,  0, -1]", s"-1.0 * getCoefficient ( geometricCoordinate_x(), geometricCoordinate_y(), geometricCoordinate_z() - 0.5 * gridWidth_z@current() ) / ( gridWidth_z@current() * gridWidth_z@current() )"))
        }
      }

      return entries
    }

    if (Knowledge.l3tmp_kelvin && "_GMRF" == postfix) {
      // only valid for 2D

      entries += (("[ 0,  0]", "(4.0 + kappa)"))
      entries += (("[ 1,  0]", "-1.0"))
      entries += (("[-1,  0]", "-1.0"))
      entries += (("[ 0,  1]", "-1.0"))
      entries += (("[ 0, -1]", "-1.0"))

      return entries
    }

    // basic Laplace stencil
    if (Knowledge.l3tmp_genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 => {
          entries += (("[ 0,  0]", "Laplace_Coeff_0_0"))
          entries += (("[ 1,  0]", "Laplace_Coeff_P1_0"))
          entries += (("[-1,  0]", "Laplace_Coeff_N1_0"))
          entries += (("[ 0,  1]", "Laplace_Coeff_0_P1"))
          entries += (("[ 0, -1]", "Laplace_Coeff_0_N1"))
        }
        case 3 => {
          entries += (("[ 0,  0,  0]", "Laplace_Coeff_0_0_0"))
          entries += (("[ 1,  0,  0]", "Laplace_Coeff_P1_0_0"))
          entries += (("[-1,  0,  0]", "Laplace_Coeff_N1_0_0"))
          entries += (("[ 0,  1,  0]", "Laplace_Coeff_0_P1_0"))
          entries += (("[ 0, -1,  0]", "Laplace_Coeff_0_N1_0"))
          entries += (("[ 0,  0,  1]", "Laplace_Coeff_0_0_P1"))
          entries += (("[ 0,  0, -1]", "Laplace_Coeff_0_0_N1"))
        }
      }
    } else {
      Knowledge.dimensionality match {
        case 2 => {
          if (Knowledge.l3tmp_genHDepStencils) {
            entries += (("[ 0,  0]", "( 2.0 / ( gridWidth_x@current() * gridWidth_x@current() ) + 2.0 / ( gridWidth_y@current() * gridWidth_y@current() ) )"))
            entries += (("[ 1,  0]", "( -1.0 / ( gridWidth_x@current() * gridWidth_x@current() ) )"))
            entries += (("[-1,  0]", "( -1.0 / ( gridWidth_x@current() * gridWidth_x@current() ) )"))
            entries += (("[ 0,  1]", "( -1.0 / ( gridWidth_y@current() * gridWidth_y@current() ) )"))
            entries += (("[ 0, -1]", "( -1.0 / ( gridWidth_y@current() * gridWidth_y@current() ) )"))
          } else {
            entries += (("[ 0,  0]", "4.0"))
            entries += (("[ 1,  0]", "-1.0"))
            entries += (("[-1,  0]", "-1.0"))
            entries += (("[ 0,  1]", "-1.0"))
            entries += (("[ 0, -1]", "-1.0"))
          }
          if (Knowledge.l3tmp_genStencilStencilConv) {
            entries += (("[-1, -1]", "0.0"))
            entries += (("[-1,  1]", "0.0"))
            entries += (("[ 1, -1]", "0.0"))
            entries += (("[ 1,  1]", "0.0"))
          }
        }
        case 3 => {
          if (Knowledge.l3tmp_genHDepStencils) {
            entries += (("[ 0,  0,  0]", "( 2.0 / ( gridWidth_x@current() * gridWidth_x@current() ) + 2.0 / ( gridWidth_y@current() * gridWidth_y@current() ) + 2.0 / ( gridWidth_z@current() * gridWidth_z@current() ) )"))
            entries += (("[ 1,  0,  0]", "( -1.0 / ( gridWidth_x@current() * gridWidth_x@current() ) )"))
            entries += (("[-1,  0,  0]", "( -1.0 / ( gridWidth_x@current() * gridWidth_x@current() ) )"))
            entries += (("[ 0,  1,  0]", "( -1.0 / ( gridWidth_y@current() * gridWidth_y@current() ) )"))
            entries += (("[ 0, -1,  0]", "( -1.0 / ( gridWidth_y@current() * gridWidth_y@current() ) )"))
            entries += (("[ 0,  0,  1]", "( -1.0 / ( gridWidth_z@current() * gridWidth_z@current() ) )"))
            entries += (("[ 0,  0, -1]", "( -1.0 / ( gridWidth_z@current() * gridWidth_z@current() ) )"))
          } else {
            entries += (("[ 0,  0,  0]", "6.0"))
            entries += (("[ 1,  0,  0]", "-1.0"))
            entries += (("[-1,  0,  0]", "-1.0"))
            entries += (("[ 0,  1,  0]", "-1.0"))
            entries += (("[ 0, -1,  0]", "-1.0"))
            entries += (("[ 0,  0,  1]", "-1.0"))
            entries += (("[ 0,  0, -1]", "-1.0"))
          }
          if (Knowledge.l3tmp_genStencilStencilConv) {
            entries += (("[ 0, -1,  1]", "0.0"))
            entries += (("[ 0, -1, -1]", "0.0"))
            entries += (("[ 0,  1,  1]", "0.0"))
            entries += (("[ 0,  1, -1]", "0.0"))
            entries += (("[-1,  0,  1]", "0.0"))
            entries += (("[-1,  0, -1]", "0.0"))
            entries += (("[ 1,  0,  1]", "0.0"))
            entries += (("[ 1,  0, -1]", "0.0"))
            entries += (("[-1, -1,  0]", "0.0"))
            entries += (("[-1,  1,  0]", "0.0"))
            entries += (("[ 1, -1,  0]", "0.0"))
            entries += (("[ 1,  1,  0]", "0.0"))

            entries += (("[-1, -1,  1]", "0.0"))
            entries += (("[-1, -1, -1]", "0.0"))
            entries += (("[-1,  1,  1]", "0.0"))
            entries += (("[-1,  1, -1]", "0.0"))
            entries += (("[ 1, -1,  1]", "0.0"))
            entries += (("[ 1, -1, -1]", "0.0"))
            entries += (("[ 1,  1,  1]", "0.0"))
            entries += (("[ 1,  1, -1]", "0.0"))
          }
        }
      }
    }

    entries
  }
}