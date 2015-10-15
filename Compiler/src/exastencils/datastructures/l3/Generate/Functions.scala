package exastencils.datastructures.l3

import exastencils.knowledge._
import scala.collection.mutable.ListBuffer

object Functions {
  def solFunction : String = {
    if (Knowledge.l3tmp_genNonZeroRhs) {
      Knowledge.l3tmp_exactSolution match {
        case "Polynomial" =>
          if (Knowledge.l3tmp_genPeriodicBounds)
            Knowledge.dimensionality match {
              case 2 => "( vf_nodePosition_x@current * vf_nodePosition_x@current )"
              case 3 => "( vf_nodePosition_x@current * vf_nodePosition_x@current )"
            }
          else
            Knowledge.dimensionality match {
              case 2 => "( vf_nodePosition_x@current * vf_nodePosition_x@current * vf_nodePosition_x@current + vf_nodePosition_y@current * vf_nodePosition_y@current * vf_nodePosition_y@current )"
              case 3 => "( vf_nodePosition_x@current * vf_nodePosition_x@current * vf_nodePosition_x@current + vf_nodePosition_y@current * vf_nodePosition_y@current * vf_nodePosition_y@current + vf_nodePosition_z@current * vf_nodePosition_z@current * vf_nodePosition_z@current )"
            }
        case "Trigonometric" if Knowledge.experimental_Neumann =>
          Knowledge.dimensionality match {
            case 2 => "( cos ( 2.0 * PI * vf_nodePosition_x@current ) * cos ( 2.0 * PI * vf_nodePosition_y@current ) )"
            case 3 => "( cos ( 2.0 * PI * vf_nodePosition_x@current ) * cos ( 2.0 * PI * vf_nodePosition_y@current ) * cos ( 2.0 * PI * vf_nodePosition_z@current ) )"
          }
        case "Kappa" =>
          Knowledge.dimensionality match {
            case 2 => "( kappa * ( (vf_nodePosition_x@current - vf_nodePosition_x@current ** 2) * (vf_nodePosition_y@current - vf_nodePosition_y@current ** 2) ) )"
            case 3 => "( kappa * ( (vf_nodePosition_x@current - vf_nodePosition_x@current ** 2) * (vf_nodePosition_y@current - vf_nodePosition_y@current ** 2) * (vf_nodePosition_z@current - vf_nodePosition_z@current ** 2) ) )"
          }
        case "Kappa_VC" =>
          Knowledge.dimensionality match {
            case 2 => "( 1.0 - exp ( -1.0 * kappa * ( (vf_nodePosition_x@current - vf_nodePosition_x@current ** 2) * (vf_nodePosition_y@current - vf_nodePosition_y@current ** 2) ) ) )"
            case 3 => "( 1.0 - exp ( -1.0 * kappa * ( (vf_nodePosition_x@current - vf_nodePosition_x@current ** 2) * (vf_nodePosition_y@current - vf_nodePosition_y@current ** 2) * (vf_nodePosition_z@current - vf_nodePosition_z@current ** 2) ) ) )"
          }
      }
    } else {
      Knowledge.l3tmp_exactSolution match {
        case "Zero" => "0.0"
        case "Polynomial" =>
          Knowledge.dimensionality match {
            case 2 => "( vf_nodePosition_x@current * vf_nodePosition_x@current - vf_nodePosition_y@current * vf_nodePosition_y@current )"
            case 3 => "( vf_nodePosition_x@current * vf_nodePosition_x@current - 0.5 * vf_nodePosition_y@current * vf_nodePosition_y@current - 0.5 * vf_nodePosition_z@current * vf_nodePosition_z@current )"
          }
        case "Trigonometric" =>
          Knowledge.dimensionality match {
            case 2 =>
              if (Knowledge.useDblPrecision)
                "( sin ( PI * vf_nodePosition_x@current ) * sinh ( PI * vf_nodePosition_y@current ) )"
              else
                "( sinf ( PI * vf_nodePosition_x@current ) * sinhf ( PI * vf_nodePosition_y@current ) )"
            case 3 =>
              if (Knowledge.useDblPrecision)
                "( sin ( PI * vf_nodePosition_x@current ) * sin ( PI * vf_nodePosition_y@current ) * sinh ( sqrt ( 2.0 ) * PI * vf_nodePosition_z@current ) )"
              else
                "( sinf ( PI * vf_nodePosition_x@current ) * sinf ( PI * vf_nodePosition_y@current ) * sinhf ( sqrt ( 2.0 ) * PI * vf_nodePosition_z@current ) )"
          }
      }
    }
  }

  def rhsFunction : String = {
    if (Knowledge.l3tmp_genNonZeroRhs) {
      Knowledge.l3tmp_exactSolution match {
        case "Polynomial" =>
          if (Knowledge.l3tmp_genPeriodicBounds)
            Knowledge.dimensionality match {
              case 2 => "-2.0"
              case 3 => "-2.0"
            }
          else
            Knowledge.dimensionality match {
              case 2 => "( -6.0 * ( vf_nodePosition_x@current + vf_nodePosition_y@current ) )"
              case 3 => "( -6.0 * ( vf_nodePosition_x@current + vf_nodePosition_y@current + vf_nodePosition_z@current ) )"
            }
        case "Trigonometric" if Knowledge.experimental_Neumann =>
          Knowledge.dimensionality match {
            case 2 => "( 8.0 * PI * PI * cos ( 2.0 * PI * vf_nodePosition_x@current ) * cos ( 2.0 * PI * vf_nodePosition_y@current ) )"
            case 3 => "( 12.0 * PI * PI * cos ( 2.0 * PI * vf_nodePosition_x@current ) * cos ( 2.0 * PI * vf_nodePosition_y@current ) * cos ( 2.0 * PI * vf_nodePosition_z@current ) )"
          }
        case "Kappa" | "Kappa_VC" =>
          Knowledge.dimensionality match {
            case 2 => "( 2.0 * kappa * ( (vf_nodePosition_x@current - vf_nodePosition_x@current ** 2) + (vf_nodePosition_y@current - vf_nodePosition_y@current ** 2) ) )"
            case 3 => "( 2.0 * kappa * ( (vf_nodePosition_x@current - vf_nodePosition_x@current ** 2) * (vf_nodePosition_y@current - vf_nodePosition_y@current ** 2) + (vf_nodePosition_x@current - vf_nodePosition_x@current ** 2) * (vf_nodePosition_z@current - vf_nodePosition_z@current ** 2) + (vf_nodePosition_y@current - vf_nodePosition_y@current ** 2) * (vf_nodePosition_z@current - vf_nodePosition_z@current ** 2) ) )"
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
          entries += (("[ 0,  0]", s"( getCoefficient ( vf_nodePosition_x@current + 0.5 * vf_gridWidth_x@current, vf_nodePosition_y@current) + getCoefficient ( vf_nodePosition_x@current - 0.5 * vf_gridWidth_x@current, vf_nodePosition_y@current ) ) / ( vf_gridWidth_x@current * vf_gridWidth_x@current ) + ( getCoefficient ( vf_nodePosition_x@current, vf_nodePosition_y@current + 0.5 * vf_gridWidth_y@current ) + getCoefficient ( vf_nodePosition_x@current, vf_nodePosition_y@current - 0.5 * vf_gridWidth_y@current ) ) / ( vf_gridWidth_y@current * vf_gridWidth_y@current )"))
          entries += (("[ 1,  0]", s"-1.0 * getCoefficient ( vf_nodePosition_x@current + 0.5 * vf_gridWidth_x@current, vf_nodePosition_y@current ) / ( vf_gridWidth_x@current * vf_gridWidth_x@current )"))
          entries += (("[-1,  0]", s"-1.0 * getCoefficient ( vf_nodePosition_x@current - 0.5 * vf_gridWidth_x@current, vf_nodePosition_y@current ) / ( vf_gridWidth_x@current * vf_gridWidth_x@current )"))
          entries += (("[ 0,  1]", s"-1.0 * getCoefficient ( vf_nodePosition_x@current, vf_nodePosition_y@current + 0.5 * vf_gridWidth_y@current ) / ( vf_gridWidth_y@current * vf_gridWidth_y@current )"))
          entries += (("[ 0, -1]", s"-1.0 * getCoefficient ( vf_nodePosition_x@current, vf_nodePosition_y@current - 0.5 * vf_gridWidth_y@current ) / ( vf_gridWidth_y@current * vf_gridWidth_y@current )"))
        }
        case 3 => {
          entries += (("[ 0,  0,  0]", s"( getCoefficient ( vf_nodePosition_x@current + 0.5 * vf_gridWidth_x@current, vf_nodePosition_y@current, vf_nodePosition_z@current ) + getCoefficient ( vf_nodePosition_x@current - 0.5 * vf_gridWidth_x@current, vf_nodePosition_y@current, vf_nodePosition_z@current ) ) / ( vf_gridWidth_x@current * vf_gridWidth_x@current ) + ( getCoefficient ( vf_nodePosition_x@current, vf_nodePosition_y@current + 0.5 * vf_gridWidth_y@current, vf_nodePosition_z@current ) + getCoefficient ( vf_nodePosition_x@current, vf_nodePosition_y@current - 0.5 * vf_gridWidth_y@current, vf_nodePosition_z@current ) ) / ( vf_gridWidth_y@current * vf_gridWidth_y@current ) + ( getCoefficient ( vf_nodePosition_x@current, vf_nodePosition_y@current, vf_nodePosition_z@current + 0.5 * vf_gridWidth_z@current ) + getCoefficient ( vf_nodePosition_x@current, vf_nodePosition_y@current, vf_nodePosition_z@current - 0.5 * vf_gridWidth_z@current ) ) / ( vf_gridWidth_z@current * vf_gridWidth_z@current )"))
          entries += (("[ 1,  0,  0]", s"-1.0 * getCoefficient ( vf_nodePosition_x@current + 0.5 * vf_gridWidth_x@current, vf_nodePosition_y@current, vf_nodePosition_z@current ) / ( vf_gridWidth_x@current * vf_gridWidth_x@current )"))
          entries += (("[-1,  0,  0]", s"-1.0 * getCoefficient ( vf_nodePosition_x@current - 0.5 * vf_gridWidth_x@current, vf_nodePosition_y@current, vf_nodePosition_z@current ) / ( vf_gridWidth_x@current * vf_gridWidth_x@current )"))
          entries += (("[ 0,  1,  0]", s"-1.0 * getCoefficient ( vf_nodePosition_x@current, vf_nodePosition_y@current + 0.5 * vf_gridWidth_y@current, vf_nodePosition_z@current ) / ( vf_gridWidth_y@current * vf_gridWidth_y@current )"))
          entries += (("[ 0, -1,  0]", s"-1.0 * getCoefficient ( vf_nodePosition_x@current, vf_nodePosition_y@current - 0.5 * vf_gridWidth_y@current, vf_nodePosition_z@current ) / ( vf_gridWidth_y@current * vf_gridWidth_y@current )"))
          entries += (("[ 0,  0,  1]", s"-1.0 * getCoefficient ( vf_nodePosition_x@current, vf_nodePosition_y@current, vf_nodePosition_z@current + 0.5 * vf_gridWidth_z@current ) / ( vf_gridWidth_z@current * vf_gridWidth_z@current )"))
          entries += (("[ 0,  0, -1]", s"-1.0 * getCoefficient ( vf_nodePosition_x@current, vf_nodePosition_y@current, vf_nodePosition_z@current - 0.5 * vf_gridWidth_z@current ) / ( vf_gridWidth_z@current * vf_gridWidth_z@current )"))
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
            entries += (("[ 0,  0]", "( 2.0 / ( vf_gridWidth_x@current * vf_gridWidth_x@current ) + 2.0 / ( vf_gridWidth_y@current * vf_gridWidth_y@current ) )"))
            entries += (("[ 1,  0]", "( -1.0 / ( vf_gridWidth_x@current * vf_gridWidth_x@current ) )"))
            entries += (("[-1,  0]", "( -1.0 / ( vf_gridWidth_x@current * vf_gridWidth_x@current ) )"))
            entries += (("[ 0,  1]", "( -1.0 / ( vf_gridWidth_y@current * vf_gridWidth_y@current ) )"))
            entries += (("[ 0, -1]", "( -1.0 / ( vf_gridWidth_y@current * vf_gridWidth_y@current ) )"))
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
            entries += (("[ 0,  0,  0]", "( 2.0 / ( vf_gridWidth_x@current * vf_gridWidth_x@current ) + 2.0 / ( vf_gridWidth_y@current * vf_gridWidth_y@current ) + 2.0 / ( vf_gridWidth_z@current * vf_gridWidth_z@current ) )"))
            entries += (("[ 1,  0,  0]", "( -1.0 / ( vf_gridWidth_x@current * vf_gridWidth_x@current ) )"))
            entries += (("[-1,  0,  0]", "( -1.0 / ( vf_gridWidth_x@current * vf_gridWidth_x@current ) )"))
            entries += (("[ 0,  1,  0]", "( -1.0 / ( vf_gridWidth_y@current * vf_gridWidth_y@current ) )"))
            entries += (("[ 0, -1,  0]", "( -1.0 / ( vf_gridWidth_y@current * vf_gridWidth_y@current ) )"))
            entries += (("[ 0,  0,  1]", "( -1.0 / ( vf_gridWidth_z@current * vf_gridWidth_z@current ) )"))
            entries += (("[ 0,  0, -1]", "( -1.0 / ( vf_gridWidth_z@current * vf_gridWidth_z@current ) )"))
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
