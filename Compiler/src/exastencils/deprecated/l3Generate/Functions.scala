package exastencils.deprecated.l3Generate

import scala.collection.mutable.ListBuffer

import exastencils.knowledge._

object Functions {
  def solFunction(boundary : Boolean) : String = {
    def geomCoord : String = {
      if (boundary) "vf_boundaryCoord"
      else if (Knowledge.l3tmp_genCellBasedDiscr) "vf_cellCenter"
      else "vf_nodePosition"
    }

    if (Knowledge.l3tmp_genNonZeroRhs) {
      Knowledge.l3tmp_exactSolution match {
        case "Polynomial"                                      =>
          if (Knowledge.l3tmp_genPeriodicBounds)
            Knowledge.dimensionality match {
              case 2 => s"( ${ geomCoord }_x@current * ${ geomCoord }_x@current )"
              case 3 => s"( ${ geomCoord }_x@current * ${ geomCoord }_x@current )"
            }
          else
            Knowledge.dimensionality match {
              case 2 => s"( ${ geomCoord }_x@current * ${ geomCoord }_x@current * ${ geomCoord }_x@current + ${ geomCoord }_y@current * ${ geomCoord }_y@current * ${ geomCoord }_y@current )"
              case 3 => s"( ${ geomCoord }_x@current * ${ geomCoord }_x@current * ${ geomCoord }_x@current + ${ geomCoord }_y@current * ${ geomCoord }_y@current * ${ geomCoord }_y@current + ${ geomCoord }_z@current * ${ geomCoord }_z@current * ${ geomCoord }_z@current )"
            }
        case "Trigonometric" if Knowledge.experimental_Neumann =>
          Knowledge.dimensionality match {
            case 2 => s"( cos ( 2.0 * PI * ${ geomCoord }_x@current ) * cos ( 2.0 * PI * ${ geomCoord }_y@current ) )"
            case 3 => s"( cos ( 2.0 * PI * ${ geomCoord }_x@current ) * cos ( 2.0 * PI * ${ geomCoord }_y@current ) * cos ( 2.0 * PI * ${ geomCoord }_z@current ) )"
          }
        case "Kappa"                                           =>
          Knowledge.dimensionality match {
            case 2 => s"( kappa * ( (${ geomCoord }_x@current - ${ geomCoord }_x@current ** 2) * (${ geomCoord }_y@current - ${ geomCoord }_y@current ** 2) ) )"
            case 3 => s"( kappa * ( (${ geomCoord }_x@current - ${ geomCoord }_x@current ** 2) * (${ geomCoord }_y@current - ${ geomCoord }_y@current ** 2) * (${ geomCoord }_z@current - ${ geomCoord }_z@current ** 2) ) )"
          }
        case "Kappa_VC"                                        =>
          Knowledge.dimensionality match {
            case 2 => s"( 1.0 - exp ( -1.0 * kappa * ( (${ geomCoord }_x@current - ${ geomCoord }_x@current ** 2) * (${ geomCoord }_y@current - ${ geomCoord }_y@current ** 2) ) ) )"
            case 3 => s"( 1.0 - exp ( -1.0 * kappa * ( (${ geomCoord }_x@current - ${ geomCoord }_x@current ** 2) * (${ geomCoord }_y@current - ${ geomCoord }_y@current ** 2) * (${ geomCoord }_z@current - ${ geomCoord }_z@current ** 2) ) ) )"
          }
      }
    } else {
      Knowledge.l3tmp_exactSolution match {
        case "Zero"          => s"0.0"
        case "Polynomial"    =>
          Knowledge.dimensionality match {
            case 2 => s"( ${ geomCoord }_x@current * ${ geomCoord }_x@current - ${ geomCoord }_y@current * ${ geomCoord }_y@current )"
            case 3 => s"( ${ geomCoord }_x@current * ${ geomCoord }_x@current - 0.5 * ${ geomCoord }_y@current * ${ geomCoord }_y@current - 0.5 * ${ geomCoord }_z@current * ${ geomCoord }_z@current )"
          }
        case "Trigonometric" =>
          Knowledge.dimensionality match {
            case 2 =>
              if (Knowledge.useDblPrecision)
                s"( sin ( PI * ${ geomCoord }_x@current ) * sinh ( PI * ${ geomCoord }_y@current ) )"
              else
                s"( sinf ( PI * ${ geomCoord }_x@current ) * sinhf ( PI * ${ geomCoord }_y@current ) )"
            case 3 =>
              if (Knowledge.useDblPrecision)
                s"( sin ( PI * ${ geomCoord }_x@current ) * sin ( PI * ${ geomCoord }_y@current ) * sinh ( sqrt ( 2.0 ) * PI * ${ geomCoord }_z@current ) )"
              else
                s"( sinf ( PI * ${ geomCoord }_x@current ) * sinf ( PI * ${ geomCoord }_y@current ) * sinhf ( sqrt ( 2.0 ) * PI * ${ geomCoord }_z@current ) )"
          }
      }
    }
  }

  def rhsFunction(boundary : Boolean) : String = {
    def geomCoord : String = {
      if (boundary) "vf_boundaryCoord"
      else if (Knowledge.l3tmp_genCellBasedDiscr) "vf_cellCenter"
      else "vf_nodePosition"
    }

    if (Knowledge.l3tmp_genNonZeroRhs) {
      Knowledge.l3tmp_exactSolution match {
        case "Polynomial"                                      =>
          if (Knowledge.l3tmp_genPeriodicBounds)
            Knowledge.dimensionality match {
              case 2 => s"-2.0"
              case 3 => s"-2.0"
            }
          else
            Knowledge.dimensionality match {
              case 2 => s"( -6.0 * ( ${ geomCoord }_x@current + ${ geomCoord }_y@current ) )"
              case 3 => s"( -6.0 * ( ${ geomCoord }_x@current + ${ geomCoord }_y@current + ${ geomCoord }_z@current ) )"
            }
        case "Trigonometric" if Knowledge.experimental_Neumann =>
          Knowledge.dimensionality match {
            case 2 => s"( 8.0 * PI * PI * cos ( 2.0 * PI * ${ geomCoord }_x@current ) * cos ( 2.0 * PI * ${ geomCoord }_y@current ) )"
            case 3 => s"( 12.0 * PI * PI * cos ( 2.0 * PI * ${ geomCoord }_x@current ) * cos ( 2.0 * PI * ${ geomCoord }_y@current ) * cos ( 2.0 * PI * ${ geomCoord }_z@current ) )"
          }
        case "Kappa" | "Kappa_VC"                              =>
          Knowledge.dimensionality match {
            case 2 => s"( 2.0 * kappa * ( (${ geomCoord }_x@current - ${ geomCoord }_x@current ** 2) + (${ geomCoord }_y@current - ${ geomCoord }_y@current ** 2) ) )"
            case 3 => s"( 2.0 * kappa * ( (${ geomCoord }_x@current - ${ geomCoord }_x@current ** 2) * (${ geomCoord }_y@current - ${ geomCoord }_y@current ** 2) + (${ geomCoord }_x@current - ${ geomCoord }_x@current ** 2) * (${ geomCoord }_z@current - ${ geomCoord }_z@current ** 2) + (${ geomCoord }_y@current - ${ geomCoord }_y@current ** 2) * (${ geomCoord }_z@current - ${ geomCoord }_z@current ** 2) ) )"
          }
      }
    } else {
      "0.0"
    }
  }
}

object MainStencilCoefficients {
  def geomCoord : String = {
    if (Knowledge.l3tmp_genCellBasedDiscr) "vf_cellCenter" else "vf_nodePosition"
  }
  def geomWidth : String = {
    if (Knowledge.l3tmp_genCellBasedDiscr) "vf_cellWidth" else "vf_gridWidth"
  }

  def getEntries(postfix : String) : ListBuffer[(String, String)] = {
    var entries : ListBuffer[(String, String)] = ListBuffer()

    // specific problems with specialized stencils
    if ("Kappa_VC" == Knowledge.l3tmp_exactSolution) {
      Knowledge.dimensionality match {
        case 2 => {
          entries += (("[ 0,  0]", s"( getCoefficient ( ${ geomCoord }_x@current + 0.5 * ${ geomWidth }_x@current, ${ geomCoord }_y@current) + getCoefficient ( ${ geomCoord }_x@current - 0.5 * ${ geomWidth }_x@current, ${ geomCoord }_y@current ) ) / ( ${ geomWidth }_x@current * ${ geomWidth }_x@current ) + ( getCoefficient ( ${ geomCoord }_x@current, ${ geomCoord }_y@current + 0.5 * ${ geomWidth }_y@current ) + getCoefficient ( ${ geomCoord }_x@current, ${ geomCoord }_y@current - 0.5 * ${ geomWidth }_y@current ) ) / ( ${ geomWidth }_y@current * ${ geomWidth }_y@current )"))
          entries += (("[ 1,  0]", s"-1.0 * getCoefficient ( ${ geomCoord }_x@current + 0.5 * ${ geomWidth }_x@current, ${ geomCoord }_y@current ) / ( ${ geomWidth }_x@current * ${ geomWidth }_x@current )"))
          entries += (("[-1,  0]", s"-1.0 * getCoefficient ( ${ geomCoord }_x@current - 0.5 * ${ geomWidth }_x@current, ${ geomCoord }_y@current ) / ( ${ geomWidth }_x@current * ${ geomWidth }_x@current )"))
          entries += (("[ 0,  1]", s"-1.0 * getCoefficient ( ${ geomCoord }_x@current, ${ geomCoord }_y@current + 0.5 * ${ geomWidth }_y@current ) / ( ${ geomWidth }_y@current * ${ geomWidth }_y@current )"))
          entries += (("[ 0, -1]", s"-1.0 * getCoefficient ( ${ geomCoord }_x@current, ${ geomCoord }_y@current - 0.5 * ${ geomWidth }_y@current ) / ( ${ geomWidth }_y@current * ${ geomWidth }_y@current )"))
        }
        case 3 => {
          entries += (("[ 0,  0,  0]", s"( getCoefficient ( ${ geomCoord }_x@current + 0.5 * ${ geomWidth }_x@current, ${ geomCoord }_y@current, ${ geomCoord }_z@current ) + getCoefficient ( ${ geomCoord }_x@current - 0.5 * ${ geomWidth }_x@current, ${ geomCoord }_y@current, ${ geomCoord }_z@current ) ) / ( ${ geomWidth }_x@current * ${ geomWidth }_x@current ) + ( getCoefficient ( ${ geomCoord }_x@current, ${ geomCoord }_y@current + 0.5 * ${ geomWidth }_y@current, ${ geomCoord }_z@current ) + getCoefficient ( ${ geomCoord }_x@current, ${ geomCoord }_y@current - 0.5 * ${ geomWidth }_y@current, ${ geomCoord }_z@current ) ) / ( ${ geomWidth }_y@current * ${ geomWidth }_y@current ) + ( getCoefficient ( ${ geomCoord }_x@current, ${ geomCoord }_y@current, ${ geomCoord }_z@current + 0.5 * ${ geomWidth }_z@current ) + getCoefficient ( ${ geomCoord }_x@current, ${ geomCoord }_y@current, ${ geomCoord }_z@current - 0.5 * ${ geomWidth }_z@current ) ) / ( ${ geomWidth }_z@current * ${ geomWidth }_z@current )"))
          entries += (("[ 1,  0,  0]", s"-1.0 * getCoefficient ( ${ geomCoord }_x@current + 0.5 * ${ geomWidth }_x@current, ${ geomCoord }_y@current, ${ geomCoord }_z@current ) / ( ${ geomWidth }_x@current * ${ geomWidth }_x@current )"))
          entries += (("[-1,  0,  0]", s"-1.0 * getCoefficient ( ${ geomCoord }_x@current - 0.5 * ${ geomWidth }_x@current, ${ geomCoord }_y@current, ${ geomCoord }_z@current ) / ( ${ geomWidth }_x@current * ${ geomWidth }_x@current )"))
          entries += (("[ 0,  1,  0]", s"-1.0 * getCoefficient ( ${ geomCoord }_x@current, ${ geomCoord }_y@current + 0.5 * ${ geomWidth }_y@current, ${ geomCoord }_z@current ) / ( ${ geomWidth }_y@current * ${ geomWidth }_y@current )"))
          entries += (("[ 0, -1,  0]", s"-1.0 * getCoefficient ( ${ geomCoord }_x@current, ${ geomCoord }_y@current - 0.5 * ${ geomWidth }_y@current, ${ geomCoord }_z@current ) / ( ${ geomWidth }_y@current * ${ geomWidth }_y@current )"))
          entries += (("[ 0,  0,  1]", s"-1.0 * getCoefficient ( ${ geomCoord }_x@current, ${ geomCoord }_y@current, ${ geomCoord }_z@current + 0.5 * ${ geomWidth }_z@current ) / ( ${ geomWidth }_z@current * ${ geomWidth }_z@current )"))
          entries += (("[ 0,  0, -1]", s"-1.0 * getCoefficient ( ${ geomCoord }_x@current, ${ geomCoord }_y@current, ${ geomCoord }_z@current - 0.5 * ${ geomWidth }_z@current ) / ( ${ geomWidth }_z@current * ${ geomWidth }_z@current )"))
        }
      }

      return entries
    }

    if (Knowledge.l3tmp_kelvin && "_GMRF" == postfix) {
      // only valid for 2D

      entries += (("[ 0,  0]", s"(4.0 + kappa)"))
      entries += (("[ 1,  0]", s"-1.0"))
      entries += (("[-1,  0]", s"-1.0"))
      entries += (("[ 0,  1]", s"-1.0"))
      entries += (("[ 0, -1]", s"-1.0"))

      return entries
    }

    // basic Laplace stencil
    if (Knowledge.l3tmp_genSetableStencil) {
      Knowledge.dimensionality match {
        case 2 => {
          entries += (("[ 0,  0]", s"Laplace_Coeff_0_0"))
          entries += (("[ 1,  0]", s"Laplace_Coeff_P1_0"))
          entries += (("[-1,  0]", s"Laplace_Coeff_N1_0"))
          entries += (("[ 0,  1]", s"Laplace_Coeff_0_P1"))
          entries += (("[ 0, -1]", s"Laplace_Coeff_0_N1"))
        }
        case 3 => {
          entries += (("[ 0,  0,  0]", s"Laplace_Coeff_0_0_0"))
          entries += (("[ 1,  0,  0]", s"Laplace_Coeff_P1_0_0"))
          entries += (("[-1,  0,  0]", s"Laplace_Coeff_N1_0_0"))
          entries += (("[ 0,  1,  0]", s"Laplace_Coeff_0_P1_0"))
          entries += (("[ 0, -1,  0]", s"Laplace_Coeff_0_N1_0"))
          entries += (("[ 0,  0,  1]", s"Laplace_Coeff_0_0_P1"))
          entries += (("[ 0,  0, -1]", s"Laplace_Coeff_0_0_N1"))
        }
      }
    } else {
      Knowledge.dimensionality match {
        case 2 => {
          if (Knowledge.l3tmp_genHDepStencils) {
            entries += (("[ 0,  0]", s"( 2.0 / ( ${ geomWidth }_x@current * ${ geomWidth }_x@current ) + 2.0 / ( ${ geomWidth }_y@current * ${ geomWidth }_y@current ) )"))
            entries += (("[ 1,  0]", s"( -1.0 / ( ${ geomWidth }_x@current * ${ geomWidth }_x@current ) )"))
            entries += (("[-1,  0]", s"( -1.0 / ( ${ geomWidth }_x@current * ${ geomWidth }_x@current ) )"))
            entries += (("[ 0,  1]", s"( -1.0 / ( ${ geomWidth }_y@current * ${ geomWidth }_y@current ) )"))
            entries += (("[ 0, -1]", s"( -1.0 / ( ${ geomWidth }_y@current * ${ geomWidth }_y@current ) )"))
          } else {
            entries += (("[ 0,  0]", s"4.0"))
            entries += (("[ 1,  0]", s"-1.0"))
            entries += (("[-1,  0]", s"-1.0"))
            entries += (("[ 0,  1]", s"-1.0"))
            entries += (("[ 0, -1]", s"-1.0"))
          }
          if (Knowledge.l3tmp_genStencilStencilConv) {
            entries += (("[-1, -1]", s"0.0"))
            entries += (("[-1,  1]", s"0.0"))
            entries += (("[ 1, -1]", s"0.0"))
            entries += (("[ 1,  1]", s"0.0"))
          }
        }
        case 3 => {
          if (Knowledge.l3tmp_genHDepStencils) {
            entries += (("[ 0,  0,  0]", s"( 2.0 / ( ${ geomWidth }_x@current * ${ geomWidth }_x@current ) + 2.0 / ( ${ geomWidth }_y@current * ${ geomWidth }_y@current ) + 2.0 / ( ${ geomWidth }_z@current * ${ geomWidth }_z@current ) )"))
            entries += (("[ 1,  0,  0]", s"( -1.0 / ( ${ geomWidth }_x@current * ${ geomWidth }_x@current ) )"))
            entries += (("[-1,  0,  0]", s"( -1.0 / ( ${ geomWidth }_x@current * ${ geomWidth }_x@current ) )"))
            entries += (("[ 0,  1,  0]", s"( -1.0 / ( ${ geomWidth }_y@current * ${ geomWidth }_y@current ) )"))
            entries += (("[ 0, -1,  0]", s"( -1.0 / ( ${ geomWidth }_y@current * ${ geomWidth }_y@current ) )"))
            entries += (("[ 0,  0,  1]", s"( -1.0 / ( ${ geomWidth }_z@current * ${ geomWidth }_z@current ) )"))
            entries += (("[ 0,  0, -1]", s"( -1.0 / ( ${ geomWidth }_z@current * ${ geomWidth }_z@current ) )"))
          } else {
            entries += (("[ 0,  0,  0]", s"6.0"))
            entries += (("[ 1,  0,  0]", s"-1.0"))
            entries += (("[-1,  0,  0]", s"-1.0"))
            entries += (("[ 0,  1,  0]", s"-1.0"))
            entries += (("[ 0, -1,  0]", s"-1.0"))
            entries += (("[ 0,  0,  1]", s"-1.0"))
            entries += (("[ 0,  0, -1]", s"-1.0"))
          }
          if (Knowledge.l3tmp_genStencilStencilConv) {
            entries += (("[ 0, -1,  1]", s"0.0"))
            entries += (("[ 0, -1, -1]", s"0.0"))
            entries += (("[ 0,  1,  1]", s"0.0"))
            entries += (("[ 0,  1, -1]", s"0.0"))
            entries += (("[-1,  0,  1]", s"0.0"))
            entries += (("[-1,  0, -1]", s"0.0"))
            entries += (("[ 1,  0,  1]", s"0.0"))
            entries += (("[ 1,  0, -1]", s"0.0"))
            entries += (("[-1, -1,  0]", s"0.0"))
            entries += (("[-1,  1,  0]", s"0.0"))
            entries += (("[ 1, -1,  0]", s"0.0"))
            entries += (("[ 1,  1,  0]", s"0.0"))

            entries += (("[-1, -1,  1]", s"0.0"))
            entries += (("[-1, -1, -1]", s"0.0"))
            entries += (("[-1,  1,  1]", s"0.0"))
            entries += (("[-1,  1, -1]", s"0.0"))
            entries += (("[ 1, -1,  1]", s"0.0"))
            entries += (("[ 1, -1, -1]", s"0.0"))
            entries += (("[ 1,  1,  1]", s"0.0"))
            entries += (("[ 1,  1, -1]", s"0.0"))
          }
        }
      }
    }

    entries
  }
}
