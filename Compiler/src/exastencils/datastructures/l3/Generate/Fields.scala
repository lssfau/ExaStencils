package exastencils.datastructures.l3

import exastencils.knowledge._

object Fields {
  def solution(level : String, postfix : String = "") = {
    if (Knowledge.l3tmp_genVectorFields)
      (0 until Knowledge.l3tmp_numVecDims).toArray.map(d => s"Solution$postfix${if (Knowledge.l3tmp_useSlotsForJac) "[0]" else ""}@$level[$d]")
    else
      Array(s"Solution$postfix${if (Knowledge.l3tmp_useSlotsForJac) "[0]" else ""}@$level")
  }

  def solution2(level : String, postfix : String = "") = {
    if (Knowledge.l3tmp_genVectorFields)
      (0 until Knowledge.l3tmp_numVecDims).toArray.map(d => s"Solution$postfix${if (Knowledge.l3tmp_useSlotsForJac) "[1]" else "2"}@$level[$d]")
    else
      Array(s"Solution${if (Knowledge.l3tmp_useSlotsForJac) s"$postfix[1]" else s"2$postfix"}@$level")
  }

  def solutionSlotted(level : String, slot : String, postfix : String = "") = {
    if (Knowledge.l3tmp_genVectorFields)
      (0 until Knowledge.l3tmp_numVecDims).toArray.map(d => s"Solution$postfix[$slot]@$level[$d]")
    else
      Array(s"Solution$postfix[$slot]@$level")
  }

  def residual(level : String, postfix : String = "") = {
    if (Knowledge.l3tmp_genVectorFields)
      (0 until Knowledge.l3tmp_numVecDims).toArray.map(d => s"Residual$postfix@$level[$d]")
    else
      Array(s"Residual$postfix@$level")
  }

  def rhs(level : String, postfix : String = "") = {
    if (Knowledge.l3tmp_genVectorFields)
      (0 until Knowledge.l3tmp_numVecDims).toArray.map(d => s"RHS$postfix@$level[$d]")
    else
      Array(s"RHS$postfix@$level")
  }

  def addFields(printer : java.io.PrintWriter, postfix : String, domain : String) = {
    if (Knowledge.l3tmp_genFunctionBC || (Knowledge.l3tmp_kelvin && "" == postfix)) {
      var bc = (
        if (Knowledge.l3tmp_kelvin && "" == postfix) "bcSol(xPos, yPos)"
        else Knowledge.l3tmp_functionBC match {
          case "Polynomial" => Knowledge.dimensionality match {
            case 2 => "xPos * xPos - yPos * yPos"
            case 3 => "xPos * xPos - 0.5 * yPos * yPos - 0.5 * zPos * zPos"
          }
          case "Trigonometric" => Knowledge.dimensionality match {
            case 2 =>
              if (Knowledge.useDblPrecision)
                "sin ( M_PI * xPos ) * sinh ( M_PI * yPos )"
              else
                "sinf ( M_PI * xPos ) * sinhf ( M_PI * yPos )"
            case 3 =>
              if (Knowledge.useDblPrecision)
                "sin ( M_PI * xPos ) * sin ( M_PI * yPos ) * sinh ( sqrt ( 2.0 ) * M_PI * zPos )"
              else
                "sinf ( M_PI * xPos ) * sinf ( M_PI * yPos ) * sinhf ( sqrt ( 2.0 ) * M_PI * zPos )"
          }
          case "InvSqrt" => Knowledge.dimensionality match {
            case 2 => "1.0 / sqrt ( xPos * xPos + yPos * yPos )"
            case 3 => "1.0 / sqrt ( xPos * xPos + yPos * yPos + zPos * zPos )"
          }
        })
      if ("Jac" == Knowledge.l3tmp_smoother) {
        if (Knowledge.l3tmp_useSlotsForJac) {
          printer.println(s"Field Solution$postfix< $domain, BasicComm, 0.0 >[2]@(coarsest to ${Knowledge.l3tmp_tempBlockingMinLevel - 1})")
          if (Knowledge.l3tmp_tempBlockingMinLevel < Knowledge.maxLevel - 1)
            printer.println(s"Field Solution$postfix< $domain, CommFullTempBlockable, 0.0 >[2]@(${Knowledge.l3tmp_tempBlockingMinLevel} to (finest - 1))")
          printer.println(s"Field Solution$postfix< $domain, CommFullTempBlockable, $bc >[2]@finest")
        } else {
          printer.println(s"Field Solution$postfix< $domain, BasicComm, 0.0 >@(coarsest to ${Knowledge.l3tmp_tempBlockingMinLevel - 1})")
          if (Knowledge.l3tmp_tempBlockingMinLevel < Knowledge.maxLevel - 1)
            printer.println(s"Field Solution$postfix< $domain, CommFullTempBlockable, 0.0 >@(${Knowledge.l3tmp_tempBlockingMinLevel} to (finest - 1))")
          printer.println(s"Field Solution$postfix< $domain, CommFullTempBlockable, $bc >@finest")
          printer.println(s"Field Solution2$postfix< $domain, BasicComm, 0.0 >@(coarsest to ${Knowledge.l3tmp_tempBlockingMinLevel - 1})")
          if (Knowledge.l3tmp_tempBlockingMinLevel < Knowledge.maxLevel - 1)
            printer.println(s"Field Solution2$postfix< $domain, CommFullTempBlockable, 0.0 >@(${Knowledge.l3tmp_tempBlockingMinLevel} to (finest - 1))")
          printer.println(s"Field Solution2$postfix< $domain, CommFullTempBlockable, $bc >@finest")
        }
      } else {
        printer.println(s"Field Solution$postfix< $domain, BasicComm, 0.0 >@(coarsest to ${Knowledge.l3tmp_tempBlockingMinLevel - 1})")
        if (Knowledge.l3tmp_tempBlockingMinLevel < Knowledge.maxLevel - 1)
          printer.println(s"Field Solution$postfix< $domain, CommFullTempBlockable, 0.0 >@(${Knowledge.l3tmp_tempBlockingMinLevel} to (finest - 1))")
        printer.println(s"Field Solution$postfix< $domain, CommFullTempBlockable, $bc >@finest")
      }
    } else {
      if ("Jac" == Knowledge.l3tmp_smoother) {
        if (Knowledge.l3tmp_useSlotsForJac) {
          printer.println(s"Field Solution$postfix< $domain, BasicComm, 0.0 >[2]@(coarsest to ${Knowledge.l3tmp_tempBlockingMinLevel - 1})")
          printer.println(s"Field Solution$postfix< $domain, CommFullTempBlockable, 0.0 >[2]@(${Knowledge.l3tmp_tempBlockingMinLevel} to finest)")
        } else {
          printer.println(s"Field Solution$postfix< $domain, BasicComm, 0.0 >@(coarsest to ${Knowledge.l3tmp_tempBlockingMinLevel - 1})")
          printer.println(s"Field Solution$postfix< $domain, CommFullTempBlockable, 0.0 >@(${Knowledge.l3tmp_tempBlockingMinLevel} to finest)")
          printer.println(s"Field Solution2$postfix< $domain, BasicComm, 0.0 >@(coarsest to ${Knowledge.l3tmp_tempBlockingMinLevel - 1})")
          printer.println(s"Field Solution2$postfix< $domain, CommFullTempBlockable, 0.0 >@(${Knowledge.l3tmp_tempBlockingMinLevel} to finest)")
        }
      } else {
        printer.println(s"Field Solution$postfix< $domain, BasicComm, 0.0 >@(coarsest to ${Knowledge.l3tmp_tempBlockingMinLevel - 1})")
        printer.println(s"Field Solution$postfix< $domain, CommFullTempBlockable, 0.0 >@(${Knowledge.l3tmp_tempBlockingMinLevel} to finest)")
      }
    }

    if (Knowledge.l3tmp_kelvin && "" == postfix)
      printer.println(s"Field SolutionMean< $domain, NoComm, bcSol(xPos, yPos) >@finest")

    printer.println(s"Field Residual$postfix< $domain, BasicComm, None >@all")

    printer.println(s"Field RHS$postfix< $domain, NoComm, None >@(coarsest to ${Knowledge.l3tmp_tempBlockingMinLevel - 1})")
    if (Knowledge.l3tmp_kelvin && "_GMRF" == postfix) {
      if (Knowledge.l3tmp_tempBlockingMinLevel < Knowledge.maxLevel - 1)
        printer.println(s"Field RHS$postfix< $domain, CommPartTempBlockable, None >@(${Knowledge.l3tmp_tempBlockingMinLevel} to (finest - 1))")
      printer.println(s"Field RHS$postfix< $domain, CommPartTempBlockable, 0.0 >@finest")
    } else {
      printer.println(s"Field RHS$postfix< $domain, CommPartTempBlockable, None >@(${Knowledge.l3tmp_tempBlockingMinLevel} to finest)")
    }

    if ("CG" == Knowledge.l3tmp_cgs) {
      if (Knowledge.l3tmp_genVectorFields) {
        printer.println(s"Field VecP$postfix< $domain, BasicCommScalar, None >@coarsest")
        printer.println(s"Field VecGradP$postfix< $domain, NoCommScalar, None >@coarsest")
      } else {
        printer.println(s"Field VecP$postfix< $domain, BasicComm, None >@coarsest")
        printer.println(s"Field VecGradP$postfix< $domain, NoComm, None >@coarsest")
      }
    }
    printer.println
  }
}