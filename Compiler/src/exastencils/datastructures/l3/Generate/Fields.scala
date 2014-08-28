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
    var fieldDatatype = (if (Knowledge.l3tmp_genVectorFields) s"Array[Real][${Knowledge.l3tmp_numVecDims}]" else "Real")
    if (Knowledge.l3tmp_genFunctionBC || (Knowledge.l3tmp_kelvin && "" == postfix)) {
      var bc = (
        if (Knowledge.l3tmp_kelvin && "" == postfix) "bcSol(xPos, yPos)"
        else "sin ( M_PI * xPos ) * sinh ( M_PI * yPos )")
      if ("Jac" == Knowledge.l3tmp_smoother) {
        if (Knowledge.l3tmp_useSlotsForJac) {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, CommFullTempBlockable, 0.0 >[2]@(coarsest to (finest - 1))")
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, CommFullTempBlockable, $bc >[2]@finest")
        } else {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, CommFullTempBlockable, 0.0 >@(coarsest to (finest - 1))")
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, CommFullTempBlockable, $bc >@finest")
          printer.println(s"Field Solution2$postfix< $fieldDatatype, $domain, CommFullTempBlockable, 0.0 >@(coarsest to (finest - 1))")
          printer.println(s"Field Solution2$postfix< $fieldDatatype, $domain, CommFullTempBlockable, $bc >@finest")
        }
      } else {
        printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, CommFullTempBlockable, 0.0 >@(coarsest to (finest - 1))")
        printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, CommFullTempBlockable, $bc >@finest")
      }
    } else {
      if ("Jac" == Knowledge.l3tmp_smoother) {
        if (Knowledge.l3tmp_useSlotsForJac) {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, CommFullTempBlockable, 0.0 >[2]@all")
        } else {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, CommFullTempBlockable, 0.0 >@all")
          printer.println(s"Field Solution2$postfix< $fieldDatatype, $domain, CommFullTempBlockable, 0.0 >@all")
        }
      } else {
        printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, CommFullTempBlockable, 0.0 >@all")
      }
    }

    if (Knowledge.l3tmp_kelvin && "" == postfix)
      printer.println(s"Field SolutionMean< $fieldDatatype, $domain, NoComm, bcSol(xPos, yPos) >@all")

    val rhsLayout = if (Knowledge.l3tmp_genTemporalBlocking) "CommPartTempBlockable" else "NoComm"
    printer.println(s"Field Residual$postfix< $fieldDatatype, $domain, BasicComm, None >@all")
    if (Knowledge.l3tmp_kelvin && "_GMRF" == postfix) {
      printer.println(s"Field RHS$postfix< $fieldDatatype, $domain, $rhsLayout, 0.0 >@finest")
      printer.println(s"Field RHS$postfix< $fieldDatatype, $domain, $rhsLayout, None >@(coarsest to (finest - 1))")
    } else
      printer.println(s"Field RHS$postfix< $fieldDatatype, $domain, $rhsLayout, None >@all")
    if ("CG" == Knowledge.l3tmp_cgs) {
      printer.println(s"Field VecP$postfix< $fieldDatatype, $domain, BasicComm, None >@coarsest")
      printer.println(s"Field VecGradP$postfix< $fieldDatatype, $domain, NoComm, None >@coarsest")
    }
    printer.println
  }
}