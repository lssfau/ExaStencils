package exastencils.datastructures.l3

import exastencils.knowledge._

object Fields {
  def solution(level : String, postfix : String = "") = {
    if (Knowledge.useVecFields)
      (0 until Knowledge.numVecDims).toArray.map(d => s"Solution${if (Knowledge.useSlotsForJac) "[0]" else ""}@$level[$d]")
    else
      Array(s"Solution$postfix${if (Knowledge.useSlotsForJac) "[0]" else ""}@$level")
  }
  def solution2(level : String, postfix : String = "") = {
    if (Knowledge.useVecFields)
      (0 until Knowledge.numVecDims).toArray.map(d => s"Solution${if (Knowledge.useSlotsForJac) "[1]" else "2"}@$level[$d]")
    else
      Array(s"Solution${if (Knowledge.useSlotsForJac) s"$postfix[1]" else s"2$postfix"}@$level")
  }
  def residual(level : String, postfix : String = "") = {
    if (Knowledge.useVecFields)
      (0 until Knowledge.numVecDims).toArray.map(d => s"Residual$postfix@$level[$d]")
    else
      Array(s"Residual$postfix@$level")
  }
  def rhs(level : String, postfix : String = "") = {
    if (Knowledge.useVecFields)
      (0 until Knowledge.numVecDims).toArray.map(d => s"RHS$postfix@$level[$d]")
    else
      Array(s"RHS$postfix@$level")
  }

  def addFields(printer : java.io.PrintWriter, postfix : String, domain : String) = {
    var fieldDatatype = (if (Knowledge.useVecFields) s"Array[Real][${Knowledge.numVecDims}]" else "Real")
    if (Knowledge.testBC || (Knowledge.kelvin && "" == postfix)) {
      var bc = (
        if (Knowledge.kelvin && "" == postfix) "bcSol(xPos, yPos)"
        else "sin ( M_PI * xPos ) * sinh ( M_PI * yPos )")
      if ("Jac" == Knowledge.smoother) {
        if (Knowledge.useSlotsForJac) {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >[2]@(coarsest to (finest - 1))")
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, $bc >[2]@finest")
        } else {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@(coarsest to (finest - 1))")
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, $bc >@finest")
          printer.println(s"Field Solution2$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@(coarsest to (finest - 1))")
          printer.println(s"Field Solution2$postfix< $fieldDatatype, $domain, BasicComm, $bc >@finest")
        }
      } else {
        printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@(coarsest to (finest - 1))")
        printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, $bc >@finest")
      }
    } else {
      if ("Jac" == Knowledge.smoother) {
        if (Knowledge.useSlotsForJac) {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >[2]@all")
        } else {
          printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@all")
          printer.println(s"Field Solution2$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@all")
        }
      } else {
        printer.println(s"Field Solution$postfix< $fieldDatatype, $domain, BasicComm, 0.0 >@all")
      }
    }

    if (Knowledge.kelvin && "" == postfix)
      printer.println(s"Field SolutionMean< $fieldDatatype, $domain, NoComm, bcSol(xPos, yPos) >@all")

    printer.println(s"Field Residual$postfix< $fieldDatatype, $domain, BasicComm, None >@all")
    if (Knowledge.kelvin && "_GMRF" == postfix) {
      printer.println(s"Field RHS$postfix< $fieldDatatype, $domain, NoComm, 0.0 >@finest")
      printer.println(s"Field RHS$postfix< $fieldDatatype, $domain, NoComm, None >@(coarsest to (finest - 1))")
    } else
      printer.println(s"Field RHS$postfix< $fieldDatatype, $domain, NoComm, None >@all")
    if ("CG" == Knowledge.cgs) {
      printer.println(s"Field VecP$postfix< $fieldDatatype, $domain, BasicComm, None >@coarsest")
      printer.println(s"Field VecGradP$postfix< $fieldDatatype, $domain, NoComm, None >@coarsest")
    }
    printer.println
  }
}