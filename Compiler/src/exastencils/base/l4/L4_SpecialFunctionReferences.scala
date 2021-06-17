package exastencils.base.l4

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

import exastencils.domain.ir.IR_ReadLineFromFile
import exastencils.globals.ir.IR_AllocateDataFunction

object L4_SpecialFunctionReferences {
  private var protectedFunctions : ListBuffer[String] = ListBuffer(
    "initGeometry", "initDomain", "initGlobals", "destroyGlobals", "InitFields", "initFieldsWithZero", IR_AllocateDataFunction.fctName, // (de-) init funcs
    "waitForFlag", "waitForMPIReq", "commFragOrderInternal", // comm funcs
    "stopTimer", "startTimer", "reduceTimers", "printAllTimers", "printAllTimersToFile", "getTotalTime", "getMeanTime", "getLastTime", // timer funcs
    "smallMatrixInversion", "inv_diagonal", "inv_schur", "inv_filled", "inv_blockdiagonal", "isOfStructure", // matrix funcs
    "cuda_std_fill", "cuda_std_copy", "cuda_std_swap", // cuda special functions
    "readStations", "readParameterFile", IR_ReadLineFromFile.name, // read input funcs
    "gen_printVal" // print funcs
  )

  private var protectedFunctionsRegex : ListBuffer[Regex] = ListBuffer(
    "set[A-Za-z]+".r, "get[A-Za-z]+".r,
    "LUSolve_[0-9]x[0-9]".r,
    "resizeInner_[A-Za-z]+_[A-Za-z]+_[0-9]".r, "resizeAllInner_[0-9]".r,
    "writeStations[0-9]+".r,
    "printField(?:[A-Za-z]+)?".r,
    "printVtk[A-Za-z]+".r, "readField[A-Za-z]+".r
  )

  def contains(funcName : String) = protectedFunctions.contains(funcName) || protectedFunctionsRegex.exists(_.pattern.matcher(funcName).matches())
}
