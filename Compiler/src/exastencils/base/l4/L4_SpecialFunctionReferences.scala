package exastencils.base.l4

import exastencils.globals.ir.IR_AllocateDataFunction
import exastencils.logger.Logger

object L4_SpecialFunctionReferences {

  // regex definitions
  val luSolve = "LUSolve_[0-9]+x[0-9]+".r
  val setExt = "set([A-Za-z]+)".r
  val getExt = "get([A-Za-z]+)".r

  def contains(funcName : String) = {

    var ret = true
    funcName match {
      // (de-) init funcs
      case "initGeometry" | "initDomain" | "initGlobals" | "destroyGlobals" | "initFieldsWithZero" | IR_AllocateDataFunction.fctName =>
      // comm funcs
      case "waitForFlag" | "waitForMPIReq" | "commFragOrderInternal" =>
      // timer funcs
      case "stopTimer" | "startTimer" | "reduceTimers" | "printAllTimers" | "printAllTimersToFile" | "getTotalTime" | "getMeanTime" | "getLastTime" =>
      // util funcs
      case "benchmarkStart" | "benchmarkStop" | "clearCharacteristics" | "logCharacteristics" | "printJSON" | "printWithReducedPrec" =>
      // matrix funcs
      case "smallMatrixInversion" | "inv_diagonal" | "inv_schur" | "inv_filled" | "inv_blockdiagonal" | "isOfStructure" | luSolve() =>
      // cuda special funcs
      case "cuda_std_fill" | "cuda_std_copy" | "cuda_std_swap" =>
      // read input funcs
      case "readStations" | "readParameterFile" | "readLine" =>
      case s : String if s.startsWith("readField")           =>
      // print funcs
      case s : String if s == "gen_printVal" ||  s.startsWith("printVtk") || s.startsWith("printField") || s.startsWith("writeStations") =>
      // misc
      case s : String if s.startsWith("resizeAllInner") || s.startsWith("resizeInner") =>
      // ext fields
      case setExt(_) | getExt(_) =>
        Logger.warn(s"Function name $funcName might coincide with a getter/setter function for an external field.")
        ret = false

      case _ =>
        ret = false
    }

    ret
  }
}
