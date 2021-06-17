package exastencils.base.l4

import exastencils.globals.ir.IR_AllocateDataFunction

object L4_SpecialFunctionReferences {

  def contains(funcName : String) = {

    // regex defs
    val luSolve = "LUSolve_[0-9]+x[0-9]+".r
    val setExt = "set([A-Za-z]+)".r
    val getExt = "get([A-Za-z]+)".r

    var ret = true
    funcName match {
      // (de-) init funcs
      case "initGeometry" | "initDomain" | "initGlobals" | "destroyGlobals" | "initFieldsWithZero" | IR_AllocateDataFunction.fctName =>
      // comm funcs
      case "waitForFlag" | "waitForMPIReq" | "commFragOrderInternal" =>
      // timer funcs
      case "stopTimer" | "startTimer" | "reduceTimers" | "printAllTimers" | "printAllTimersToFile" | "getTotalTime" | "getMeanTime" | "getLastTime" =>
      // matrix funcs
      case "smallMatrixInversion" | "inv_diagonal" | "inv_schur" | "inv_filled" | "inv_blockdiagonal" | "isOfStructure" | luSolve() =>
      // cuda special funcs
      case "cuda_std_fill" | "cuda_std_copy" | "cuda_std_swap" =>
      // read input funcs
      case "readStations" | "readParameterFile" | "readLine" =>
      case s : String if s.startsWith("readField")           =>
      // print funcs
      case s : String if s == "gen_printVal" ||  s.startsWith("printVtk") || s.startsWith("printField") || s.startsWith("writeStations") =>
      // ext fields
      // TODO: does not work as ext field decls are not processed at this point
      //case setExt(fieldName) if L4_ExternalFieldCollection.exists(fieldName) =>
      //case getExt(fieldName) if L4_ExternalFieldCollection.exists(fieldName) =>
      // misc
      case s : String if s.startsWith("resizeAllInner") || s.startsWith("resizeInner") =>

      case _ =>
        ret = false
    }
    ret
  }
}
