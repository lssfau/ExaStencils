package exastencils.baseExt.ir.IR_MatOperations

import exastencils.config.Knowledge
import exastencils.logger.Logger

// evaluate for each inversion/solution of a local system if it should be executed at runtime/compiletime

object IR_EvalMOpRuntimeExe {
  def apply(problem : String, m : Int, isConst : Boolean = false): String = {
      // knowledge flags still count if experimental_evalMOpRuntimeExe is not set
      if(!Knowledge.experimental_evalMOpRuntimeExe) {
          if(problem == "inverse") {
            Knowledge.experimental_resolveInverseFunctionCall
          } else if(problem == "localsystem") {
            Knowledge.experimental_resolveLocalMatSys
          } else Logger.error("unexpected problem type")
      } else {
            if(isConst) {
              // constant matrix problems can always be calculated at compiletime
              "Compiletime"
            } else {
              // otherwise decide based on size
              if(m > Knowledge.experimental_MOpRTExeThreshold) "Runtime"
              else "Compiletime"
            }
      }
  }
}
