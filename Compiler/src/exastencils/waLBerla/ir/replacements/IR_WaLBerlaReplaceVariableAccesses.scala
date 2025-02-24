package exastencils.waLBerla.ir.replacements

import exastencils.base.ir._
import exastencils.datastructures.Transformation
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaCollection
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaFunction
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

object IR_WaLBerlaReplaceVariableAccesses extends IR_WaLBerlaReplacementStrategy("Find and append suffix") {

  this += Transformation("Replace", {
    case acc : IR_VariableAccess =>
      val isWaLBerlaVar = IR_WaLBerlaCollection.get.variables.contains(IR_VariableDeclaration(acc))
      val isWaLBerlaFuncionParam = {
        val enclosingWbFunc = collector.stack.collectFirst { case e : IR_WaLBerlaFunction => e }
        if (enclosingWbFunc.isDefined)
          enclosingWbFunc.get.parameters.exists(p => p.name == acc.name && p.datatype == acc.datatype)
        else
          false
      }

      if ( isWaLBerlaVar || isWaLBerlaFuncionParam )
        IR_VariableAccess(IR_WaLBerlaUtil.getGeneratedName(acc.name), acc.datatype)
      else
        acc
  })
}
