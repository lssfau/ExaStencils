package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_LeveledFunction
import exastencils.base.ir.IR_Statement
import exastencils.core.Duplicate
import exastencils.field.ir.IR_FieldAccess

// store context
case class IR_WaLBerlaSweepGenerationContext(func : IR_LeveledFunction)  {

  // get fields accessed in sweep
  private val fAcc = Duplicate(IR_WaLBerlaUtil.fieldAccesses)
  private val vfAcc = Duplicate(IR_WaLBerlaUtil.vfieldAccesses) // TODO handle these too

  var fieldAccesses : ListBuffer[IR_FieldAccess] = fAcc
  var parameters : ListBuffer[IR_FunctionArgument] = func.parameters
  var body : ListBuffer[IR_Statement] = func.body

}
