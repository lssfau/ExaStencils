package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_LeveledFunction
import exastencils.base.ir.IR_Statement

// store context

// TODO offload work from IR_WaLBerlaSweep to this class for better separation
case class IR_WaLBerlaSweepGenerationContext(func : IR_LeveledFunction)  {

  var fields : ListBuffer[IR_WaLBerlaField] = IR_WaLBerlaFieldCollection.objects
  var parameters : ListBuffer[IR_FunctionArgument] = func.parameters
  var body : ListBuffer[IR_Statement] = func.body
}
