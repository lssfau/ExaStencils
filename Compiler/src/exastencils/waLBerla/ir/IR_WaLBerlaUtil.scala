package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_LeveledFunction
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid.ir.IR_VirtualFieldAccess
import exastencils.util.ir.IR_CollectFieldAccesses

object IR_WaLBerlaUtil extends DefaultStrategy("Get waLBerla sweep") {
  def isWaLBerlaKernel(func : IR_Function) : Boolean = func.name.startsWith("walberla_")
  var startNode : Option[IR_LeveledFunction] = None
  var fieldAccesses : ListBuffer[IR_FieldAccess] = ListBuffer()
  var vfieldAccesses : ListBuffer[IR_VirtualFieldAccess] = ListBuffer()

  this += Transformation("Get sweep node", {
    case func : IR_LeveledFunction if isWaLBerlaKernel(func) =>
      startNode = Some(func)

      // get field accesses
      IR_CollectFieldAccesses.applyStandalone(func.body)
      fieldAccesses ++= Duplicate(IR_CollectFieldAccesses.fieldAccesses).groupBy(_.name).map(_._2.head)
      vfieldAccesses ++= Duplicate(IR_CollectFieldAccesses.vFieldAccesses).groupBy(_.name).map(_._2.head)

      func
  })
}
