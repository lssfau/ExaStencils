package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_StdArrayDatatype
import exastencils.optimization.ir.EvaluationException
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceParameter
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil.getGeneratedName

case class IR_WaLBerlaBlockDataID(var wbField : IR_WaLBerlaField, var slot : IR_Expression, var onGPU : Boolean) extends IR_WaLBerlaInterfaceParameter {

  def name = wbField.name + "_ID" + (if (onGPU) "_GPU" else "")

  override def datatype : IR_Datatype = {
    var dt : IR_Datatype = WB_BlockDataID

    if (numSlots > 1)
      dt = IR_StdArrayDatatype(dt, numSlots)
    if (levels.size > 1)
      dt = IR_StdArrayDatatype(dt, levels.size)

    dt
  }

  var level : IR_Expression = wbField.level
  val numSlots : Int = wbField.numSlots
  val levels : ListBuffer[Int] = IR_WaLBerlaFieldCollection.getAllByIdentifier(wbField.name, suppressError = true).map(_.level)

  override def resolveAccess() = {
    var access : IR_Access = member

    if (levels.size > 1) {
      val simplifiedLvlIdx = try {
        IR_SimplifyExpression.simplifyIntegralExpr(level - levels.min)
      } catch {
        case _ : EvaluationException => level - levels.min
      }
      access = IR_ArrayAccess(access, simplifiedLvlIdx)
    }
    if (numSlots > 1) {
      access = IR_ArrayAccess(access, slot)
    }

    access
  }

  override def ctorParameter : IR_FunctionArgument = IR_FunctionArgument(name, datatype)
  override def member : IR_VariableAccess = IR_VariableAccess(getGeneratedName(name), datatype)
}
