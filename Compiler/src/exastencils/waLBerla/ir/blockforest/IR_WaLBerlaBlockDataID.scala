package exastencils.waLBerla.ir.blockforest

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_StdArrayDatatype
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceParameter
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil.getGeneratedName

case class IR_WaLBerlaBlockDataID(var wbField : IR_WaLBerlaField, var slot : IR_Expression) extends IR_WaLBerlaInterfaceParameter {

  def name = wbField.name + "_ID"

  override def datatype : IR_Datatype = {
    var dt : IR_Datatype = WB_BlockDataID

    if (numSlots > 1)
      dt = IR_StdArrayDatatype(dt, numSlots)
    if (levels.size > 1)
      dt = IR_StdArrayDatatype(dt, levels.size)

    dt
  }

  var level = wbField.level
  val numSlots = wbField.numSlots
  val levels = IR_WaLBerlaFieldCollection.getAllByIdentifier(wbField.name).map(_.level)

  override def resolveAccess() = {
    var access : IR_Access = member

    if (levels.size > 1)
      access = IR_ArrayAccess(access, level - levels.min)
    if (numSlots > 1)
      access = IR_ArrayAccess(access, slot)

    access
  }

  override def ctorParameter : IR_FunctionArgument = IR_FunctionArgument(name, datatype)
  override def member : IR_VariableAccess = IR_VariableAccess(getGeneratedName(name), datatype)
}
