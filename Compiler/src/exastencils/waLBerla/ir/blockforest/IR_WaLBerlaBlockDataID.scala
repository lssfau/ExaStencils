package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_StdArrayDatatype
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.gpu.GPU_WaLBerlaAddGPUFieldToStorage
import exastencils.waLBerla.ir.field.IR_WaLBerlaAddFieldToStorage
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceParameter
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_BlockDataID

case class IR_WaLBerlaBlockDataID(
    var wbField : IR_WaLBerlaField,
    var slot : IR_Expression,
    var onGPU : Boolean
) extends IR_WaLBerlaInterfaceParameter(false, true, false) {

  def name = wbField.name + "_ID" + (if (onGPU) "_GPU" else "")

  override def resolveDatatype() : IR_Datatype = {
    var dt : IR_Datatype = WB_BlockDataID

    if (numSlots > 1)
      dt = IR_StdArrayDatatype(dt, numSlots)

    dt
  }

  // IR_WaLBerlaAdd(GPU)FieldToStorage initializes all slots and levels
  override def getCtor() : Option[IR_Statement] = Some(
    if (onGPU)
      GPU_WaLBerlaAddGPUFieldToStorage(wbField).expandSpecial()
    else
      IR_WaLBerlaAddFieldToStorage(wbField).expandSpecial()
  )

  var level : IR_Expression = wbField.level
  val numSlots : Int = wbField.numSlots
  val levels : ListBuffer[Int] = IR_WaLBerlaFieldCollection.getAllByIdentifier(wbField.name, suppressError = true).map(_.level)

  override def minLevel : Int = levels.min
  override def maxLevel : Int = levels.max

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveMemberBaseAccess(), IR_NullExpression, level, IR_NullExpression)

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, level : IR_Expression, neigh : IR_Expression) = {
    var baseAccess : IR_Access = resolveMemberBaseAccess()
    var access = super.resolveAccess(baseAccess, fragment, level, level)

    if (numSlots > 1) {
      access = IR_ArrayAccess(access, slot)
    }

    access
  }

  private val blockforest = IR_WaLBerlaBlockForest()

  override def isPrivate : Boolean = true

  override def resolveDefValue() = None
}
