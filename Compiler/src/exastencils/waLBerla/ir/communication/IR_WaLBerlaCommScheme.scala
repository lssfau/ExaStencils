package exastencils.waLBerla.ir.communication

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_StdArrayDatatype
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_CommScheme
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_FieldDatatype
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil.getGeneratedName
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil.make_shared

case class IR_WaLBerlaCommScheme(var wbField : IR_WaLBerlaField, var slot : IR_Expression) extends IR_Access {

  private val blockDataID = IR_WaLBerlaBlockDataID(wbField, slot)
  private val blockForest = IR_WaLBerlaBlockForest()

  var level = wbField.level
  val numSlots = wbField.numSlots
  val levels = blockDataID.levels

  def basetype = IR_UniquePointerDatatype(WB_CommScheme)
  def datatype : IR_Datatype = {
    var dt : IR_Datatype = basetype

    if (numSlots > 1)
      dt = IR_StdArrayDatatype(dt, numSlots)
    if (levels.size > 1)
      dt = IR_StdArrayDatatype(dt, levels.size)

    dt
  }

  def addPackInfo() = IR_MemberFunctionCallArrow(resolveAccess(), "addPackInfo", createUniformPackInfo())

  def createUniformPackInfo() =
    make_shared(s"field::communication::PackInfo< ${ WB_FieldDatatype(wbField).prettyprint() } >", blockDataID)

  def baseAccess() = IR_VariableAccess(name, datatype)

  def name = getGeneratedName(s"commScheme_${ wbField.name }")

  def resolveAccess() = {
    var access : IR_Access = baseAccess()

    if (levels.size > 1)
      access = IR_ArrayAccess(access, level - levels.min)
    if (numSlots > 1)
      access = IR_ArrayAccess(access, slot)

    access
  }

  def communicate : IR_Statement = IR_MemberFunctionCallArrow(resolveAccess(), "communicate")

  override def prettyprint(out : PpStream) : Unit = out << baseAccess()
}
