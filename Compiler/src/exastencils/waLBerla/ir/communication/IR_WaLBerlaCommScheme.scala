package exastencils.waLBerla.ir.communication

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_StdArrayDatatype
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField

trait IR_WaLBerlaCommScheme extends IR_Access{
  def wbField : IR_WaLBerlaField
  def slot : IR_Expression

  def blockDataID : IR_WaLBerlaBlockDataID
  def blockForest : IR_WaLBerlaBlockForest
  def name : String

  def baseAccess() = IR_VariableAccess(name, datatype)

  def level = wbField.level
  def numSlots = wbField.numSlots
  def levels = blockDataID.levels

  def basetype : IR_Datatype
  def datatype : IR_Datatype = {
    var dt : IR_Datatype = basetype

    if (numSlots > 1)
      dt = IR_StdArrayDatatype(dt, numSlots)
    if (levels.size > 1)
      dt = IR_StdArrayDatatype(dt, levels.size)

    dt
  }

  def createUniformPackInfo() : IR_Expression

  def addPackInfo() = IR_MemberFunctionCallArrow(resolveAccess(), "addPackInfo", createUniformPackInfo())

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
