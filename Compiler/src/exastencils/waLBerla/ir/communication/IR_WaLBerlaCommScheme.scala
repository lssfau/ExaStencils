package exastencils.waLBerla.ir.communication

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_StdArrayDatatype
import exastencils.config.Knowledge
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

trait IR_WaLBerlaCommScheme extends IR_WaLBerlaInterfaceMember {
  def wbField : IR_WaLBerlaField
  def slot : IR_Expression

  def blockDataID : IR_WaLBerlaBlockDataID
  def blockForest : IR_WaLBerlaBlockForest
  def name : String

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

  override def resolveAccess() = {
    var access : IR_Access = IR_VariableAccess(IR_WaLBerlaUtil.getGeneratedName(name), datatype)

    if (levels.size > 1)
      access = IR_ArrayAccess(access, level - levels.min)
    if (numSlots > 1)
      access = IR_ArrayAccess(access, slot)

    access
  }

  def comnSchemeNecessaryWrapper(stmts : ListBuffer[IR_Statement]) : IR_IfCondition =
    if (Knowledge.waLBerla_useGridFromExa)
      IR_IfCondition(Knowledge.domain_numFragmentsTotal > 1, stmts)
    else
      IR_IfCondition(blockForest.getNumberOfAllRootBlocks() > 1, stmts)

  def communicate() : IR_Statement = {
    val comm = IR_MemberFunctionCallArrow(resolveAccess(), "communicate")
    comnSchemeNecessaryWrapper(ListBuffer(comm))
  }
}
