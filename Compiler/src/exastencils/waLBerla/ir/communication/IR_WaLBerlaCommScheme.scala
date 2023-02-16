package exastencils.waLBerla.ir.communication

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_StdArrayDatatype
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember

abstract class IR_WaLBerlaCommScheme extends IR_WaLBerlaInterfaceMember(false, true, false) {
  def wbField : IR_WaLBerlaField
  def slot : IR_Expression

  def blockDataID : IR_WaLBerlaBlockDataID
  def blockForest : IR_WaLBerlaBlockForest

  def level = wbField.level
  def numSlots = wbField.numSlots
  def levels = blockDataID.levels
  override def numLevels = blockDataID.numLevels

  def basetype : IR_Datatype

  override def resolveDatatype() : IR_Datatype = {
    var dt : IR_Datatype = basetype

    if (numSlots > 1)
      dt = IR_StdArrayDatatype(dt, numSlots)

    dt
  }

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess()

  private def resolveAccess() : IR_Expression = resolveAccess(resolveMemberBaseAccess(), IR_NullExpression, level, IR_NullExpression)

  override def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(datatype, resolveName())

  def createUniformPackInfo() : IR_Expression

  def addPackInfo() = IR_MemberFunctionCallArrow(resolveAccess(), "addPackInfo", createUniformPackInfo())

  override def resolveAccess(baseAccess : IR_Expression, block : IR_Expression, level : IR_Expression, neigh : IR_Expression) = {
    var baseAccess : IR_Access = IR_VariableAccess(resolveName(), datatype)
    var access = super.resolveAccess(baseAccess, IR_NullExpression, level, IR_NullExpression)

    if (numSlots > 1)
      access = IR_ArrayAccess(access, slot)

    access
  }

  def comnSchemeNecessaryWrapper(stmts : ListBuffer[IR_Statement]) : IR_IfCondition = {
    if (Knowledge.waLBerla_useGridFromExa)
      IR_IfCondition(Knowledge.domain_numFragmentsTotal > 1, stmts)
    else
      IR_IfCondition(blockForest.getNumberOfAllRootBlocks() > 1, stmts)
  }

  def communicate() : IR_Statement = {
    val comm = IR_MemberFunctionCallArrow(resolveAccess(), "communicate")
    comnSchemeNecessaryWrapper(ListBuffer(comm))
  }
}
