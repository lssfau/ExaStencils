package exastencils.waLBerla.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_StructuredBlockForest

case class IR_WaLBerlaBlockForest() extends IR_WaLBerlaInterfaceParameter {

  override def name = "blocks"
  override def datatype = IR_SharedPointerDatatype(WB_StructuredBlockForest)

  override def ctorParameter : IR_FunctionArgument = IR_FunctionArgument(name, IR_ConstReferenceDatatype(datatype))
  override def member : IR_VariableAccess = IR_VariableAccess(IR_WaLBerlaUtil.getGeneratedName(name), datatype)

  override def resolveAccess() : IR_Access = member

  def maxLevelWaLBerlaField = {
    if (IR_WaLBerlaFieldCollection.objects.nonEmpty)
      Some(IR_WaLBerlaFieldCollection.objects.maxBy(_.level))
    else
      None
  }

  // iterators
  def iterator = new IR_WaLBerlaBlock("block", IR_SpecialDatatype("auto"))
  def begin() = IR_MemberFunctionCallArrow(resolveAccess(), "begin", datatype)
  def end() = IR_MemberFunctionCallArrow(resolveAccess(), "end", datatype)

  // blocks
  def getBlocks(targetVector : IR_VariableAccess) = {
    IR_MemberFunctionCallArrow(resolveAccess(), "getBlocks", IR_UnitDatatype, targetVector, /* level */ 0) // TODO: adjust when refinement is supported
  }

  // aabb
  def aabbDatatype = IR_SpecialDatatype("math::AABB")

  def getCellAABB(idx : IR_ExpressionIndex) =
    IR_MemberFunctionCallArrow(resolveAccess(), "getBlockLocalCellAABB", aabbDatatype,
      IR_DerefAccess(IR_WaLBerlaLoopOverBlocks.defIt), IR_FunctionCall(IR_ExternalFunctionReference("Cell"), idx.indices.padTo(3, 0 : IR_Expression) : _*))

  // domain border
  def isAtDomainBorder(dirArr: Array[Int]) = {
    val dir = IR_WaLBerlaDirection.getDirnameFromArray(dirArr)
    if (IR_WaLBerlaDirection.isAxisDirection(dir)) {
      val borderName = IR_WaLBerlaDirection.stringFromDirection(dir) match {
        case "N" => "YMax"
        case "S" => "YMin"
        case "E" => "XMax"
        case "W" => "XMin"
        case "T" => "ZMax"
        case "B" => "ZMin"
      }
      IR_MemberFunctionCallArrow(resolveAccess(), s"atDomain${borderName}Border", IR_BooleanDatatype, IR_DerefAccess(IR_WaLBerlaLoopOverBlocks.defIt))
    } else {
      Logger.error("Unsupported direction for \"isAtDomainBorder\"")
    }
  }
}
