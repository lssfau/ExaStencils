package exastencils.waLBerla.ir.blockforest

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection
import exastencils.waLBerla.ir.grid.IR_WaLBerlaCellAABB
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceParameter
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_StructuredBlockForest
import exastencils.waLBerla.ir.util.IR_WaLBerlaDirection
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

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
  def begin() = IR_MemberFunctionCallArrowWithDt(resolveAccess(), "begin", datatype)
  def end() = IR_MemberFunctionCallArrowWithDt(resolveAccess(), "end", datatype)

  // blocks
  def getBlocks(targetVector : IR_VariableAccess) = {
    IR_MemberFunctionCallArrow(resolveAccess(), "getBlocks", targetVector, /* level */ 0) // TODO: adjust when refinement is supported
  }

  def getNumberOfRootBlocks(d : Int) = IR_MemberFunctionCallArrow(resolveAccess(), "getSize", d)

  def getNumberOfAllRootBlocks() : IR_Expression = (0 until 3).map(d => getNumberOfRootBlocks(d) : IR_Expression).reduce(_ * _)

  // cells
  def getNumberOfCells(dim : Int) : IR_Expression =
    IR_MemberFunctionCallArrowWithDt(resolveAccess(), s"getNumberOf${ ('X' + dim).toChar }Cells", IR_SpecialDatatype("uint_t"))

  def getNumberOfCells(dim : Int, block : IR_WaLBerlaBlock) : IR_Expression =
    IR_MemberFunctionCallArrowWithDt(resolveAccess(), s"getNumberOf${ ('X' + dim).toChar }Cells", IR_SpecialDatatype("uint_t"), IR_DerefAccess(block))

  // aabb
  def getCellAABB(idx : IR_ExpressionIndex) = IR_WaLBerlaCellAABB(this, idx)

  // domain border
  def isAtDomainBorder(dirArr : Array[Int]) = {
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
      IR_MemberFunctionCallArrowWithDt(resolveAccess(), s"atDomain${ borderName }Border", IR_BooleanDatatype, IR_DerefAccess(IR_WaLBerlaLoopOverBlocks.defIt))
    } else {
      Logger.error("Unsupported direction for \"isAtDomainBorder\"")
    }
  }
}
