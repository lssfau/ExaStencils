package exastencils.waLBerla.ir.blockforest

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceParameter
import exastencils.waLBerla.ir.refinement.IR_WaLBerlaInitNonuniformBlockForest
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_StructuredBlockForest
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_UintType
import exastencils.waLBerla.ir.util.IR_WaLBerlaDirection

case class IR_WaLBerlaBlockForest() extends IR_WaLBerlaInterfaceParameter(false, false, false) {

  def name : String = "blocks"

  override def ctorParameter : IR_FunctionArgument = IR_FunctionArgument(name, IR_ConstReferenceDatatype(datatype))
  override def resolveMemberBaseAccess() : IR_VariableAccess = IR_VariableAccess(resolveName(), datatype)
  private def resolveAccess() = resolveMemberBaseAccess()

  override def isPrivate : Boolean = true

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Access = resolveMemberBaseAccess()
  override def resolveDatatype() = IR_SharedPointerDatatype(WB_StructuredBlockForest)

  override def resolveDefValue() : Option[IR_Expression] = {
    val initFunc = if (Knowledge.waLBerla_useRefinement)
      IR_WaLBerlaInitNonuniformBlockForest()
    else
      IR_WaLBerlaInitUniformBlockForest()

    Some(IR_FunctionCall(initFunc.name))
  }

  def maxLevelWaLBerlaField = {
    if (IR_WaLBerlaFieldCollection.objects.nonEmpty)
      Some(IR_WaLBerlaFieldCollection.objects.maxBy(_.level))
    else
      None
  }

  // refinement level
  def getRefinementLvl(iterator : IR_WaLBerlaBlock) = IR_MemberFunctionCallArrow(resolveAccess(), "getLevel", IR_DerefAccess(iterator.access))

  def getLevelFromBlockId(id : IR_Expression) = IR_MemberFunctionCallArrow(resolveAccess(), "getLevelFromBlockId", id)

  // stepsize (potentially for a refinement level)
  def getStepSize(dim : Int, refinementLevel : Option[IR_Expression] = None) = IR_MemberFunctionCallArrowWithDt(resolveAccess(), s"d${ ('x' + dim).toChar.toString }", IR_RealDatatype, refinementLevel.toList : _*)
  def dx(refinementLevel : Option[IR_Expression] = None) = getStepSize(0, refinementLevel)
  def dy(refinementLevel : Option[IR_Expression] = None) = getStepSize(1, refinementLevel)
  def dz(refinementLevel : Option[IR_Expression] = None) = getStepSize(2, refinementLevel)

  // iterators
  def begin() = IR_MemberFunctionCallArrowWithDt(resolveAccess(), "begin", datatype)
  def end() = IR_MemberFunctionCallArrowWithDt(resolveAccess(), "end", datatype)

  def getNumberOfRootBlocksPerDim(d : Int) = IR_MemberFunctionCallArrow(resolveAccess(), "getSize", d)

  def getNumberOfAllRootBlocks() : IR_Expression = (0 until 3).map(d => getNumberOfRootBlocksPerDim(d) : IR_Expression).reduce(_ * _)

  def getNumberOfAllLocalBlocks() : IR_Expression = IR_MemberFunctionCallArrow(resolveAccess(), "getNumberOfBlocks")

  // cells
  def getNumberOfCellsPerBlock(dim : Int) : IR_Expression =
    IR_MemberFunctionCallArrowWithDt(resolveAccess(), s"getNumberOf${ ('X' + dim).toChar }CellsPerBlock", WB_UintType)

  def getNumberOfCellsForBlock(dim : Int, block : IR_WaLBerlaBlockLike) : IR_Expression =
    IR_MemberFunctionCallArrowWithDt(resolveAccess(), s"getNumberOf${ ('X' + dim).toChar }Cells", WB_UintType, IR_DerefAccess(block.access))

  def getAABBFromBlockId(inAABB : IR_Expression, id : IR_Expression) = IR_MemberFunctionCallArrow(resolveAccess(), "getAABBFromBlockId", inAABB, id)

  // domain border
  def isAtDomainBorder(dirArr : Array[Int]) = {
    val dir = IR_WaLBerlaDirection.getDirIndexFromArray(dirArr)
    if (IR_WaLBerlaDirection.isAxisDirection(dir)) {
      val borderName = IR_WaLBerlaDirection.stringFromDirection(dir) match {
        case "N" => "YMax"
        case "S" => "YMin"
        case "E" => "XMax"
        case "W" => "XMin"
        case "T" => "ZMax"
        case "B" => "ZMin"
      }
      IR_MemberFunctionCallArrowWithDt(resolveAccess(), s"atDomain${ borderName }Border", IR_BooleanDatatype, IR_DerefAccess(IR_WaLBerlaLoopOverLocalBlocks.block))
    } else {
      Logger.error("Unsupported direction for \"isAtDomainBorder\"")
    }
  }
}
