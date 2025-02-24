package exastencils.waLBerla.ir.field

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.ir.IR_FieldLayout
import exastencils.field.ir.IR_FieldLayoutPerDim
import exastencils.fieldlike.ir.IR_FieldLikeLayout
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_Localization
import exastencils.logger.Logger

case class IR_WaLBerlaFieldLayout(
    var name : String,
    var level : Int,
    var numDimsGrid : Int,
    var numDimsData : Int,
    var referenceOffset : IR_ExpressionIndex,
    var datatype : IR_Datatype,
    var layoutsPerDim : Array[IR_FieldLayoutPerDim],
    var layoutName : String, // "xyzf" or "fzyx"
    var communicatesDuplicated : Boolean,
    var communicatesGhosts : Boolean
) extends IR_FieldLikeLayout {

  override def createDuplicate() : IR_WaLBerlaFieldLayout = {
    IR_WaLBerlaFieldLayout.tupled(Duplicate(IR_WaLBerlaFieldLayout.unapply(this).get))
  }

  var localization : IR_Localization = IR_AtCellCenter

  lazy val wbField : IR_WaLBerlaField = IR_WaLBerlaFieldCollection.getByLayoutIdentifierLevExp(name, level, suppressError = true).get

  // layout assumed to be equal for slotted and CPU/GPU variants
  private def getField = IR_IV_WaLBerlaGetField(wbField, 0, onGPU = false)

  // layouts are identical for each slot: use "0" as default
  private def callMemberFunc(name : String) = IR_MemberFunctionCallArrowWithDt(getField, name, IR_IntegerDatatype)

  private def callMemberFuncForDim(name : String, dim : Int) = {
    // TODO handling layout transformations ?
    var newLayout = Duplicate(layoutName)
    for (d <- 0 until 3 - numDimsGrid)
      newLayout = newLayout.replace(('z' - d).toChar.toString, "")
    if (numDimsData == numDimsGrid)
      newLayout = newLayout.replace("f", "")
    val prefix = dim match {
      case d if d < newLayout.length => newLayout.reverse(d) // rightmost is innermost dim
    }
    IR_MemberFunctionCallArrowWithDt(getField, prefix + name, IR_IntegerDatatype)
  }

  private def numPadLayersLeft(dim : Int) : IR_Expression = 0

  private def numGhostLayersLeft(dim : Int) = IR_Cast(IR_IntegerDatatype, callMemberFunc("nrOfGhostLayers"))

  private def numDupLayersLeft(dim : Int) : IR_Expression = 0 // cell-centered

  private def numInnerLayers(dim : Int) = numInner(dim)

  private def numDupLayersRight(dim : Int) : IR_Expression = 0 // cell-centered

  private def numGhostLayersRight(dim : Int) = IR_Cast(IR_IntegerDatatype, callMemberFunc("nrOfGhostLayers"))

  private def numPadLayersRight(dim : Int) = numPad(dim) // at the end of each coordinate

  private def numDup(d : Int) = numDupLayersLeft(d) + numDupLayersRight(d)

  private def numGhost(d : Int) = numGhostLayersLeft(d) + numGhostLayersRight(d)

  private def numInner(d : Int) = IR_Cast(IR_IntegerDatatype, callMemberFuncForDim("Size", d))

  private def numPad(d : Int) = IR_Cast(IR_IntegerDatatype, callMemberFuncForDim("AllocSize", d) - numInner(d) - numDup(d) - numGhost(d))

  def useFixedLayoutSizes = {
    if (Knowledge.waLBerla_useFixedLayoutsFromExa && layoutName != "fzyx")
      Logger.error("Cannot use fixed layout sizes (cf. waLBerla_useFixedLayoutsFromExa) for AoS memory layouts (\"zyxf\"). This must be done with layout transformations.")

    Knowledge.waLBerla_useFixedLayoutsFromExa && layoutName == "fzyx"
  }

  // fixed iteration spaces handled by a regular exastencils field layout proxy
  private val regularLayout = IR_FieldLayout(name, level, datatype, localization, layoutsPerDim, numDimsGrid, referenceOffset, communicatesDuplicated, communicatesGhosts)

  def defIdxInnerBegin(dim : Int) : IR_Expression = 0
  def defIdxInnerEnd(dim : Int) : IR_Expression = defIdxInnerBegin(dim) + numInnerLayers(dim)

  def defIdxDupLeftEnd(dim : Int) : IR_Expression = defIdxInnerBegin(dim)
  def defIdxDupLeftBegin(dim : Int) : IR_Expression = defIdxInnerBegin(dim) - numDupLayersLeft(dim)

  def defIdxGhostLeftEnd(dim : Int) : IR_Expression = defIdxDupLeftBegin(dim)
  def defIdxGhostLeftBegin(dim : Int) : IR_Expression = defIdxDupLeftBegin(dim) - numGhostLayersLeft(dim)

  def defIdxPadLeftBegin(dim : Int) : IR_Expression = defIdxDupLeftBegin(dim) - numGhostLayersLeft(dim) - numPadLayersLeft(dim)
  def defIdxPadLeftEnd(dim : Int) : IR_Expression = defIdxPadLeftBegin(dim) + numPadLayersLeft(dim)

  def defIdxDupRightBegin(dim : Int) : IR_Expression = defIdxInnerEnd(dim)
  def defIdxDupRightEnd(dim : Int) : IR_Expression = defIdxDupRightBegin(dim) + numDupLayersRight(dim)

  def defIdxGhostRightBegin(dim : Int) : IR_Expression = defIdxDupRightEnd(dim)
  def defIdxGhostRightEnd(dim : Int) : IR_Expression = defIdxGhostRightBegin(dim) + numGhostLayersRight(dim)

  def defIdxPadRightBegin(dim : Int) : IR_Expression = defIdxGhostRightEnd(dim)
  def defIdxPadRightEnd(dim : Int) : IR_Expression = defIdxPadRightBegin(dim) + numPadLayersRight(dim)

  def defTotalFixed(dim : Int) : Int = if (!useFixedLayoutSizes && dim < Knowledge.dimensionality)
    Logger.error("Using fixed-size indexing for field with variable size")
  else
    regularLayout.defTotalFixed(dim)

  def defTotalExpr(dim : Int) : IR_Expression = IR_Cast(IR_IntegerDatatype, callMemberFuncForDim("AllocSize", dim))

  def defIdxByIdFixed(id : String, dim : Int) : Int = {
    if (!useFixedLayoutSizes && dim < Knowledge.dimensionality)
      Logger.error("Using fixed-size indexing for field layouts with variable sizes")

    id match {
      case "PLB"                => regularLayout.defIdxPadLeftBegin(dim)
      case "PLE"                => regularLayout.defIdxPadLeftEnd(dim)
      case "GLB"                => regularLayout.defIdxGhostLeftBegin(dim)
      case "GLE"                => regularLayout.defIdxGhostLeftEnd(dim)
      case "DLB"                => regularLayout.defIdxDupLeftBegin(dim)
      case "DLE"                => regularLayout.defIdxDupLeftEnd(dim)
      case "IB" | "ILB" | "IRB" => regularLayout.defIdxInnerBegin(dim)
      case "IE" | "ILE" | "IRE" => regularLayout.defIdxInnerEnd(dim)
      case "DRB"                => regularLayout.defIdxDupRightBegin(dim)
      case "DRE"                => regularLayout.defIdxDupRightEnd(dim)
      case "GRB"                => regularLayout.defIdxGhostRightBegin(dim)
      case "GRE"                => regularLayout.defIdxGhostRightEnd(dim)
      case "PRB"                => regularLayout.defIdxPadRightBegin(dim)
      case "PRE"                => regularLayout.defIdxPadRightEnd(dim)
      case "TOT"                => regularLayout.defTotalFixed(dim)
    }
  }

  def defIdxByIdExpr(id : String, dim : Int) : IR_Expression = {
    if (useFixedLayoutSizes && dim < Knowledge.dimensionality)
      Logger.error("Using variable-size indexing for field layouts with fixed sizes")

    id match {
      case "PLB"                => defIdxPadLeftBegin(dim)
      case "PLE"                => defIdxPadLeftEnd(dim)
      case "GLB"                => defIdxGhostLeftBegin(dim)
      case "GLE"                => defIdxGhostLeftEnd(dim)
      case "DLB"                => defIdxDupLeftBegin(dim)
      case "DLE"                => defIdxDupLeftEnd(dim)
      case "IB" | "ILB" | "IRB" => defIdxInnerBegin(dim)
      case "IE" | "ILE" | "IRE" => defIdxInnerEnd(dim)
      case "DRB"                => defIdxDupRightBegin(dim)
      case "DRE"                => defIdxDupRightEnd(dim)
      case "GRB"                => defIdxGhostRightBegin(dim)
      case "GRE"                => defIdxGhostRightEnd(dim)
      case "PRB"                => defIdxPadRightBegin(dim)
      case "PRE"                => defIdxPadRightEnd(dim)
      case "TOT"                => defTotal(dim)
    }
  }
}
