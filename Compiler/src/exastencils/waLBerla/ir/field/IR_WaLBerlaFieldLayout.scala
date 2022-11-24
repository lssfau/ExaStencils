package exastencils.waLBerla.ir.field

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.ir.IR_FieldLayout
import exastencils.field.ir.IR_FieldLayoutPerDim
import exastencils.field.ir.IR_IV_IndexFromField
import exastencils.fieldlike.ir.IR_FieldLikeLayout
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_Localization

case class IR_WaLBerlaFieldLayout(
    var name : String,
    var level : Int,
    var numDimsGrid : Int,
    var numDimsData : Int,
    var useFixedLayoutSizes : Boolean,
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

  def apply(dim : Int) = layoutsPerDim(dim)

  var localization : IR_Localization = IR_AtCellCenter

  lazy val wbField : IR_WaLBerlaField = IR_WaLBerlaFieldCollection.getByLayoutIdentifierLevExp(name, level, suppressError = true).get

  // layouts are identical for each slot: use "0" as default
  private def callMemberFunc(name : String) = IR_MemberFunctionCallArrowWithDt(IR_IV_WaLBerlaGetField(wbField, 0), name, IR_IntegerDatatype)

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
    IR_MemberFunctionCallArrowWithDt(IR_IV_WaLBerlaGetField(wbField, 0), prefix + name, IR_IntegerDatatype)
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

  // TODO: copied from IR_FieldLayout
  def updateDefReferenceOffset() = {
    // TODO: this should work for now but may be adapted in the future
    referenceOffset = IR_ExpressionIndex(Array.fill(layoutsPerDim.length)(0))
    for (dim <- 0 until layoutsPerDim.length)
      referenceOffset(dim) = IR_IntegerConstant(layoutsPerDim(dim).numPadLayersLeft + layoutsPerDim(dim).numGhostLayersLeft)
  }

  // fixed iteration spaces handled by a regular exastencils field layout proxy
  private val regularLayout = IR_FieldLayout(name, level, datatype, localization, layoutsPerDim, numDimsGrid, referenceOffset, communicatesDuplicated, communicatesGhosts)

  // TODO: macro impl would be cleaner
  def defIdxInnerBegin(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxInnerBegin(dim) } else { 0 }
  def defIdxInnerEnd(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxInnerEnd(dim) } else { defIdxInnerBegin(dim) + numInnerLayers(dim) }

  def defIdxDupLeftEnd(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxDupLeftEnd(dim) } else { defIdxInnerBegin(dim) }
  def defIdxDupLeftBegin(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxDupLeftBegin(dim) } else { defIdxInnerBegin(dim) - numDupLayersLeft(dim) }

  def defIdxGhostLeftEnd(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxGhostLeftEnd(dim) } else { defIdxDupLeftBegin(dim) }
  def defIdxGhostLeftBegin(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxGhostLeftBegin(dim) } else { defIdxDupLeftBegin(dim) - numGhostLayersLeft(dim) }

  def defIdxPadLeftBegin(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxPadLeftBegin(dim) } else { defIdxDupLeftBegin(dim) - numGhostLayersLeft(dim) - numPadLayersLeft(dim) }
  def defIdxPadLeftEnd(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxPadLeftEnd(dim) } else { defIdxPadLeftBegin(dim) + numPadLayersLeft(dim) }

  def defIdxDupRightBegin(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxDupRightBegin(dim) } else { defIdxInnerEnd(dim) }
  def defIdxDupRightEnd(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxDupRightEnd(dim) } else { defIdxDupRightBegin(dim) + numDupLayersRight(dim) }

  def defIdxGhostRightBegin(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxGhostRightBegin(dim) } else { defIdxDupRightEnd(dim) }
  def defIdxGhostRightEnd(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxGhostRightEnd(dim) } else { defIdxGhostRightBegin(dim) + numGhostLayersRight(dim) }

  def defIdxPadRightBegin(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxPadRightBegin(dim) } else { defIdxGhostRightEnd(dim) }
  def defIdxPadRightEnd(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defIdxPadRightEnd(dim) } else { defIdxPadRightBegin(dim) + numPadLayersRight(dim) }

  def defTotal(dim : Int) : IR_Expression = if (useFixedLayoutSizes) { regularLayout.defTotal(dim) } else { IR_Cast(IR_IntegerDatatype, callMemberFuncForDim("AllocSize", dim)) }

  def idxById(id : String, dim : Int) : IR_Expression = {
    if (Knowledge.data_genVariableFieldSizes && dim < Knowledge.dimensionality)
    // TODO : total
      IR_IV_IndexFromField(name, level, id, dim)
    else
      defIdxById(id, dim)
  }

  def defIdxById(id : String, dim : Int) : IR_Expression = {
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
