package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_MemberFunctionCallArrow
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_FieldLayout
import exastencils.field.ir.IR_FieldLayoutLike
import exastencils.field.ir.IR_FieldLayoutPerDim
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.field.ir.IR_IV_IndexFromField
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_Localization
import exastencils.knowledge.ir.IR_LeveledKnowledgeObject
import exastencils.optimization.ir.IR_SimplifyExpression

/// IR_WaLBerlaFieldLayout

/*
waLBerla Layouts:
  - fzyx: Value-sorted data layout (f should be outermost loop)
  - zyxf: Cell-sorted data layout, (f should be innermost loop)
*/

object IR_WaLBerlaFieldLayout extends ((String, Int, IR_Datatype, Int, Int, String, IR_ExpressionIndex, Boolean, Boolean) => IR_WaLBerlaFieldLayout) {
  def apply(layout : IR_FieldLayout, wbLayoutName : String = "fzyx") : IR_WaLBerlaFieldLayout = {
    new IR_WaLBerlaFieldLayout(layout.name, layout.level, layout.datatype, layout.numDimsGrid, layout.numDimsData,
      wbLayoutName, layout.referenceOffset, layout.communicatesDuplicated, layout.communicatesGhosts)
  }
}

case class IR_WaLBerlaFieldLayout(
    var name : String,
    var level : Int,
    var datatype : IR_Datatype,
    var numDimsGrid : Int,
    var numDimsData : Int,
    var layoutName : String, // "xyzf" or "fzyx"
    var referenceOffset : IR_ExpressionIndex,
    var communicatesDuplicated : Boolean,
    var communicatesGhosts : Boolean
) extends IR_LeveledKnowledgeObject with IR_FieldLayoutLike {

  override def createDuplicate() : IR_WaLBerlaFieldLayout = {
    IR_WaLBerlaFieldLayout.tupled(Duplicate(IR_WaLBerlaFieldLayout.unapply(this).get))
  }

  def apply(dim : Int) = { } // TODO ?

  var localization : IR_Localization = IR_AtCellCenter

  lazy val wbField : IR_WaLBerlaField = IR_WaLBerlaFieldCollection.getByLayoutIdentifierLevExp(name, level, suppressError = true).get

  def defIdxById(id : String, dim : Int) : IR_Expression = idxById(id, dim)

  def idxById(id : String, dim : Int) : IR_Expression = {

    def callMemberFunc(name : String) = IR_MemberFunctionCallArrow(WB_IV_FieldData(wbField, IR_IV_ActiveSlot(wbField.field)), name, IR_IntegerDatatype)
    def callMemberFuncForDim(name : String, dim : Int) = {
      // TODO handling layout transformations ?
      var newLayout = Duplicate(layoutName)
      for (d <- 0 until 3 - numDimsGrid)
        newLayout = newLayout.replace(('z' - d).toChar.toString, "")
      if (numDimsData == numDimsGrid)
        newLayout = newLayout.replace("f", "")
      val prefix = dim match {
        case d if d < newLayout.length => newLayout.reverse(d) // rightmost is innermost dim
      }
      IR_MemberFunctionCallArrow(WB_IV_FieldData(wbField, IR_IV_ActiveSlot(wbField.field)), prefix+name, IR_IntegerDatatype)
    }

    def numPadLayersLeft(dim : Int) = numPad(dim) / 2
    def numGhostLayersLeft(dim : Int) = callMemberFunc("nrOfGhostLayers")
    def numDupLayersLeft(dim : Int) = 0 // cell-centered
    def numInnerLayers(dim : Int) = numInner(dim)
    def numDupLayersRight(dim : Int) = 0 // cell-centered
    def numGhostLayersRight(dim : Int) = callMemberFunc("nrOfGhostLayers")
    def numPadLayersRight(dim : Int) = numPad(dim) / 2

    def numDup(d : Int) = numDupLayersLeft(d) + numDupLayersRight(d)
    def numGhost(d : Int) = numGhostLayersLeft(d) + numGhostLayersRight(d)
    def numInner(d : Int) = callMemberFuncForDim("Size", d)
    def numPad(d : Int) = IR_SimplifyExpression.simplifyIntegralExpr(callMemberFuncForDim("AllocSize", d) - numInner(d) - numDup(d) - numGhost(d)) // TODO expression too complex

    def defIdxPadLeftBegin(dim : Int) = { 0 }
    def defIdxPadLeftEnd(dim : Int) = { defIdxPadLeftBegin(dim) + numPadLayersLeft(dim) }

    def defIdxGhostLeftBegin(dim : Int) = { defIdxPadLeftBegin(dim) + numPadLayersLeft(dim) }
    def defIdxGhostLeftEnd(dim : Int) = { defIdxGhostLeftBegin(dim) + numGhostLayersLeft(dim) }

    def defIdxDupLeftBegin(dim : Int) = { defIdxGhostLeftBegin(dim) + numGhostLayersLeft(dim) }
    def defIdxDupLeftEnd(dim : Int) = { defIdxDupLeftBegin(dim) + numDupLayersLeft(dim) }

    def defIdxInnerBegin(dim : Int) = { defIdxDupLeftBegin(dim) + numDupLayersLeft(dim) }
    def defIdxInnerEnd(dim : Int) = { defIdxInnerBegin(dim) + numInnerLayers(dim) }

    def defIdxDupRightBegin(dim : Int) = { defIdxInnerBegin(dim) + numInnerLayers(dim) }
    def defIdxDupRightEnd(dim : Int) = { defIdxDupRightBegin(dim) + numDupLayersRight(dim) }

    def defIdxGhostRightBegin(dim : Int) = { defIdxDupRightBegin(dim) + numDupLayersRight(dim) }
    def defIdxGhostRightEnd(dim : Int) = { defIdxGhostRightBegin(dim) + numGhostLayersRight(dim) }

    def defIdxPadRightBegin(dim : Int) = { defIdxGhostRightBegin(dim) + numGhostLayersRight(dim) }
    def defIdxPadRightEnd(dim : Int) = { defIdxPadRightBegin(dim) + numPadLayersRight(dim) }

    def defTotal(dim : Int) = { callMemberFuncForDim("AllocSize", dim) }

    val idx : IR_Expression = id match {
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

    IR_SimplifyExpression.simplifyIntegralExpr(idx)
  }

  override var layoutsPerDim : Array[IR_FieldLayoutPerDim] = _ // TODO
}

// Mainly for loops, field accesses already handled
object IR_WaLBerlaReplaceLayoutIVs extends DefaultStrategy("Replace layout IVs with field member functions") {
  this += Transformation("Find and replace", {
    case iv @ IR_IV_IndexFromField(layoutIdentifier, level, indexId, dim, fragmentIdx) =>
      val l = level.asInstanceOf[IR_IntegerConstant].v.toInt
      val wbField = IR_WaLBerlaFieldCollection.getByLayoutIdentifierLevExp(layoutIdentifier, l, suppressError = true)

      // replace if field is found
      if (wbField.isDefined) {
        wbField.get.layout.defIdxById(indexId, dim)
      } else {
        iv
      }
  }, applyAtNode = IR_WaLBerlaUtil.startNode)
}