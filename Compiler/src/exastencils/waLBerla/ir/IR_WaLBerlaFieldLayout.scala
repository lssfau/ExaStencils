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
    var name : String, // will be used to find the field
    var level : Int, // the (geometric) level the layout is associated with
    var datatype : IR_Datatype, // represents the (original) data type; may be multidimensional, i.e. vectors, matrices, etc.
    var numDimsGrid : Int, // dimensionality of the associated grid; usually lesser than or equal to 3
    var numDimsData : Int, // dimensionality of the stored data, greater equal numDimsGrid
    var layoutName : String, // "xyzf" or "fzyx"
    var referenceOffset : IR_ExpressionIndex, // specifies the (index) offset from the lower corner of the field to the first reference point; in case of node-centered data points the reference point is the first vertex point
    var communicatesDuplicated : Boolean, // specifies if duplicated values need to be exchanged between processes
    var communicatesGhosts : Boolean // specifies if ghost layer values need to be exchanged between processes
) extends IR_LeveledKnowledgeObject {

  override def createDuplicate() : IR_WaLBerlaFieldLayout = {
    IR_WaLBerlaFieldLayout.tupled(Duplicate(IR_WaLBerlaFieldLayout.unapply(this).get))
  }

  def apply(dim : Int) = { } // TODO ?

  def localization : IR_Localization = IR_AtCellCenter

  def defIdxById(wbField : IR_WaLBerlaField, id : String, dim : Int) : IR_Expression = idxById(wbField, id, dim)

  def idxById(wbField : IR_WaLBerlaField, id : String, dim : Int) : IR_Expression = {

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
}

// Mainly for loops, field accesses already handled
object IR_WaLBerlaReplaceLayoutIVs extends DefaultStrategy("Replace layout IVs with field member functions") {
  this += Transformation("Find and replace", {
    case iv @ IR_IV_IndexFromField(layoutIdentifier, level, indexId, dim, fragmentIdx) =>
      val l = level.asInstanceOf[IR_IntegerConstant].v.toInt
      val wbField = IR_WaLBerlaFieldCollection.getByLayoutIdentifierLevExp(layoutIdentifier, l, suppressError = true)

      // replace if field is found
      if (wbField.isDefined) {
        wbField.get.layout.defIdxById(wbField.get, indexId, dim)
      } else {
        iv
      }
  }, applyAtNode = IR_WaLBerlaUtil.startNode)
}