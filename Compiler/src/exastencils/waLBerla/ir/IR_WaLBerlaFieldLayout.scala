package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_MemberFunctionCallArrow
import exastencils.core.Duplicate
import exastencils.field.ir.IR_FieldLayoutLike
import exastencils.field.ir.IR_FieldLayoutPerDim
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

case class IR_WaLBerlaFieldLayout(
    var name : String,
    var level : Int,
    var numDimsGrid : Int,
    var numDimsData : Int,
    var datatype : IR_Datatype,
    var layoutsPerDim : Array[IR_FieldLayoutPerDim],
    var layoutName : String, // "xyzf" or "fzyx"
    var communicatesDuplicated : Boolean,
    var communicatesGhosts : Boolean
) extends IR_LeveledKnowledgeObject with IR_FieldLayoutLike {

  override def createDuplicate() : IR_WaLBerlaFieldLayout = {
    IR_WaLBerlaFieldLayout.tupled(Duplicate(IR_WaLBerlaFieldLayout.unapply(this).get))
  }

  var referenceOffset : IR_ExpressionIndex = IR_ExpressionIndex(Array.fill(layoutsPerDim.length)(0))

  def apply(dim : Int) = layoutsPerDim(dim)

  var localization : IR_Localization = IR_AtCellCenter

  lazy val wbField : IR_WaLBerlaField = IR_WaLBerlaFieldCollection.getByLayoutIdentifierLevExp(name, level, suppressError = true).get

  def defIdxById(id : String, dim : Int) : IR_Expression = idxById(id, dim)

  def idxById(id : String, dim : Int) : IR_Expression = {

    def callMemberFunc(name : String) = IR_MemberFunctionCallArrow(IR_IV_WaLBerlaFieldData(wbField), name, IR_IntegerDatatype)
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
      IR_MemberFunctionCallArrow(IR_IV_WaLBerlaFieldData(wbField), prefix+name, IR_IntegerDatatype)
    }

    def numPadLayersLeft(dim : Int) = 0
    def numGhostLayersLeft(dim : Int) = callMemberFunc("nrOfGhostLayers")
    def numDupLayersLeft(dim : Int) = 0 // cell-centered
    def numInnerLayers(dim : Int) = numInner(dim)
    def numDupLayersRight(dim : Int) = 0 // cell-centered
    def numGhostLayersRight(dim : Int) = callMemberFunc("nrOfGhostLayers")
    def numPadLayersRight(dim : Int) = numPad(dim) // at the end of each coordinate

    def numDup(d : Int) = numDupLayersLeft(d) + numDupLayersRight(d)
    def numGhost(d : Int) = numGhostLayersLeft(d) + numGhostLayersRight(d)
    def numInner(d : Int) = callMemberFuncForDim("Size", d)
    def numPad(d : Int) = IR_SimplifyExpression.simplifyIntegralExpr(callMemberFuncForDim("AllocSize", d) - numInner(d) - numDup(d) - numGhost(d))

    def defIdxInnerBegin(dim : Int) = { 0 }
    def defIdxInnerEnd(dim : Int) = { defIdxInnerBegin(dim) + numInnerLayers(dim) }

    def defIdxDupLeftEnd(dim : Int) = { defIdxInnerBegin(dim) }
    def defIdxDupLeftBegin(dim : Int) = { defIdxInnerBegin(dim) - numDupLayersLeft(dim) }

    def defIdxGhostLeftEnd(dim : Int) = { defIdxDupLeftBegin(dim) }
    def defIdxGhostLeftBegin(dim : Int) = { defIdxDupLeftBegin(dim) - numGhostLayersLeft(dim) }

    def defIdxPadLeftBegin(dim : Int) = { defIdxDupLeftBegin(dim) - numGhostLayersLeft(dim) - numPadLayersLeft(dim) }
    def defIdxPadLeftEnd(dim : Int) = { defIdxPadLeftBegin(dim) + numPadLayersLeft(dim) }

    def defIdxDupRightBegin(dim : Int) = { defIdxInnerEnd(dim) }
    def defIdxDupRightEnd(dim : Int) = { defIdxDupRightBegin(dim) + numDupLayersRight(dim) }

    def defIdxGhostRightBegin(dim : Int) = { defIdxDupRightEnd(dim) }
    def defIdxGhostRightEnd(dim : Int) = { defIdxGhostRightBegin(dim) + numGhostLayersRight(dim) }

    def defIdxPadRightBegin(dim : Int) = { defIdxGhostRightEnd(dim) }
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