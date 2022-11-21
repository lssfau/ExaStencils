package exastencils.waLBerla.ir.field

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.OutputType
import exastencils.field.ir.IR_FieldAccess
import exastencils.fieldlike.ir._
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

/// IR_FieldAccessLike

trait IR_WaLBerlaFieldAccessLike extends IR_FieldLikeAccessLike {
  def field : IR_WaLBerlaField
  override def target : IR_WaLBerlaField = field
}

/// IR_MultiDimFieldAccess

trait IR_MultiDimWaLBerlaFieldAccess extends IR_WaLBerlaFieldAccessLike with IR_MultiDimFieldLikeAccess with IR_SpecialExpandable

/// IR_DirectWaLBerlaFieldAccess

case class IR_DirectWaLBerlaFieldAccess(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var index : IR_ExpressionIndex) extends IR_MultiDimWaLBerlaFieldAccess with IR_DirectFieldLikeAccess {

  def linearize = IR_LinearizedWaLBerlaFieldAccess(field, slot, fragIdx, field.layout.linearizeIndex(index))

  override def polyStringBuilderBaseName : String = "waLBerlaField"
}

/// IR_WaLBerlaFieldAccess

object IR_WaLBerlaFieldAccess {
  def apply(field : IR_WaLBerlaField, slot : IR_Expression, index : IR_ExpressionIndex) : IR_WaLBerlaFieldAccess =
    new IR_WaLBerlaFieldAccess(field, slot, IR_LoopOverFragments.defIt, index)
}

case class IR_WaLBerlaFieldAccess(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var index : IR_ExpressionIndex,
    var offset : Option[IR_ConstIndex] = None,
    var frozen : Boolean = false,
    var matIndex : Option[IR_MatIndex] = None
) extends IR_FieldLikeAccess with IR_MultiDimWaLBerlaFieldAccess {

  def expandSpecial() = {
    applyUnresolvedOffset()

    if (Knowledge.waLBerla_useInternalMemoryPointers) {
      IR_DirectWaLBerlaFieldAccess(field, slot, fragIdx, index + field.referenceOffset)
    } else {
      val newIdx = IR_WaLBerlaUtil.adaptIndexForAccessors(index, field.gridDatatype, field.numDimsGrid, field.layout.numDimsData)
      IR_MemberFunctionCallArrow(IR_IV_WaLBerlaGetFieldData(field, slot, fragIdx), "get", newIdx.indices : _*)
    }
  }
}

/// IR_LinearizedWaLBerlaFieldAccess

case class IR_LinearizedWaLBerlaFieldAccess(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var index : IR_Expression) extends IR_WaLBerlaFieldAccessLike with IR_LinearizedFieldLikeAccess {

  override def expand() : OutputType = {
    IR_ArrayAccess(
      IR_IV_WaLBerlaFieldDataAt(field, slot, fragIdx),
      index,
      Knowledge.data_alignFieldPointers)
  }
}

/// IR_WaLBerlaResolveFieldAccess

object IR_WaLBerlaResolveFieldAccess extends DefaultStrategy("Resolve FieldAccess nodes to waLBerla ones") {
  this += new Transformation("Resolve", {
    case access : IR_FieldAccess if IR_WaLBerlaFieldCollection.contains(access) =>
      val field = IR_WaLBerlaFieldCollection.getByIdentifier(access.name, access.level, suppressError = true).get
      IR_WaLBerlaFieldAccess(field, access.slot, access.fragIdx, access.index, access.offset, access.frozen, access.matIndex).expandSpecial()

    case access : IR_WaLBerlaFieldAccess => access.expandSpecial()
  })
}
