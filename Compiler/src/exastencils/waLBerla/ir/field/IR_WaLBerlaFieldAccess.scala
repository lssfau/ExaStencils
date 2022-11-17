package exastencils.waLBerla.ir.field

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_FieldAccess
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

/// IR_FieldAccessLike

trait IR_WaLBerlaFieldAccessLike extends IR_FieldLikeAccess {
  def field : IR_WaLBerlaField
  override def target : IR_WaLBerlaField = field
}

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
) extends IR_WaLBerlaFieldAccessLike with IR_Expandable {

  override def datatype = field.layout.datatype

  def expand() = {

    if (Knowledge.waLBerla_useInternalMemoryPointers) {
      IR_ArrayAccess(
        IR_IV_WaLBerlaFieldDataAt(field, slot, fragIdx),
        field.layout.linearizeIndex(IR_WaLBerlaUtil.adaptIndexForAccessors(index, field.gridDatatype, field.numDimsGrid, field.layout.numDimsData)),
        Knowledge.data_alignFieldPointers)
    } else {
      val newIdx = IR_WaLBerlaUtil.adaptIndexForAccessors(index, field.gridDatatype, field.numDimsGrid, field.layout.numDimsData)
      IR_MemberFunctionCallArrow(IR_IV_WaLBerlaGetFieldData(field, slot, fragIdx), "get", newIdx.indices : _*)
    }
  }
}

object IR_WaLBerlaResolveFieldAccess extends DefaultStrategy("Resolve FieldAccess nodes to waLBerla ones") {
  this += new Transformation("Resolve", {
    case access : IR_FieldAccess if IR_WaLBerlaFieldCollection.contains(access) =>
      val field = IR_WaLBerlaFieldCollection.getByIdentifier(access.name, access.level, suppressError = true).get
      IR_WaLBerlaFieldAccess(field, access.slot, access.fragIdx, access.index, access.offset, access.frozen, access.matIndex)
  })
}
