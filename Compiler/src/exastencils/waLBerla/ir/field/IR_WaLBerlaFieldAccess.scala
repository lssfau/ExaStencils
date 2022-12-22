package exastencils.waLBerla.ir.field

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.OutputType
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.field.ir.IR_SlotAccess
import exastencils.fieldlike.ir._
import exastencils.logger.Logger
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

/// IR_FieldAccessLike

trait IR_WaLBerlaFieldAccessLike extends IR_FieldLikeAccessLike {
  def field : IR_WaLBerlaField
  override def target : IR_WaLBerlaField = field
}

/// IR_MultiDimFieldAccess

trait IR_MultiDimWaLBerlaFieldAccess extends IR_WaLBerlaFieldAccessLike with IR_MultiDimFieldLikeAccess with IR_SpecialExpandable

/// IR_DirectWaLBerlaFieldAccess

object IR_DirectWaLBerlaFieldAccess {
  def apply(field : IR_WaLBerlaField, slot : IR_Expression, index : IR_ExpressionIndex) = new IR_DirectWaLBerlaFieldAccess(field, slot, IR_LoopOverFragments.defIt, index)
}

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
      // access via raw memory pointer
      IR_DirectWaLBerlaFieldAccess(field, slot, fragIdx, index + field.referenceOffset)
    } else {
      if (Knowledge.cuda_enabled)
        Logger.error("Single element accesses field->get(x,y,z,f) not combinable with CUDA. Enable \"waLBerla_useInternalMemoryPointers\" instead.")

      // single-element access via get(x, y, z, f) access
      val newIdx = IR_WaLBerlaUtil.adaptIndexForAccessors(index, field.gridDatatype, field.numDimsGrid, field.layout.numDimsData)
      IR_MemberFunctionCallArrow(IR_IV_WaLBerlaGetField(field, slot, onGPU = false, fragIdx), "get", newIdx.indices : _*)
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
      IR_IV_WaLBerlaFieldData(field, slot, fragIdx),
      index,
      Knowledge.data_alignFieldPointers)
  }
}

/// IR_WaLBerlaResolveFieldAccess

object IR_WaLBerlaResolveFieldAccess extends DefaultStrategy("Resolve FieldAccess nodes to waLBerla ones") {
  this += new Transformation("Resolve", {
    case access : IR_FieldAccess if IR_WaLBerlaFieldCollection.contains(access) =>
      val field = IR_WaLBerlaFieldCollection.getByIdentifier(access.name, access.level, suppressError = true).get
      val newSlot = access.slot match {
        case _ @ IR_SlotAccess(_ @ IR_IV_ActiveSlot(_, frag), off) => IR_SlotAccess(IR_IV_ActiveSlot(field, frag), off)
        case slot                                                  => slot
      }
      IR_WaLBerlaFieldAccess(field, newSlot, Duplicate(access.fragIdx), Duplicate(access.index), Duplicate(access.offset),
        Duplicate(access.frozen), Duplicate(access.matIndex)).expandSpecial()

    case access : IR_WaLBerlaFieldAccess => access.expandSpecial()
  })
}
