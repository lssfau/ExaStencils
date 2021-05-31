package exastencils.waLBerla.ir

import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Index
import exastencils.base.ir.IR_MemberFunctionCallArrow
import exastencils.base.ir.IR_ScalarDatatype
import exastencils.base.ir.IR_SpecialExpandable
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_FieldAccess
import exastencils.knowledge.ir.IR_LeveledKnowledgeAccess
import exastencils.logger.Logger

/// IR_FieldAccessLike

trait IR_WaLBerlaFieldAccessLike extends IR_LeveledKnowledgeAccess {
  def field : IR_WaLBerlaField
  def target = field
}

case class IR_WaLBerlaFieldAccess(
    var field : IR_WaLBerlaField,
    var fragIdx : IR_Expression,
    var index : IR_ExpressionIndex,
    var offset : Option[IR_ConstIndex] = None,
    var frozen : Boolean = false,
    var matIndex : Option[Array[IR_Index]] = None
) extends IR_WaLBerlaFieldAccessLike with IR_SpecialExpandable {

  override def datatype = field.layout.datatype

  def expandSpecial = {
    // add zero entries for grid dims < 3
    var newIndex = Duplicate(index)
    newIndex = IR_ExpressionIndex(
      newIndex.indices.take(field.numDimsGrid) ++
        (0 until 3 - field.numDimsGrid).map(_ => 0 : IR_Expression) ++
          newIndex.indices.drop(field.numDimsGrid))

    // indices need to be flattened
    val linearizedHigherDimIndex = if (field.layout.numDimsData > field.numDimsGrid) {
      field.gridDatatype match {
        case mat : IR_MatrixDatatype =>
          val matIndices = newIndex.indices.takeRight(2)
          IR_ExpressionIndex( newIndex.indices.dropRight(2) :+ (matIndices(1) + matIndices(0) * mat.sizeN ) )
        case _ : IR_ScalarDatatype   =>
          newIndex
        // TODO: other datatypes
        case _                       =>
          Logger.error("Unsupported higher dimensional datatype.")
      }
    } else {
      newIndex
    }
    IR_MemberFunctionCallArrow(WB_IV_FieldData(field, fragIdx), "get", datatype, linearizedHigherDimIndex.indices : _*)
  }
}

object IR_WaLBerlaResolveFieldAccess extends DefaultStrategy("Resolve FieldAccess nodes") {
  this += new Transformation("Resolve", {
    case access : IR_FieldAccess if IR_WaLBerlaFieldCollection.contains(access) =>
      val field = IR_WaLBerlaFieldCollection.getByIdentifier(access.name, access.level, suppressError = true).get
      IR_WaLBerlaFieldAccess(field, access.fragIdx, access.index, access.offset, access.frozen, access.matIndex).expandSpecial
  })
}
