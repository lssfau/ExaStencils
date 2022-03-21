package exastencils.waLBerla.ir

import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_MatIndex
import exastencils.base.ir.IR_ScalarDatatype
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_FieldAccess
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.prettyprinting.PrettyPrintable

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
) extends IR_WaLBerlaFieldAccessLike with PrettyPrintable {

  override def datatype = field.layout.datatype

  def prettyprint(out : PpStream) = {
    // add zero entries for grid dims < 3
    var newIndex = Duplicate(index)
    newIndex = IR_ExpressionIndex(
      index.indices.take(field.numDimsGrid) ++
        Array.fill(3 - field.numDimsGrid)(0 : IR_Expression) ++
        Duplicate(index).indices.drop(field.numDimsGrid))

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
    out << IR_IV_WaLBerlaFieldData(field, slot, fragIdx) << "->get(" <<< (linearizedHigherDimIndex.indices, ",") << ")"
  }
}

object IR_WaLBerlaResolveFieldAccess extends DefaultStrategy("Resolve FieldAccess nodes to waLBerla ones") {
  this += new Transformation("Resolve", {
    case access : IR_FieldAccess if IR_WaLBerlaFieldCollection.contains(access) =>
      val field = IR_WaLBerlaFieldCollection.getByIdentifier(access.name, access.level, suppressError = true).get
      IR_WaLBerlaFieldAccess(field, access.slot, access.fragIdx, access.index, access.offset, access.frozen, access.matIndex)
  })
}
