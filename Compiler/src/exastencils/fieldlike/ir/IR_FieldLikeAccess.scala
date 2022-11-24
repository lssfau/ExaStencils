package exastencils.fieldlike.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FieldIteratorAccess
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_SlotAccess
import exastencils.knowledge.ir.IR_LeveledKnowledgeAccess
import exastencils.logger.Logger
import exastencils.polyhedron.IR_PolyArrayAccessLike
import exastencils.waLBerla.ir.field.IR_DirectWaLBerlaFieldAccess
import exastencils.waLBerla.ir.field.IR_WaLBerlaField

/// IR_FieldLikeAccess

trait IR_FieldLikeAccessLike extends IR_LeveledKnowledgeAccess {
  def field : IR_FieldLike
  def target = field
}

/// IR_MultiDimFieldLikeAccess

trait IR_MultiDimFieldLikeAccess extends IR_FieldLikeAccessLike with IR_SpecialExpandable {
  def slot : IR_Expression
  def fragIdx : IR_Expression

  def index : IR_ExpressionIndex // TODO: IR_Index, also in subclasses

  var allowLinearization : Boolean = true
}

/// IR_FieldLikeAccess

trait IR_FieldLikeAccess extends IR_MultiDimFieldLikeAccess with IR_CanBeOffset {
  def field : IR_FieldLike
  def slot : IR_Expression
  def fragIdx : IR_Expression
  def index : IR_ExpressionIndex
  def frozen : Boolean
  def matIndex : Option[IR_MatIndex]

  override def datatype = {
    val layout = field.layout
    if (index.length == layout.numDimsGrid)
      layout.datatype
    else if (index.length == layout.numDimsData)
      layout.datatype.resolveBaseDatatype
    else if (index.length == layout.numDimsData - 1 && layout.datatype.isInstanceOf[IR_MatrixDatatype] && 1 == layout.datatype.asInstanceOf[IR_MatrixDatatype].sizeM)
    // FIXME: find a reasonable way to deal with this case and remove this HACK
      layout.datatype.resolveBaseDatatype
    else if (index.length == layout.numDimsData - 1 && layout.datatype.isInstanceOf[IR_MatrixDatatype] && 1 == layout.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
    // FIXME: find a reasonable way to deal with this case and remove this HACK
      layout.datatype.resolveBaseDatatype
    else Logger.error(s"Trying to resolve data type with invalid index ${ index.prettyprint() }; field ${ field.name } has data type ${ layout.datatype }")
  }

  var offset : Option[IR_ConstIndex]

  def applyUnresolvedOffset() = {
    if (offset.isDefined) {
      Logger.warn(s"IR_FieldLikeAccess with unresolved offset ${ offset.get.prettyprint() } found")
      for (i <- 0 until offset.get.size)
        index(i) += offset.get(i)
      offset = None
    }
  }

  def getOffsetFromIndex = {
    val dupIndex = Duplicate(index)
    dupIndex.indices = dupIndex.indices.zipWithIndex.map { case (e, i) => e - IR_FieldIteratorAccess(i) }
    dupIndex.toConstIndex
  }
}

/// IR_ApplyOffsetToFieldLikeAccess

object IR_ApplyOffsetToFieldLikeAccess extends DefaultStrategy("Apply offsets to FieldLikeAccess nodes") {
  this += new Transformation("Resolve", {
    case access : IR_FieldLikeAccess if access.offset.isDefined =>
      for (i <- 0 until access.offset.get.size)
        access.index(i) += access.offset.get(i)
      access.offset = None
      access
  })
}

/// IR_DirectFieldLikeAccess

// TODO: avoid this
object IR_DirectFieldLikeAccess {
  def apply(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, index : IR_ExpressionIndex) = field match {
    case f : IR_Field           => IR_DirectFieldAccess(f, slot, fragIdx, index)
    case wbf : IR_WaLBerlaField => IR_DirectWaLBerlaFieldAccess(wbf, slot, fragIdx, index)
    case _                      => Logger.error("Unknown field type used for direct field access")
  }

  def apply(field : IR_FieldLike, slot : IR_Expression, index : IR_ExpressionIndex) = field match {
    case f : IR_Field           => IR_DirectFieldAccess(f, slot, index)
    case wbf : IR_WaLBerlaField => IR_DirectWaLBerlaFieldAccess(wbf, slot, index)
    case _                      => Logger.error("Unknown field type used for direct field access")
  }
}

// TODO: try to remove "IR_DirectFieldAccess" occurrences

trait IR_DirectFieldLikeAccess extends IR_MultiDimFieldLikeAccess with IR_PolyArrayAccessLike {
  def field : IR_FieldLike
  def slot : IR_Expression
  def fragIdx : IR_Expression
  def index : IR_ExpressionIndex

  override def datatype = {
    val layout = field.layout
    if (index.length == layout.numDimsGrid)
      layout.datatype
    else if (index.length == layout.numDimsData)
      layout.datatype.resolveBaseDatatype
    else if (index.length == layout.numDimsData - 1 && layout.datatype.isInstanceOf[IR_MatrixDatatype] && 1 == layout.datatype.asInstanceOf[IR_MatrixDatatype].sizeM)
    // FIXME: find a reasonable way to deal with this case and remove this HACK
      layout.datatype.resolveBaseDatatype
    else Logger.error(s"Trying to resolve data type with invalid index ${ index.prettyprint() }; field data type is ${ layout.datatype }")
  }

  def polyStringBuilderBaseName : String

  // TODO: refactor
  override def uniqueID : String = {
    val name = new StringBuilder(polyStringBuilderBaseName)
    name.append('_').append(field.name).append(field.index).append('_').append(field.level)
    name.append("_l").append(level).append('a')
    name.append('_').append(fragIdx.prettyprint()).append('_')
    slot match {
      case IR_SlotAccess(_, offset) => name.append('s').append(offset)
      case s                        => name.append(s.prettyprint())
    }
    replaceSpecial(name).toString()
  }

  def linearize : IR_LinearizedFieldLikeAccess
}

/// IR_LinearizeDirectFieldLikeAccess

object IR_LinearizeDirectFieldLikeAccess extends DefaultStrategy("Linearize DirectFieldLikeAccess nodes") {
  this += new Transformation("Linearize", {
    case access : IR_DirectFieldLikeAccess if access.allowLinearization => access.linearize
  })
}

/// IR_LinearizedFieldLikeAccess

// TODO: try to remove "IR_LinearizedFieldAccess" occurrences

trait IR_LinearizedFieldLikeAccess extends IR_FieldLikeAccessLike with IR_Expandable {
  def field : IR_FieldLike
  var slot : IR_Expression
  var fragIdx : IR_Expression
  var index : IR_Expression

  override def datatype = field.layout.datatype
}