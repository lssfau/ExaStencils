//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.field.ir

import scala.collection.mutable.StringBuilder

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.knowledge.ir.IR_LeveledKnowledgeAccess
import exastencils.logger.Logger
import exastencils.polyhedron.IR_PolyArrayAccessLike

/// IR_FieldAccessLike

trait IR_FieldAccessLike extends IR_LeveledKnowledgeAccess {
  def field : IR_Field
  def target = field
}

/// IR_MultiDimFieldAccess

trait IR_MultiDimFieldAccess extends IR_FieldAccessLike with IR_SpecialExpandable {
  def slot : IR_Expression
  def fragIdx : IR_Expression

  def index : IR_ExpressionIndex // TODO: IR_Index, also in subclasses

  var allowLinearization : Boolean = true
}

/// IR_DirectFieldAccess

object IR_DirectFieldAccess {
  def apply(field : IR_Field, slot : IR_Expression, index : IR_ExpressionIndex) = new IR_DirectFieldAccess(field, slot, IR_LoopOverFragments.defIt, index)
}

case class IR_DirectFieldAccess(
    var field : IR_Field,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var index : IR_ExpressionIndex) extends IR_MultiDimFieldAccess with IR_PolyArrayAccessLike {

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

  // TODO: refactor
  override def uniqueID : String = {
    val name = new StringBuilder("field")
    name.append('_').append(field.name).append(field.index).append('_').append(field.level)
    name.append("_l").append(level).append('a')
    name.append('_').append(fragIdx.prettyprint()).append('_')
    slot match {
      case IR_SlotAccess(_, offset) => name.append('s').append(offset)
      case s                        => name.append(s.prettyprint())
    }
    replaceSpecial(name).toString()
  }

  def linearize = IR_LinearizedFieldAccess(field, slot, fragIdx, field.layout.linearizeIndex(index))
}

/// IR_LinearizeDirectFieldAccess

object IR_LinearizeDirectFieldAccess extends DefaultStrategy("Linearize DirectFieldAccess nodes") {
  this += new Transformation("Linearize", {
    case access : IR_DirectFieldAccess if access.allowLinearization => access.linearize
  })
}

/// IR_FieldAccess

object IR_FieldAccess {
  def apply(field : IR_Field, slot : IR_Expression, index : IR_ExpressionIndex) = new IR_FieldAccess(field, slot, IR_LoopOverFragments.defIt, index)

  def applySpecial(field : IR_Field, slot : IR_Expression, index : IR_ExpressionIndex, matIndex : Option[IR_MatIndex]): IR_FieldAccess = {
    val fa = new IR_FieldAccess(field, slot, IR_LoopOverFragments.defIt, index)
    fa.matIndex = matIndex
    fa
  }

  def apply(field : IR_Field, slot : IR_Expression, index : IR_ExpressionIndex, offset : Option[IR_ConstIndex])
  = new IR_FieldAccess(field, slot, IR_LoopOverFragments.defIt, index, offset)

  def apply(field : IR_Field, slot : IR_Expression, index : IR_ExpressionIndex, offset : Option[IR_ConstIndex], frozen : Boolean)
  = new IR_FieldAccess(field, slot, IR_LoopOverFragments.defIt, index, offset, frozen)

  def apply(field : IR_Field, slot : IR_Expression, index : IR_ExpressionIndex, offset : Option[IR_ConstIndex], frozen : Boolean, matIndex : Option[IR_MatIndex])
  = new IR_FieldAccess(field, slot, IR_LoopOverFragments.defIt, index, offset, frozen, matIndex)
}

case class IR_FieldAccess(
    var field : IR_Field,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var index : IR_ExpressionIndex,
    var offset : Option[IR_ConstIndex] = None,
    var frozen : Boolean = false,
    var matIndex : Option[IR_MatIndex] = None
) extends IR_MultiDimFieldAccess with IR_CanBeOffset {

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

  def getOffsetFromIndex = {
    val dupIndex = Duplicate(index)
    dupIndex.indices = dupIndex.indices.zipWithIndex.map { case (e, i) => e - IR_FieldIteratorAccess(i) }
    dupIndex.toConstIndex
  }

  def expandSpecial = {
    if (offset.isDefined) {
      Logger.warn(s"IR_FieldAccess with unresolved offset ${ offset.get.prettyprint() } found")
      for (i <- 0 until offset.get.size)
        index(i) += offset.get(i)
      offset = None
    }

    IR_DirectFieldAccess(field, slot, fragIdx, index + field.referenceOffset)
  }

  override def offsetWith(newOffset : IR_ConstIndex) = index += newOffset
}

/// IR_ResolveFieldAccess

object IR_ResolveFieldAccess extends DefaultStrategy("Resolve FieldAccess nodes") {
  this += new Transformation("Resolve", {
    case access : IR_FieldAccess => access.expandSpecial
  })
}

/// IR_ApplyOffsetToFieldAccess

object IR_ApplyOffsetToFieldAccess extends DefaultStrategy("Apply offsets to FieldAccess nodes") {
  this += new Transformation("Resolve", {
    case access : IR_FieldAccess if access.offset.isDefined =>
      for (i <- 0 until access.offset.get.size)
        access.index(i) += access.offset.get(i)
      access.offset = None
      access
  })
}

/// IR_LinearizedFieldAccess

case class IR_LinearizedFieldAccess(
    var field : IR_Field,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var index : IR_Expression) extends IR_FieldAccessLike with IR_Expandable {
  override def datatype = field.layout.datatype

  override def expand() = {
    IR_ArrayAccess(
      IR_IV_FieldData(field, slot, fragIdx),
      index,
      Knowledge.data_alignFieldPointers)
  }
}
