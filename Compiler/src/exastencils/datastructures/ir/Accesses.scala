package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.iv.VecShiftIndex
import exastencils.knowledge._
import exastencils.prettyprinting._
import exastencils.strategies._

trait Access extends Expression {
  def access : Access
  def datatype : Datatype
  def selection : Array[MultiIndex]
}

case class SelectionAccess(
    var access : Access,
    var datatype : Datatype,
    var selection : Array[MultiIndex]) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "INVALID, class = SelectionAccess"
}

case class ArrayAccess(
    var base : Expression,
    var index : Expression,
    var alignedAccessPossible : Boolean = false) extends Access {
  override def prettyprint(out : PpStream) : Unit = {
    index match {
      case ind : MultiIndex => out << base << ind
      case ind : Expression => out << base << '[' << ind << ']'
    }
  }
}

case class TempBufferAccess(
  var buffer : iv.TmpBuffer,
  var index : MultiIndex,
  var strides : MultiIndex)
    extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TempBufferAccess\n"

  def linearize : ArrayAccess = {
    new ArrayAccess(buffer,
      Mapping.resolveMultiIdx(index, strides),
      false && Knowledge.data_alignTmpBufferPointers /* change here if aligned vector operations are possible for tmp buffers */ )
  }
}

abstract class FieldAccessLike extends Access {
  def fieldSelection : FieldSelection
  def index : MultiIndex
}

case class DirectFieldAccess(
    var fieldSelection : FieldSelection,
    var index : MultiIndex) extends FieldAccessLike {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = DirectFieldAccess\n"

  def linearize : LinearizedFieldAccess = {
    new LinearizedFieldAccess(fieldSelection, Mapping.resolveMultiIdx(fieldSelection.fieldLayout, index))
  }
}

case class FieldAccess(
    var fieldSelection : FieldSelection,
    var index : MultiIndex) extends FieldAccessLike {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = FieldAccess\n"

  def expandSpecial() : DirectFieldAccess = {
    DirectFieldAccess(fieldSelection, index + fieldSelection.referenceOffset)
  }
}

case class VirtualFieldAccess(var fieldName : String,
                              var level : Expression,
                              var index : MultiIndex,
                              var arrayIndex : Option[Int] = None,
                              var fragIdx : Expression = LoopOverFragments.defIt) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = VirtualFieldAccess\n"

}

case class ExternalFieldAccess(var name : Expression,
                               var field : ExternalField,
                               var index : MultiIndex) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ExternalFieldAccess\n"

  def x = new VariableAccess("x", IntegerDatatype)
  def y = new VariableAccess("y", IntegerDatatype)
  def z = new VariableAccess("z", IntegerDatatype)
  def w = new VariableAccess("w", IntegerDatatype)

  def linearize : ArrayAccess = {
    if (Knowledge.generateFortranInterface) { // Fortran requires multi-index access to multidimensional arrays
      val it = LoopOverDimensions.defIt(field.fieldLayout.numDimsData)
      var ret = name
      for (dim <- field.fieldLayout.numDimsData - 1 to 0)
        ret = new ArrayAccess(ret, it(dim), false)
      ret.asInstanceOf[ArrayAccess]
    } else
      new ArrayAccess(name, Mapping.resolveMultiIdx(field.fieldLayout, index), false)
  }
}

case class LinearizedFieldAccess(
    var fieldSelection : FieldSelection,
    var index : Expression) extends Access with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LinearizedFieldAccess\n"

  override def expand : Output[Expression] = {
    new ArrayAccess(new iv.FieldData(fieldSelection.field, fieldSelection.level, fieldSelection.slot, fieldSelection.fragIdx), index, Knowledge.data_alignFieldPointers)
  }
}

case class StencilAccess(var stencil : Stencil) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilAccess\n"
}

case class StencilFieldAccess(
    var stencilFieldSelection : StencilFieldSelection,
    var index : MultiIndex) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilFieldAccess\n"

  def buildStencil : Stencil = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()
    for (e <- 0 until stencilFieldSelection.stencil.entries.size) {
      var stencilFieldIdx = Duplicate(index)
      stencilFieldIdx(stencilFieldSelection.stencilField.field.fieldLayout.numDimsData - 1) = e // TODO: assumes last index is vector dimension
      var fieldSel = stencilFieldSelection.toFieldSelection
      fieldSel.arrayIndex = Some(e)
      entries += new StencilEntry(stencilFieldSelection.stencil.entries(e).offset, new FieldAccess(fieldSel, stencilFieldIdx))
    }
    new Stencil("GENERATED_PLACEHOLDER_STENCIL", stencilFieldSelection.stencil.level, entries)
  }
}

case class MemberAccess(
    var base : Access,
    var varAcc : VariableAccess) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << base << '.' << varAcc
}

case class DerefAccess(var base : Access) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "(*" << base << ')'
}
