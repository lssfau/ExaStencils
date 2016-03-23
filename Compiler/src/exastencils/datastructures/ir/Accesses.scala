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
  def datatype : Datatype
}

case class VariableAccess(
    var name : String,
    var dType : Option[Datatype] = None) extends Access {
  def this(n : String, dT : Datatype) = this(n, Option(dT))

  override def prettyprint(out : PpStream) : Unit = out << name
  override def datatype = this.dType.get

  def printDeclaration() : String = dType.get.resolveDeclType.prettyprint + " " + name + dType.get.resolveDeclPostscript
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
  override def datatype = RealDatatype // FIXME_componentIndex // FIXME add type deduction for expressions
}

case class TempBufferAccess(
  var buffer : iv.TmpBuffer,
  var index : MultiIndex,
  var strides : MultiIndex)
    extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TempBufferAccess\n"
  override def datatype = RealDatatype // FIXME_componentIndex // FIXME add type deduction for expressions

  def linearize : ArrayAccess = {
    new ArrayAccess(buffer,
      Mapping.resolveMultiIdx(index, strides),
      false && Knowledge.data_alignTmpBufferPointers /* change here if aligned vector operations are possible for tmp buffers */ )
  }
}

case class ReductionDeviceDataAccess(
    var data : iv.ReductionDeviceData,
    var index : MultiIndex,
    var strides : MultiIndex) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ReductionDeviceDataAccess\n"

  def linearize : ArrayAccess = {
    new ArrayAccess(data, Mapping.resolveMultiIdx(index, strides), false)
  }
}

abstract class FieldAccessLike extends Access {
  def fieldSelection : FieldSelection
  def index : MultiIndex
}

case class DirectFieldAccess(
    var fieldSelection : FieldSelection,
    var index : MultiIndex,
    var componentIndex : ConstIndex = new ConstIndex(0)) extends FieldAccessLike {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = DirectFieldAccess\n"
  override def datatype = fieldSelection.fieldLayout.datatype // FIXME_componentIndex

  def linearize : LinearizedFieldAccess = {
    new LinearizedFieldAccess(fieldSelection, Mapping.resolveMultiIdx(fieldSelection.fieldLayout, index), componentIndex)
  }
}

case class FieldAccess(
    var fieldSelection : FieldSelection,
    var index : MultiIndex,
    var componentIndex : ConstIndex = new ConstIndex(0)) extends FieldAccessLike {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = FieldAccess\n"
  override def datatype = fieldSelection.fieldLayout.datatype

  def expandSpecial() : DirectFieldAccess = {
    DirectFieldAccess(fieldSelection, index + fieldSelection.referenceOffset, componentIndex)
  }
}

case class VirtualFieldAccess(
    var fieldName : String,
    var level : Expression,
    var index : MultiIndex,
    var componentIndex : ConstIndex = new ConstIndex(0),
    var fragIdx : Expression = LoopOverFragments.defIt) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = VirtualFieldAccess\n"
  override def datatype = FieldCollection.getFieldByIdentifierLevExp(fieldName, level).get.fieldLayout.datatype
}

case class ExternalFieldAccess(
    var name : Expression,
    var field : ExternalField,
    var index : MultiIndex,
    var componentIndex : ConstIndex = new ConstIndex(0)) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ExternalFieldAccess\n"
  override def datatype = field.fieldLayout.datatype

  def x = new VariableAccess("x", IntegerDatatype)
  def y = new VariableAccess("y", IntegerDatatype)
  def z = new VariableAccess("z", IntegerDatatype)
  def w = new VariableAccess("w", IntegerDatatype)

  def linearize : ArrayAccess = {
    if (Knowledge.generateFortranInterface) { // Fortran requires multi-index access to multidimensional arrays
      val it = LoopOverDimensions.defIt(field.fieldLayout.numDimsData)
      var ret = name
      for (dim <- field.fieldLayout.numDimsData - 1 to 0)
        ret = new ArrayAccess(ret, it(dim), false) // FIXME_componentIndex add componentIndex here
      ret.asInstanceOf[ArrayAccess]
    } else
      new ArrayAccess(name, Mapping.resolveMultiIdx(field.fieldLayout, index), false) // FIXME_componentIndex add componentIndex here
  }
}

case class LinearizedFieldAccess(
    var fieldSelection : FieldSelection,
    var index : Expression,
    var componentIndex : ConstIndex = new ConstIndex(0)) extends Access with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LinearizedFieldAccess\n"
  override def datatype = fieldSelection.fieldLayout.datatype // FIXME_componentIndex

  override def expand : Output[Expression] = {
    new ArrayAccess(new iv.FieldData(fieldSelection.field, fieldSelection.level, fieldSelection.slot, fieldSelection.fragIdx), index /* FIXME add componentIndex here */ , Knowledge.data_alignFieldPointers)
  }
}

case class StencilAccess(var stencil : Stencil) extends Access { // FIXME add componentIndex
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilAccess\n"
  override def datatype = {
    ir.RealDatatype // FIXME_componentIndex // FIXME add real data type member to Stencil
  }
}

case class StencilFieldAccess(
    var stencilFieldSelection : StencilFieldSelection,
    var index : MultiIndex) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StencilFieldAccess\n"
  override def datatype = stencilFieldSelection.fieldLayout.datatype

  def buildStencil : Stencil = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()
    for (e <- 0 until stencilFieldSelection.stencil.entries.size) {
      var stencilFieldIdx = Duplicate(index)
      stencilFieldIdx(stencilFieldSelection.stencilField.field.fieldLayout.numDimsData - 1) = e // TODO: assumes last index is vector dimension
      var fieldSel = stencilFieldSelection.toFieldSelection
      //fieldSel.componentIndex = Some(e) // FIXME_componentIndex
      entries += new StencilEntry(stencilFieldSelection.stencil.entries(e).offset, new FieldAccess(fieldSel, stencilFieldIdx))
    }
    new Stencil("GENERATED_PLACEHOLDER_STENCIL", stencilFieldSelection.stencil.level, entries)
  }
}

case class MemberAccess(
    var base : Access,
    var varAcc : VariableAccess) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << base << '.' << varAcc
  override def datatype = base.datatype
}

case class DerefAccess(var base : Access) extends Access {
  override def prettyprint(out : PpStream) : Unit = out << "(*" << base << ')'
  override def datatype = base.datatype
}

