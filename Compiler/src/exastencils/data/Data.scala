package exastencils.data

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.omp._
import exastencils.polyhedron._

case class SetupBuffers(var fields : ListBuffer[Field], var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = SetupBuffers\n"
  override def cpp_decl : String = cpp

  override def expand : Output[FunctionStatement] = {
    var body = ListBuffer[Statement]()

    for (field <- fields) {
      var numDataPoints : Expression = field.layout(0).total * field.layout(1).total * field.layout(2).total * field.dataType.resolveFlattendSize
      if (Knowledge.data_addPrePadding)
        numDataPoints += field.alignmentPadding

      var statements : ListBuffer[Statement] = ListBuffer()

      for (slot <- 0 until field.numSlots) {
        statements += new AssignmentStatement(iv.FieldData(field, slot),
          ("new" : Expression) ~~ field.dataType.resolveUnderlyingDatatype. /*FIXME*/ cpp ~ "[" ~ numDataPoints ~ "]")
        if (Knowledge.data_addPrePadding)
          statements += new AssignmentStatement(iv.FieldData(field, slot), iv.FieldData(field, slot) + field.alignmentPadding)
      }

      body += new LoopOverFragments(
        new ConditionStatement(iv.IsValidForSubdomain(field.domain.index),
          statements)) with OMP_PotentiallyParallel
    }

    return FunctionStatement(new UnitDatatype(), s"setupBuffers", ListBuffer(), body)
  }
}

case class GetFromExternalField(var src : Field, var dest : ExternalField) extends AbstractFunctionStatement with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = SetFromExternalField\n"
  override def cpp_decl : String = cpp

  override def expand : Output[FunctionStatement] = {
    new FunctionStatement(new UnitDatatype(), "get" ~ src.codeName,
      ListBuffer(new VariableAccess("dest", Some(PointerDatatype(src.dataType))), new VariableAccess("slot", Some(new IntegerDatatype))),
      ListBuffer[Statement](
        new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => dest.layout(i).idxDupLeftBegin)),
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => dest.layout(i).idxDupRightEnd))),
          new AssignmentStatement(ExternalFieldAccess("dest", dest, LoopOverDimensions.defIt),
            FieldAccess(FieldSelection(src, "slot", -1), LoopOverDimensions.defIt))) with OMP_PotentiallyParallel with PolyhedronAccessable))
  }
}

case class SetFromExternalField(var dest : Field, var src : ExternalField) extends AbstractFunctionStatement with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = SetFromExternalField\n"
  override def cpp_decl : String = cpp

  override def expand : Output[FunctionStatement] = {
    new FunctionStatement(new UnitDatatype(), "set" ~ dest.codeName,
      ListBuffer(new VariableAccess("src", Some(PointerDatatype(dest.dataType))), new VariableAccess("slot", Some(new IntegerDatatype))),
      ListBuffer[Statement](
        new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => src.layout(i).idxDupLeftBegin)),
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => src.layout(i).idxDupRightEnd))),
          new AssignmentStatement(FieldAccess(FieldSelection(dest, "slot", -1), LoopOverDimensions.defIt),
            ExternalFieldAccess("src", src, LoopOverDimensions.defIt))) with OMP_PotentiallyParallel with PolyhedronAccessable))
  }
}

case class SlotAccess(var slot : iv.CurrentSlot, var offset : Int) extends Expression {

  // ensure: 0 <= offset < slot.field.numSlots
  offset %= slot.field.numSlots
  if (offset < 0)
    offset += slot.field.numSlots

  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = SlotAccess\n"

  def expandSpecial : Expression = {
    (slot + offset) Mod slot.field.numSlots // offset is always positive
  }
}

case class AdvanceSlot(var slot : iv.CurrentSlot) extends Statement {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = AdvanceSlot\n"

  def expandSpecial : Statement = {
    AssignmentStatement(slot, (slot + 1) Mod slot.field.numSlots) // slot never contains negative values (currently)
  }
}
