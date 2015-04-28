package exastencils.data

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.omp._
import exastencils.polyhedron._
import exastencils.prettyprinting._

case class SetupBuffers(var fields : ListBuffer[Field], var neighbors : ListBuffer[NeighborInfo]) extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SetupBuffers\n"
  override def prettyprint_decl : String = prettyprint

  override def expand : Output[FunctionStatement] = {
    var body = ListBuffer[Statement]()

    // add static allocations here

    return FunctionStatement(new UnitDatatype(), s"setupBuffers", ListBuffer(), body)
  }
}

case class GetFromExternalField(var src : Field, var dest : ExternalField) extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SetFromExternalField\n"
  override def prettyprint_decl : String = prettyprint

  def getFortranCompDT() : Datatype = {
    var dt : Datatype = dest.dataType
    for (d <- 1 until Knowledge.dimensionality)
      dt = ArrayDatatype(dt, dest.fieldLayout.layoutsPerDim(d).evalTotal)

    if (dest.vectorSize > 1)
      dt = ArrayDatatype(dt, dest.vectorSize)

    dt
  }

  override def expand : Output[FunctionStatement] = {
    var externalDT = if (Knowledge.generateFortranInterface)
      getFortranCompDT()
    else
      PointerDatatype(src.dataType)

    new FunctionStatement(new UnitDatatype(), "get" + src.codeName,
      ListBuffer(new VariableAccess("dest", Some(externalDT)), new VariableAccess("slot", Some(new IntegerDatatype))),
      ListBuffer[Statement](
        new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => src.fieldLayout(i).idxGhostLeftBegin)),
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => src.fieldLayout(i).idxGhostRightEnd))),
          new AssignmentStatement(ExternalFieldAccess("dest", dest, LoopOverDimensions.defIt),
            DirectFieldAccess(FieldSelection(src, src.level, "slot"), LoopOverDimensions.defIt))) with OMP_PotentiallyParallel with PolyhedronAccessable))
  }
}

case class SetFromExternalField(var dest : Field, var src : ExternalField) extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SetFromExternalField\n"
  override def prettyprint_decl : String = prettyprint

  def getFortranCompDT() : Datatype = {
    var dt : Datatype = src.dataType
    for (d <- 1 until Knowledge.dimensionality)
      dt = ArrayDatatype(dt, src.fieldLayout.layoutsPerDim(d).evalTotal)

    if (src.vectorSize > 1)
      dt = ArrayDatatype(dt, src.vectorSize)

    dt
  }

  override def expand : Output[FunctionStatement] = {
    var externalDT = if (Knowledge.generateFortranInterface)
      getFortranCompDT()
    else
      PointerDatatype(dest.dataType)

    new FunctionStatement(new UnitDatatype(), "set" + dest.codeName,
      ListBuffer(new VariableAccess("src", Some(externalDT)), new VariableAccess("slot", Some(new IntegerDatatype))),
      ListBuffer[Statement](
        new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => dest.fieldLayout(i).idxGhostLeftBegin)),
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => dest.fieldLayout(i).idxGhostRightEnd))),
          new AssignmentStatement(DirectFieldAccess(FieldSelection(dest, dest.level, "slot"), LoopOverDimensions.defIt),
            ExternalFieldAccess("src", src, LoopOverDimensions.defIt))) with OMP_PotentiallyParallel with PolyhedronAccessable))
  }
}

case class SlotAccess(var slot : iv.CurrentSlot, var offset : Int) extends Expression {
  // ensure: 0 <= offset < slot.field.numSlots
  offset %= slot.field.numSlots
  if (offset < 0)
    offset += slot.field.numSlots

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SlotAccess\n"

  def expandSpecial : Expression = {
    (slot + offset) Mod slot.field.numSlots // offset is always positive
  }
}

case class AdvanceSlotStatement(var slot : iv.CurrentSlot) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = AdvanceSlot\n"

  def expandSpecial : Statement = {
    AssignmentStatement(slot, (slot + 1) Mod slot.field.numSlots) // slot never contains negative values (currently)
  }
}
