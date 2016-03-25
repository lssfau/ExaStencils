package exastencils.data

import scala.collection.mutable.ListBuffer

import exastencils.core._
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
  override def name = "setupBuffers"

  override def expand : Output[FunctionStatement] = {
    var body = ListBuffer[Statement]()

    // add static allocations here

    return FunctionStatement(UnitDatatype, name, ListBuffer(), body)
  }
}

case class GetFromExternalField(var src : Field, var dest : ExternalField) extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SetFromExternalField\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "get" + dest.identifier

  def getFortranCompDT() : Datatype = {
    var dt : Datatype = dest.resolveBaseDatatype
    for (dim <- 0 until dest.fieldLayout.numDimsData)
      dt = ArrayDatatype_VS(dt, dest.fieldLayout.idxById("TOT", dim))
    dt
  }

  override def expand : Output[FunctionStatement] = {
    val externalDT = if (Knowledge.generateFortranInterface)
      getFortranCompDT()
    else
      PointerDatatype(src.resolveBaseDatatype)

    val loopDim = dest.fieldLayout.numDimsData
    var multiIndex = LoopOverDimensions.defIt(loopDim)

    new FunctionStatement(UnitDatatype, name,
      ListBuffer(new VariableAccess("dest", Some(externalDT)), new VariableAccess("slot", Some(IntegerDatatype))),
      ListBuffer[Statement](
        new LoopOverDimensions(loopDim, new IndexRange(
          new MultiIndex((0 until loopDim).toArray.map(dim => src.fieldLayout.idxById("GLB", dim))),
          new MultiIndex((0 until loopDim).toArray.map(dim => src.fieldLayout.idxById("GRE", dim)))),
          new AssignmentStatement(ExternalFieldAccess("dest", dest, Duplicate(multiIndex)),
            DirectFieldAccess(FieldSelection(src, src.level, "slot"), Duplicate(multiIndex)))) with OMP_PotentiallyParallel with PolyhedronAccessible),
      false, true)
  }
}

case class SetFromExternalField(var dest : Field, var src : ExternalField) extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SetFromExternalField\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "set" + src.identifier

  def getFortranCompDT() : Datatype = {
    var dt : Datatype = src.resolveBaseDatatype
    for (dim <- 0 until src.fieldLayout.numDimsData)
      dt = ArrayDatatype_VS(dt, src.fieldLayout.idxById("TOT", dim))
    dt
  }

  override def expand : Output[FunctionStatement] = {
    val externalDT = if (Knowledge.generateFortranInterface)
      getFortranCompDT()
    else
      PointerDatatype(dest.resolveBaseDatatype)

    val loopDim = src.fieldLayout.numDimsData
    var multiIndex = LoopOverDimensions.defIt(loopDim)

    new FunctionStatement(UnitDatatype, name,
      ListBuffer(new VariableAccess("src", Some(externalDT)), new VariableAccess("slot", Some(IntegerDatatype))),
      ListBuffer[Statement](
        new LoopOverDimensions(loopDim, new IndexRange(
          new MultiIndex((0 until loopDim).toArray.map(dim => dest.fieldLayout.idxById("GLB", dim))),
          new MultiIndex((0 until loopDim).toArray.map(dim => dest.fieldLayout.idxById("GRE", dim)))),
          new AssignmentStatement(DirectFieldAccess(FieldSelection(dest, dest.level, "slot"), Duplicate(multiIndex)),
            ExternalFieldAccess("src", src, Duplicate(multiIndex)))) with OMP_PotentiallyParallel with PolyhedronAccessible),
      false, true)
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
