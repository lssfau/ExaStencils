package exastencils.data

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.interfacing.IR_ExternalFieldAccess
import exastencils.knowledge._
import exastencils.omp._
import exastencils.polyhedron._
import exastencils.prettyprinting._

case class SetupBuffers(var fields : ListBuffer[Field], var neighbors : ListBuffer[NeighborInfo]) extends IR_AbstractFunction with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SetupBuffers\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "setupBuffers"

  override def expand : Output[IR_Function] = {
    var body = ListBuffer[IR_Statement]()

    // add static allocations here

    IR_Function(IR_UnitDatatype, name, body)
  }
}

case class GetFromExternalField(var src : Field, var dest : ExternalField) extends IR_AbstractFunction with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SetFromExternalField\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "get" + dest.identifier

  def getFortranCompDT() : IR_Datatype = {
    var dt = dest.resolveBaseDatatype
    for (dim <- 0 until dest.fieldLayout.numDimsData)
      dt = IR_ArrayDatatype_VS(dt, dest.fieldLayout.idxById("TOT", dim))
    dt
  }

  override def expand : Output[IR_Function] = {
    val externalDT = if (Knowledge.generateFortranInterface)
      getFortranCompDT()
    else
      IR_PointerDatatype(src.resolveBaseDatatype)

    val loopDim = dest.fieldLayout.numDimsData
    def multiIndex = IR_LoopOverDimensions.defIt(loopDim)

    // access field layouts
    val internal = src.fieldLayout
    val external = dest.fieldLayout

    // match ghost layer info from internal and external fields
    def numGhostInternalLeft(dim : Integer) = internal.idxById("DLB", dim) - internal.idxById("GLB", dim)
    def numGhostExternalLeft(dim : Integer) = external.idxById("DLB", dim) - external.idxById("GLB", dim)
    def numGhostInternalRight(dim : Integer) = internal.idxById("GRE", dim) - internal.idxById("DRE", dim)
    def numGhostExternalRight(dim : Integer) = external.idxById("GRE", dim) - external.idxById("DRE", dim)
    def idxBegin(dim : Integer) : IR_Expression =
      internal.idxById("DLB", dim) - new IR_MinimumExpression(numGhostInternalLeft(dim), numGhostExternalLeft(dim))
    def idxEnd(dim : Integer) : IR_Expression =
      internal.idxById("DRE", dim) + new IR_MinimumExpression(numGhostInternalRight(dim), numGhostExternalRight(dim))
    def offsetForExtField = IR_ExpressionIndex((0 until loopDim).map(dim => numGhostExternalLeft(dim) - numGhostInternalLeft(dim) : IR_Expression).toArray)

    // compile final function
    new IR_Function(IR_UnitDatatype, name,
      ListBuffer(new IR_FunctionArgument("dest", externalDT), new IR_FunctionArgument("slot", IR_IntegerDatatype)),
      ListBuffer[IR_Statement](
        new IR_LoopOverDimensions(loopDim, new IndexRange(
          IR_ExpressionIndex((0 until loopDim).toArray.map(dim => idxBegin(dim))),
          IR_ExpressionIndex((0 until loopDim).toArray.map(dim => idxEnd(dim)))),
          ListBuffer[IR_Statement](IR_Assignment(IR_ExternalFieldAccess("dest", dest, multiIndex + offsetForExtField),
            IR_DirectFieldAccess(FieldSelection(src, src.level, "slot"), multiIndex)))) with OMP_PotentiallyParallel with PolyhedronAccessible),
      false, true)
  }
}

case class SetFromExternalField(var dest : Field, var src : ExternalField) extends IR_AbstractFunction with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SetFromExternalField\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "set" + src.identifier

  def getFortranCompDT() : IR_Datatype = {
    var dt : IR_Datatype = src.resolveBaseDatatype
    for (dim <- 0 until src.fieldLayout.numDimsData)
      dt = IR_ArrayDatatype_VS(dt, src.fieldLayout.idxById("TOT", dim))
    dt
  }

  override def expand : Output[IR_Function] = {
    val externalDT = if (Knowledge.generateFortranInterface)
      getFortranCompDT()
    else
      IR_PointerDatatype(dest.resolveBaseDatatype)

    val loopDim = src.fieldLayout.numDimsData
    def multiIndex = IR_LoopOverDimensions.defIt(loopDim)

    // access field layouts
    val internal = dest.fieldLayout
    val external = src.fieldLayout

    // match ghost layer info from internal and external fields
    def numGhostInternalLeft(dim : Integer) = internal.idxById("DLB", dim) - internal.idxById("GLB", dim)
    def numGhostExternalLeft(dim : Integer) = external.idxById("DLB", dim) - external.idxById("GLB", dim)
    def numGhostInternalRight(dim : Integer) = internal.idxById("GRE", dim) - internal.idxById("DRE", dim)
    def numGhostExternalRight(dim : Integer) = external.idxById("GRE", dim) - external.idxById("DRE", dim)
    def idxBegin(dim : Integer) : IR_Expression =
      internal.idxById("DLB", dim) - new IR_MinimumExpression(numGhostInternalLeft(dim), numGhostExternalLeft(dim))
    def idxEnd(dim : Integer) : IR_Expression =
      internal.idxById("DRE", dim) + new IR_MinimumExpression(numGhostInternalRight(dim), numGhostExternalRight(dim))
    def offsetForExtField = IR_ExpressionIndex((0 until loopDim).map(dim => numGhostExternalLeft(dim) - numGhostInternalLeft(dim) : IR_Expression).toArray)

    // compile final function
    new IR_Function(IR_UnitDatatype, name,
      ListBuffer(new IR_FunctionArgument("src", externalDT), new IR_FunctionArgument("slot", IR_IntegerDatatype)),
      ListBuffer[IR_Statement](
        new IR_LoopOverDimensions(loopDim, new IndexRange(
          IR_ExpressionIndex((0 until loopDim).toArray.map(dim => idxBegin(dim))),
          IR_ExpressionIndex((0 until loopDim).toArray.map(dim => idxEnd(dim)))),
          ListBuffer[IR_Statement](IR_Assignment(IR_DirectFieldAccess(FieldSelection(dest, dest.level, "slot"), multiIndex),
            IR_ExternalFieldAccess("src", src, multiIndex + offsetForExtField)))) with OMP_PotentiallyParallel with PolyhedronAccessible),
      false, true)
  }
}

case class SlotAccess(var slot : iv.CurrentSlot, var offset : Int) extends IR_Expression {
  // ensure: 0 <= offset < slot.field.numSlots
  offset %= slot.field.numSlots
  if (offset < 0)
    offset += slot.field.numSlots

  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SlotAccess\n"

  def expandSpecial : IR_Expression = {
    (slot + offset) Mod slot.field.numSlots // offset is always positive
  }
}

case class AdvanceSlotStatement(var slot : iv.CurrentSlot) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = AdvanceSlot\n"

  def expandSpecial : IR_Statement = {
    IR_Assignment(slot, (slot + 1) Mod slot.field.numSlots) // slot never contains negative values (currently)
  }
}
