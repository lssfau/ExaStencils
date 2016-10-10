package exastencils.interfacing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.field.ir._
import exastencils.polyhedron.PolyhedronAccessible
import exastencils.prettyprinting.PpStream

/// IR_CopyToExternalField

case class IR_CopyToExternalField(var src : IR_Field, var dest : IR_ExternalField) extends IR_AbstractFunction with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl() : String = prettyprint
  override def name = "get" + dest.identifier

  def getFortranCompDT() : IR_Datatype = {
    var dt = dest.resolveBaseDatatype
    for (dim <- 0 until dest.fieldLayout.numDimsData)
      dt = IR_ArrayDatatype_VS(dt, dest.fieldLayout.idxById("TOT", dim))
    dt
  }

  override def expand() : Output[IR_Function] = {
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
      internal.idxById("DLB", dim) - IR_MinimumExpression(numGhostInternalLeft(dim), numGhostExternalLeft(dim))
    def idxEnd(dim : Integer) : IR_Expression =
      internal.idxById("DRE", dim) + IR_MinimumExpression(numGhostInternalRight(dim), numGhostExternalRight(dim))
    def offsetForExtField = IR_ExpressionIndex((0 until loopDim).map(dim => numGhostExternalLeft(dim) - numGhostInternalLeft(dim) : IR_Expression).toArray)

    // compile loop body
    def destAccess = IR_ExternalFieldAccess("dest", dest, multiIndex + offsetForExtField)
    def srcAccess = IR_DirectFieldAccess(IR_FieldSelection(src, src.level, "slot"), multiIndex)
    def loopBody = IR_Assignment(destAccess, srcAccess)

    // compile loop
    val loop = new IR_LoopOverDimensions(loopDim, IR_ExpressionIndexRange(
      IR_ExpressionIndex((0 until loopDim).toArray.map(dim => idxBegin(dim))),
      IR_ExpressionIndex((0 until loopDim).toArray.map(dim => idxEnd(dim)))),
      ListBuffer[IR_Statement](loopBody)) with PolyhedronAccessible
    loop.parallelization.potentiallyParallel = true

    // compile final function
    val fct = IR_Function(IR_UnitDatatype, name,
      ListBuffer(IR_FunctionArgument("dest", externalDT), IR_FunctionArgument("slot", IR_IntegerDatatype)),
      loop)

    fct.allowInlining = false
    fct
  }
}

