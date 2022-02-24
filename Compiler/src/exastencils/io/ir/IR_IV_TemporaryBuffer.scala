package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.grid.ir.IR_Localization

/// IR_IV_TemporaryBuffer
// represents a temporary buffer where data is stored (fragment-wise) before writing it to file
// should be used in combination with parallel I/O libraries (MPI I/O, HDF5, PnetCDF) and only when necessary
// NOTE: assumes that a temporary buffer:
//   - only contains data of interest
//   - does not have layers to be excluded (e.g. pad/ghost/...)

object IR_IV_TemporaryBuffer {
  def accessArray(tempBuf : IR_IV_TemporaryBuffer, idx : IR_Expression) = {
    // TODO: should be done automatically
    var base : IR_Expression = tempBuf
    if (!tempBuf.blockwise)
      base = IR_ArrayAccess(tempBuf, IR_LoopOverFragments.defIt)

    IR_ArrayAccess(base, tempBuf.linearizedIndex(idx))
  }
}

case class IR_IV_TemporaryBuffer(
    var baseDatatype : IR_Datatype,
    var localization : IR_Localization,
    var name : String,
    var domainIdx : Int,
    var blockwise : Boolean,
    dimsPerFrag : ListBuffer[IR_Expression],
) extends IR_InternalVariable(!blockwise, true, false, false, false) with IR_Access {

  override def resolveName() : String = name + resolvePostfix("", domainIdx.prettyprint, "", "", "")
  override def resolveDatatype() : IR_Datatype = IR_PointerDatatype(baseDatatype)
  override def resolveDefValue() = Some(0)

  def numDims : Int = dimsPerFrag.length + (if (blockwise) 1 else 0)

  // NOTE: temp. buffers contain the data for a whole block -> Reduces the number of file accesses where each has a greater granularity compared to fragment-wise accesses
  def dimsLocal : ListBuffer[IR_Expression] = dimsPerFrag ++ (if (blockwise) List(IR_IV_NumValidFrags(domainIdx)) else Nil)
  def dimsGlobal : ListBuffer[IR_Expression] = dimsPerFrag ++ (if (blockwise) List(IR_IV_TotalNumFrags(domainIdx)) else Nil)

  def referenceOffset = IR_ExpressionIndex(Array.fill(numDims)(0))
  def beginIndices : ListBuffer[IR_Expression] = referenceOffset.indices.to[ListBuffer]
  def endIndices : ListBuffer[IR_Expression] = dimsLocal
  def totalDimsLocal : ListBuffer[IR_Expression] = dimsLocal

  def resolveAccess() : IR_Expression = {
    super.resolveAccess(resolveName(), if (!blockwise) IR_LoopOverFragments.defIt else IR_NullExpression,
      domainIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression)
  }

  def linearizedIndex(index : IR_Expression) = index match {
    case idx : IR_Index =>
      IR_ExpressionIndexRange(
        IR_ExpressionIndex(Array.fill(numDims)(0)),
        IR_ExpressionIndex(dimsLocal.toArray)
      ).linearizeIndex(idx)
    case _              =>
      index
  }

  def allocateMemory = wrapInLoops(IR_IfCondition(IR_Negation(resolveAccess()),
    IR_ArrayAllocation(resolveAccess(), baseDatatype, dimsLocal.reduce(_ * _))))

  override def getCtor() : Option[IR_Statement] = Some(
    wrapInLoops(IR_Assignment(
      resolveAccess(),
      resolveDefValue().get)))

  override def getDtor() : Option[IR_Statement] = {
    Some(wrapInLoops(
      IR_IfCondition(resolveAccess(),
        ListBuffer[IR_Statement](
          IR_ArrayFree(resolveAccess()),
          IR_Assignment(resolveAccess(), 0)))))
  }
}
