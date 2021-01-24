package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_ArrayAllocation
import exastencils.base.ir.IR_ArrayFree
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Index
import exastencils.base.ir.IR_Negation
import exastencils.base.ir.IR_NullExpression
import exastencils.base.ir.IR_PointerDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.grid.ir.IR_Localization

/// IR_IV_TemporaryBuffer
// represents a temporary buffer where data is stored (fragment-wise) before writing it to file
// should be used in combination with parallel I/O libraries (MPI I/O, HDF5, PnetCDF) and only when necessary
// NOTE: assumes that a temporary buffer:
//   - only contains data of interest
//   - does not have layers to be excluded (e.g. pad/ghost/...)

case class IR_IV_TemporaryBuffer(
    var baseDatatype : IR_Datatype,
    var localization: IR_Localization,
    var name : String,
    var domainIdx : Int,
    dimsPerFrag : ListBuffer[IR_Expression]) extends IR_InternalVariable(false, true, false, false, false) {

  override def resolveName() : String = name + resolvePostfix("", domainIdx.prettyprint, "", "", "")
  override def resolveDatatype() : IR_Datatype = IR_PointerDatatype(baseDatatype)
  override def resolveDefValue() = Some(0)

  def numDims : Int = dimsPerFrag.length + 1

  // NOTE: temp. buffers contain the data for a whole block -> Reduces the number of file accesses where each has a greater granularity compared to fragment-wise accesses
  def dimsLocal : ListBuffer[IR_Expression] = dimsPerFrag :+ IR_IV_NumValidFrags(domainIdx)
  def dimsGlobal : ListBuffer[IR_Expression] = dimsPerFrag :+ IR_IV_TotalNumFrags(domainIdx)

  def referenceOffset = IR_ExpressionIndex(Array.fill(numDims)(0))
  def beginIndices : ListBuffer[IR_Expression] = referenceOffset.indices.to[ListBuffer]
  def endIndices : ListBuffer[IR_Expression] = dimsLocal
  def totalDimsLocal : ListBuffer[IR_Expression] = dimsLocal

  val access : IR_Expression = resolveAccess(resolveName(), IR_NullExpression, domainIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  def at(index : IR_Expression) : IR_Access = index match {
    case idx : IR_Index =>
      val linearizedIdx = IR_ExpressionIndexRange(
        IR_ExpressionIndex(Array.fill(numDims)(0)),
        IR_ExpressionIndex(dimsLocal.toArray)
      ).linearizeIndex(idx)

      IR_ArrayAccess(
        access,
        linearizedIdx)
    case _ =>
      IR_ArrayAccess(name, index)
  }

  def allocateMemory = IR_IfCondition(IR_Negation(access),
    IR_ArrayAllocation(this, baseDatatype, dimsLocal.reduce(_ * _)))

  override def getCtor() : Option[IR_Statement] = Some(
    IR_Assignment(
      IR_VariableAccess(resolveName(), resolveDatatype()),
      resolveDefValue().get))

  override def getDtor() : Option[IR_Statement] = {
    Some(wrapInLoops(
      IR_IfCondition(access,
        ListBuffer[IR_Statement](
          IR_ArrayFree(access),
          IR_Assignment(access, 0)))))
  }
}
