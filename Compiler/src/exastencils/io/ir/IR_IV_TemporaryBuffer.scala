package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_ArrayAllocation
import exastencils.base.ir.IR_ArrayFree
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Index
import exastencils.base.ir.IR_NullExpression
import exastencils.base.ir.IR_PointerDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnknownDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.baseExt.ir.IR_LoopOverDomains

/// IR_IV_TemporaryBuffer
// represents a temporary buffer where data is stored before writing it to file
// should be used in combination with parallel I/O libraries (MPI I/O, HDF5, PnetCDF) and only when necessary

// TODO the IVs are not automatically declared in Globals.h and their ctors/dtors are not called
case class IR_IV_TemporaryBuffer(
    var baseDatatype : IR_Datatype,
    var name : String,
    var domainIdx : Int,
    dims : ListBuffer[IR_Expression]) extends IR_InternalVariable(false, true, false, false, false) {

  override def resolveName() : String = name + resolvePostfix("", domainIdx.prettyprint, "", "", "")
  override def resolveDatatype() : IR_Datatype = IR_PointerDatatype(baseDatatype)
  override def resolveDefValue() = Some(IR_VariableAccess("NULL", IR_UnknownDatatype))

  def numDims : Int = dims.length + 1

  def dimsLocal : ListBuffer[IR_Expression] = dims :+ IR_IV_NumValidFrags(domainIdx)
  def dimsGlobal : ListBuffer[IR_Expression] = dims :+ IR_IV_TotalNumFrags(domainIdx)

  def at(index : IR_Expression) : IR_Access = index match {
    case idx : IR_Index =>
      val simplifiedIndex = (0 until numDims).map(d => idx.toExpressionIndex.indices(d) * dimsLocal(d) : IR_Expression).reduce(_ + _)
      IR_ArrayAccess(
        resolveAccess(resolveName(), IR_NullExpression, IR_LoopOverDomains.defIt, IR_NullExpression, IR_NullExpression, IR_NullExpression),
        simplifiedIndex)
    case _ =>
      IR_ArrayAccess(name, index)
  }

  def allocateMemory = IR_ArrayAllocation(name, baseDatatype, dimsLocal.reduce(_ * _))

  override def getCtor() : Option[IR_Statement] = Some(
    IR_Assignment(
      IR_VariableAccess(resolveName(), resolveDatatype()),
      resolveDefValue().get))

  override def getDtor() : Option[IR_Statement] = {
    val access = resolveAccess(resolveName(), IR_NullExpression, IR_LoopOverDomains.defIt, IR_NullExpression, IR_NullExpression, IR_NullExpression)

    Some(wrapInLoops(
      IR_IfCondition(access,
        ListBuffer[IR_Statement](
          IR_ArrayFree(access),
          IR_Assignment(access, 0)))))
  }
}
