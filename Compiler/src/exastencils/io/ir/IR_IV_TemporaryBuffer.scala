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
import exastencils.base.ir.IR_PointerDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_UnduplicatedVariable

/// IR_IV_TemporaryBuffer
// represents a temporary buffer where data is stored before writing it to file
// should be used in combination with parallel I/O libraries (MPI I/O, HDF5, PnetCDF) and only when necessary

case class IR_IV_TemporaryBuffer(
    var baseDatatype : IR_Datatype,
    var name : String,
    var dims : ListBuffer[IR_Expression]) extends IR_UnduplicatedVariable {

  override def resolveName() : String = name
  override def resolveDatatype() : IR_Datatype = IR_PointerDatatype(baseDatatype)

  def numDims : Int = dims.length

  def resolveAccess(index : IR_Expression) : IR_Access = {
    IR_ArrayAccess(name, index)
  }

  def linearizedDims : IR_Expression = dims.reduce(_ * _)

  def allocateMemory = IR_ArrayAllocation(name, baseDatatype, linearizedDims)

  override def getDtor() : Option[IR_Statement] = {
    val access = IR_VariableAccess(name, baseDatatype)

    Some(wrapInLoops(
      IR_IfCondition(access,
        ListBuffer[IR_Statement](
          IR_ArrayFree(access),
          IR_Assignment(access, 0)))))
  }
}
