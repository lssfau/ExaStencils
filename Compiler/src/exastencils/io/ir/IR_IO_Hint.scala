package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_AddressOf
import exastencils.base.ir.IR_Cast
import exastencils.base.ir.IR_CharDatatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Neq
import exastencils.base.ir.IR_PointerDatatype
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_UnknownDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.config.Knowledge
import exastencils.util.ir.IR_Print

object IR_IO_Hint {
  var infoSet : Boolean = false // set info once and reuse
  var infoSetNc : Boolean = false
}

abstract class IR_IO_Hint extends IR_UnduplicatedVariable {
  val name = "info"
  override def resolveName() : String = name
  override def resolveDatatype() = IR_SpecialDatatype("MPI_Info")
  override def resolveDefValue() : Option[IR_Expression] = Some(IR_VariableAccess("MPI_INFO_NULL", IR_UnknownDatatype))

  override def getDtor() : Option[IR_Statement] = {
    Some(
      IR_IfCondition(IR_Neq(resolveAccess(), resolveDefValue().get),
        IR_FunctionCall(IR_ExternalFunctionReference("MPI_Info_free"), IR_AddressOf(resolveAccess())))
    )
  }

  def resolveAccess() : IR_Expression = IR_VariableAccess(resolveName(), resolveDatatype())

  // set key-value pair for hints
  def setInfo(key : String, value : String) : IR_Statement = {
    def toStrConst(str : String) = IR_Cast(IR_PointerDatatype(IR_CharDatatype), IR_StringConstant(str))
    val functionCall = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Info_set"), resolveAccess(), toStrConst(key), toStrConst(value))
    if (Knowledge.parIO_generateDebugStatements)
      IR_IfCondition(functionCall Neq IR_VariableAccess("MPI_SUCCESS", IR_UnknownDatatype),
        IR_Print(IR_VariableAccess("std::cerr", IR_UnknownDatatype), IR_StringConstant("Error in MPI Info: key-value pair invalid"), IR_Print.endl))
    else
      functionCall
  }

  def setHints() : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    def checkForAutomatic(flag : String) = flag != "automatic" && (flag == "enable" || flag == "disable")

    if (!IR_IO_Hint.infoSet) {
      var innerStmts : ListBuffer[IR_Statement] = ListBuffer()
      innerStmts += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Info_create"), IR_AddressOf(resolveAccess()))
      // striping
      if (Knowledge.lustre_stripe_count > 0)
        innerStmts += setInfo("striping_factor", s"${ Knowledge.lustre_stripe_count }")
      if (Knowledge.lustre_stripe_size > 0)
        innerStmts += setInfo("striping_unit", s"${ Knowledge.lustre_stripe_size }")

      // data sieving
      if (checkForAutomatic(Knowledge.romio_ds_read))
        innerStmts += setInfo("romio_ds_read", Knowledge.romio_ds_read)
      if (checkForAutomatic(Knowledge.romio_ds_write))
        innerStmts += setInfo("romio_ds_write", Knowledge.romio_ds_write)

      // collective buffering
      if (Knowledge.cb_nodes != 0) {
        if (Knowledge.cb_nodes == -1 && Knowledge.lustre_stripe_count < Knowledge.mpi_numThreads) {
          innerStmts += setInfo("cb_nodes", s"${ Knowledge.lustre_stripe_count }")
        } else if (Knowledge.cb_nodes < Knowledge.mpi_numThreads) {
          innerStmts += setInfo("cb_nodes", s"${ Knowledge.cb_nodes }")
        }
      }
      if (Knowledge.cb_buffer_size > 0)
        innerStmts += setInfo("cb_buffer_size", s"${ Knowledge.cb_buffer_size }")
      if (checkForAutomatic(Knowledge.romio_cb_read))
        innerStmts += setInfo("romio_cb_read", s"${ Knowledge.romio_cb_read }")
      if (checkForAutomatic(Knowledge.romio_cb_write))
        innerStmts += setInfo("romio_cb_write", s"${ Knowledge.romio_cb_write }")

      IR_IO_Hint.infoSet = true
      stmts += IR_IfCondition(resolveAccess() EqEq resolveDefValue().get, innerStmts)
    }

    stmts
  }
}

// MPI I/O hints: use functions from abstract class directly
case class MPI_Info() extends IR_IO_Hint

// PnetCDF hints: use extra hint object
case class PnetCDF_Info() extends IR_IO_Hint {
  override def resolveName() : String = "infoNc"

  override def setHints() : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    if (!IR_IO_Hint.infoSet)
      stmts ++= super.setHints()

    if (!IR_IO_Hint.infoSetNc) {
      var innerStmts : ListBuffer[IR_Statement] = ListBuffer()
      innerStmts += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Info_dup"), IR_VariableAccess(name, resolveDatatype()), IR_AddressOf(resolveAccess()))

      def handleAlignments(input : Int) = if (input == -1 && Knowledge.lustre_stripe_size != 0) Knowledge.lustre_stripe_size else input
      if (Knowledge.nc_header_align_size != 1)
        innerStmts += setInfo("nc_header_align_size", s"${handleAlignments(Knowledge.nc_header_align_size)}")
      if (Knowledge.nc_var_align_size != 1)
        innerStmts += setInfo("nc_var_align_size", s"${handleAlignments(Knowledge.nc_var_align_size)}")
      if (Knowledge.nc_record_align_size != 1)
        innerStmts += setInfo("nc_record_align_size", s"${handleAlignments(Knowledge.nc_record_align_size)}")

      IR_IO_Hint.infoSetNc = true
      stmts += IR_IfCondition(resolveAccess() EqEq resolveDefValue().get, innerStmts)
    }

    stmts
  }
}
