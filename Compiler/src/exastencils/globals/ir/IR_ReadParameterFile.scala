package exastencils.globals.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir
import exastencils.base.ir
import exastencils.base.ir.IR_AddressOf
import exastencils.base.ir.IR_Assert
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Continue
import exastencils.base.ir.IR_EqEq
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_FuturePlainFunction
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_MemberFunctionCall
import exastencils.base.ir.IR_Neq
import exastencils.base.ir.IR_ObjectInstantiation
import exastencils.base.ir.IR_PlainFunction
import exastencils.base.ir.IR_Return
import exastencils.base.ir.IR_ScalarDatatype
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_StringDatatype
import exastencils.base.ir.IR_UnitDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.base.ir.IR_WhileLoop
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_ReadLineFromFile
import exastencils.parallelization.api.mpi.MPI_Bcast
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_ReadStream

case class IR_ReadParameterFile() extends IR_FuturePlainFunction {
  override var name = "readParameterFile"
  override def prettyprint_decl() = prettyprint

  val issVal = IR_VariableAccess("issVal", IR_SpecialDatatype("std::istringstream"))

  def overwriteParams(v : IR_VariableDeclaration) = {
    var body = ListBuffer[IR_Statement]()

    //val buf = IR_VariableAccess("buf", v.datatype)
    //body += IR_VariableDeclaration(buf)
    body += IR_ReadStream(issVal, ListBuffer(IR_VariableAccess(v)))
    //body += IR_Assignment(IR_VariableAccess(v), buf)
    body += IR_Continue()
  }

  def bcastParams(variables : ListBuffer[IR_VariableDeclaration]) = {
    var bcastStmts = ListBuffer[IR_Statement]()

    variables.foreach(v => {
      bcastStmts += MPI_Bcast(IR_AddressOf(IR_VariableAccess(v)), 1, v.datatype, 0)
    })

    bcastStmts
  }

  override def generateFct() = {
    var body = ListBuffer[IR_Statement]()

    def globalParams = IR_GlobalCollection.get.variables.filter(_.datatype.isInstanceOf[IR_ScalarDatatype])

    if (Knowledge.mpi_enabled) {
      body += IR_IfCondition(IR_Neq(MPI_IV_MpiRank, IR_IntegerConstant(0)),
        bcastParams(globalParams) ++ ListBuffer[IR_Statement](IR_Return())
      )
    }

    val file = IR_VariableAccess("file", IR_SpecialDatatype("std::ifstream"))

    def fileName = IR_VariableAccess("fileName", IR_StringDatatype)

    body += IR_VariableDeclaration(file)
    body += IR_MemberFunctionCall(file, "open", fileName)
    body += IR_Assert(IR_MemberFunctionCall(file, "is_open"), ListBuffer("\"Unable to open file \"", fileName), IR_FunctionCall("exit", 1))

    val iss = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream"))
    body += IR_VariableDeclaration(iss)
    val paramName = IR_VariableAccess("name", IR_StringDatatype)
    val paramVal = IR_VariableAccess("value", IR_StringDatatype)
    body += IR_VariableDeclaration(paramName)
    body += IR_VariableDeclaration(paramVal)

    body += IR_WhileLoop(
      IR_FunctionCall(IR_ReadLineFromFile.name, file, iss),
      ListBuffer[IR_Statement](
        IR_ReadStream(iss, ListBuffer(paramName, paramVal)),
        IR_ObjectInstantiation(issVal, paramVal)
      ) ++ globalParams.map(v => IR_IfCondition(IR_EqEq(paramName, IR_StringConstant(v.name)), overwriteParams(v)))
    )

    // mpi broadcast
    if (Knowledge.mpi_enabled) {
      body ++= bcastParams(globalParams)
    }

    IR_PlainFunction(name, IR_UnitDatatype, IR_FunctionArgument(fileName), body)
  }
}
















