package exastencils.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_ReadLineFromFile
import exastencils.parallelization.api.mpi.MPI_Bcast
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_ReadStream

case class IR_ReadStations() extends IR_FuturePlainFunction {
  override var name = "readStations"
  override def prettyprint_decl() = prettyprint

  def bcastStations() = {
    var bcastStmts = ListBuffer[IR_Statement]()

    bcastStmts += MPI_Bcast(IR_AddressOf(IR_IV_Stations(0, 0)), Knowledge.swe_stationsMax, IR_IV_Stations(0, 0).datatype.resolveBaseDatatype, 0)
    for (i <- 0 until Knowledge.swe_stationsMax)
      bcastStmts += MPI_Bcast(IR_AddressOf(IR_IV_StationNames(i)), IR_MemberFunctionCall(IR_IV_StationNames(i), "length"), IR_IV_StationNames(i).datatype.resolveBaseDatatype, 0)

    bcastStmts
  }

  override def generateFct() = {
    var body = ListBuffer[IR_Statement]()

    // broadcast stations for every mpi thread besides rank 0
    if (Knowledge.mpi_enabled) {
      body += IR_IfCondition(IR_Neq(MPI_IV_MpiRank, IR_IntegerConstant(0)),
        bcastStations() ++ ListBuffer[IR_Statement](IR_Return())
      )
    }

    // read stations on rank 0
    val file = IR_VariableAccess("file", IR_SpecialDatatype("std::ifstream"))

    def fileName = IR_VariableAccess("fileName", IR_StringDatatype)

    body += IR_VariableDeclaration(file)
    body += IR_MemberFunctionCall(file, "open", fileName)
    body += IR_Assert(IR_MemberFunctionCall(file, "is_open"), ListBuffer("\"Unable to open file \"", fileName), IR_FunctionCall("exit", 1))

    val iss = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream"))
    body += IR_VariableDeclaration(iss)
    val stationNumber = IR_VariableAccess("stationNumber", IR_IntegerDatatype)
    val stationName = IR_VariableAccess("name", IR_StringDatatype)
    val stationX = IR_VariableAccess("x", IR_FloatDatatype)
    val stationY = IR_VariableAccess("y", IR_FloatDatatype)
    body += IR_VariableDeclaration(stationNumber, 0)
    body += IR_VariableDeclaration(stationName)
    body += IR_VariableDeclaration(stationX)
    body += IR_VariableDeclaration(stationY)

    body += IR_WhileLoop(
      IR_FunctionCall(IR_ReadLineFromFile.name, file, iss),
      ListBuffer[IR_Statement](
        IR_ReadStream(iss, ListBuffer(stationName, stationX, stationY)),
        IR_Assignment(IR_IV_StationNames(stationNumber), stationName),
        IR_Assignment(IR_IV_Stations(stationNumber, 0), stationX),
        IR_Assignment(IR_IV_Stations(stationNumber, 1), stationY),
        IR_PreIncrement(stationNumber)
      )
    )

    // mpi broadcast
    if (Knowledge.mpi_enabled) {
      // broadcast rank 0
      body ++= bcastStations()
    }

    IR_PlainFunction(name, IR_UnitDatatype, IR_FunctionArgument(fileName), body)
  }
}
