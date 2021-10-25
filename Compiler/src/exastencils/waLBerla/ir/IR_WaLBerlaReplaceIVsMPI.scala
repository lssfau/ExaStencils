package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_StackCollector

object IR_WaLBerlaReplaceIVsMPI extends DefaultStrategy("Replace exa's mpi data structures with the ones from waLBerla") {

  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  private val flag = IR_VariableAccess("flag", IR_IntegerDatatype)

  this += Transformation("Utilize waLBerla's MPI comm datastructures", {
    case func : IR_Function if "main" == func.name =>

      // don't initialize/finalize if already done by walberla core

      val oldBody = Duplicate(func.body)
      func.body.clear()
      oldBody.map {
        case MPI_Init =>
          func.body ++= ListBuffer(
            IR_VariableDeclaration(IR_IntegerDatatype, "flag", IR_IntegerConstant(0)),
            IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("MPI_Initialized"), IR_AddressOf(flag))),
            IR_IfCondition(IR_Negation(flag), IR_Native(MPI_Init.prettyprint)))

        case MPI_Finalize =>
          func.body ++= ListBuffer(
            IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("MPI_Finalized"), IR_AddressOf(flag))),
            IR_IfCondition(IR_Negation(flag), IR_Native(MPI_Finalize.prettyprint)))
        case s            =>
          func.body += s
      }

      func

    case iv : MPI_IV if collector.stack.exists(n => n.isInstanceOf[IR_WaLBerlaFunction] || n.isInstanceOf[IR_WaLBerlaFutureFunction]) =>
      val funcName = iv match {
        case MPI_IV_MpiRank => "rank"
        case MPI_IV_MpiSize => "numProcesses"
        case MPI_IV_MpiComm => "comm"
      }

      IR_MemberFunctionCallArrow(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), funcName, iv.datatype)
  })
}
