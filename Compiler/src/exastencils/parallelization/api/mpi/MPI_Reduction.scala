package exastencils.parallelization.api.mpi

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// MPI_Reduce

object MPI_Reduce {
  def apply(root : IR_Expression, sendbuf : IR_Expression, recvbuf : IR_Expression, datatype : IR_Datatype, count : IR_Expression, op : String) =
    new MPI_Reduce(root, sendbuf, recvbuf, datatype, count, mapOp(op))

  def apply(root : IR_Expression, buf : IR_Expression, datatype : IR_Datatype, count : IR_Expression, op : IR_Expression) =
    new MPI_Reduce(root, "MPI_IN_PLACE", buf, datatype, count, op)

  def apply(root : IR_Expression, buf : IR_Expression, datatype : IR_Datatype, count : IR_Expression, op : String) =
    new MPI_Reduce(root, "MPI_IN_PLACE", buf, datatype, count, mapOp(op))

  def mapOp(op : String) : IR_Expression = {
    op match {
      case "+"   => "MPI_SUM"
      case "*"   => "MPI_PROD"
      case "min" => "MPI_MIN"
      case "max" => "MPI_MAX"
      case _     =>
        Logger.warn("Unknown reduction operation in MPI_AllReduce: " + op)
        "---"
    }
  }
}

case class MPI_Reduce(var root : IR_Expression, var sendbuf : IR_Expression, var recvbuf : IR_Expression, var datatype : IR_Datatype, var count : IR_Expression, var op : IR_Expression) extends MPI_Statement {
  def reallyPrint(out : PpStream) : Unit = {
    out << "MPI_Reduce(" << sendbuf << ", " << recvbuf << ", " << count << ", " << datatype.prettyprint_mpi << ", " << op << ", " << root << ", mpiCommunicator);"
  }

  override def prettyprint(out : PpStream) : Unit = {
    sendbuf match {
      // TODO: possible to extract to strategy/ specialized constructors?
      case IR_StringLiteral("MPI_IN_PLACE") => // special handling for MPI_IN_PLACE required
        out << "if (" << IR_EqEqExpression(root, MPI_IV_MpiRank) << ") {\n"
        MPI_Reduce(root, sendbuf, recvbuf, datatype, count, op).reallyPrint(out) // MPI_IN_PLACE for root proc
        out << "\n} else {\n"
        MPI_Reduce(root, recvbuf, recvbuf, datatype, count, op).reallyPrint(out) // same behavior, different call required on all other procs -.-
        out << "\n}"
      case _                                => reallyPrint(out) // otherwise a simple print suffices
    }
  }
}

/// MPI_AllReduce

object MPI_AllReduce {

  import MPI_Reduce.mapOp

  def apply(sendbuf : IR_Expression, recvbuf : IR_Expression, datatype : IR_Datatype, count : IR_Expression, op : String) =
    new MPI_AllReduce(sendbuf, recvbuf, datatype, count, mapOp(op))

  def apply(buf : IR_Expression, datatype : IR_Datatype, count : IR_Expression, op : IR_Expression) =
    new MPI_AllReduce("MPI_IN_PLACE", buf, datatype, count, op)

  def apply(buf : IR_Expression, datatype : IR_Datatype, count : IR_Expression, op : String) =
    new MPI_AllReduce("MPI_IN_PLACE", buf, datatype, count, mapOp(op))
}

case class MPI_AllReduce(var sendbuf : IR_Expression, var recvbuf : IR_Expression, var datatype : IR_Datatype, var count : IR_Expression, var op : IR_Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "MPI_Allreduce(" << sendbuf << ", " << recvbuf << ", " << count << ", " << datatype.prettyprint_mpi << ", " << op << ", mpiCommunicator);"
  }
}

/// MPI_AddReductions

object MPI_AddReductions extends DefaultStrategy("Add mpi reductions") {
  this += new Transformation("Resolve", {
    case loop : IR_ForLoop if loop.parallelization.reduction.isDefined =>
      val reduction = loop.parallelization.reduction.get
      val stmts = ListBuffer[IR_Statement]()
      stmts += loop
      stmts += MPI_AllReduce(IR_AddressofExpression(reduction.target), reduction.target.datatype, 1, reduction.op)
      stmts
  }, false) // switch off recursion due to wrapping mechanism
}
