//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.parallelization.api.mpi

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatrixDatatype
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
        out << "if (" << IR_EqEq(root, MPI_IV_MpiRank) << ") {\n"
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
      if (!reduction.skipMpi) {
        val targetDt = reduction.target.datatype
        val dt = if (targetDt.isInstanceOf[IR_MatrixDatatype]) {
          reduction.target match {
            case _ : IR_ArrayAccess =>
              // (resolved) matrix element access: use base datatype
              targetDt.resolveBaseDatatype
            case acc =>
              Logger.warn("Reduction target is of type IR_MatrixDatatype but something other than a single matrix element is targeted: " + acc)
              targetDt
          }
        } else {
          targetDt
        }
        stmts += MPI_AllReduce(IR_AddressOf(reduction.target), dt, 1, reduction.op)
      }
      stmts
  }, false) // switch off recursion due to wrapping mechanism
}
