package exastencils.util.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.parallelization.api.mpi.MPI_IsRootProc
import exastencils.prettyprinting.PpStream

/// IR_Print

object IR_Print {
  def apply(stream : IR_VariableAccess, toPrint : IR_Expression*) = new IR_Print(stream, toPrint.to[ListBuffer])

  def endl : IR_Expression = IR_VariableAccess("std::endl", IR_StringDatatype)
}

case class IR_Print(var stream : IR_VariableAccess, var toPrint : ListBuffer[IR_Expression]) extends IR_Statement {
  override def prettyprint(out : PpStream) = out << stream << " << " <<< (toPrint, " << ") << ';'
}

/// IR_RawPrint

object IR_RawPrint {
  def apply(toPrint : IR_Expression*) = new IR_RawPrint(toPrint.to[ListBuffer])
}

// FIXME: name
case class IR_RawPrint(var toPrint : ListBuffer[IR_Expression], var stream : IR_VariableAccess = IR_VariableAccess("std::cout", IR_UnknownDatatype)) extends IR_Statement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    if (toPrint.isEmpty) {
      IR_NullStatement
    } else {
      val printStmt = IR_Print(stream, toPrint.view.flatMap { e => ListBuffer(e, IR_StringConstant(" ")) }.to[ListBuffer] :+ IR_Print.endl)

      // TODO: extract wrapping to separate strategy
      // filter by mpi rank if required
      if (Knowledge.mpi_enabled)
        IR_IfCondition(MPI_IsRootProc(), printStmt)
      else
        printStmt
    }
  }
}
