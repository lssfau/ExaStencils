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

package exastencils.util.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.parallelization.api.mpi.MPI_IsRootProc
import exastencils.prettyprinting.PpStream

trait PrintStream extends IR_Statement {
  def vAccStream : IR_VariableAccess
  def exprToPrint : ListBuffer[IR_Expression]
}

/// IR_Print

object IR_Print {
  def apply(stream : IR_VariableAccess, toPrint : IR_Expression*) = new IR_Print(stream, toPrint.to[ListBuffer])

  def newline : IR_Expression = IR_StringConstant("\\n")
  def flush : IR_Expression = IR_VariableAccess("std::flush", IR_StringDatatype)
  def endl : IR_Expression = IR_VariableAccess("std::endl", IR_StringDatatype)
}

case class IR_Print(var stream : IR_VariableAccess, var toPrint : ListBuffer[IR_Expression]) extends PrintStream {
  override def vAccStream : IR_VariableAccess = stream
  override def exprToPrint : ListBuffer[IR_Expression] = toPrint
  override def prettyprint(out : PpStream) = out << stream << " << " <<< (toPrint, " << ") << ';'
}

/// IR_PrintBinary

object IR_PrintBinary {
  def apply(stream : IR_VariableAccess, toPrint : IR_Access*) = new IR_PrintBinary(stream, toPrint.to[ListBuffer])
}

case class IR_PrintBinary(var stream : IR_VariableAccess, var toPrint : ListBuffer[IR_Access]) extends PrintStream {
  override def vAccStream : IR_VariableAccess = stream
  override def exprToPrint : ListBuffer[IR_Expression] = toPrint.asInstanceOf[ListBuffer[IR_Expression]]
  override def prettyprint(out : PpStream) = {
    toPrint.foreach(acc => {
      out << IR_MemberFunctionCall(stream, "write", IR_Cast(IR_PointerDatatype(IR_CharDatatype), IR_AddressOf(acc)), IR_IntegerConstant(acc.datatype.resolveBaseDatatype.typicalByteSize))
      out << ";" << (if (toPrint.last equals acc) "" else "\n")
    })
  }
}

/// IR_PrintBlockBinary

case class IR_PrintBlockBinary(var stream : IR_VariableAccess, var address : IR_Expression, var byteSize : IR_Expression) extends PrintStream {
  override def vAccStream : IR_VariableAccess = stream
  override def exprToPrint : ListBuffer[IR_Expression] = ListBuffer(address)
  override def prettyprint(out : PpStream) = {
    out << IR_MemberFunctionCall(stream, "write", IR_Cast(IR_PointerDatatype(IR_CharDatatype), address), byteSize) << ";\n"
  }
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
