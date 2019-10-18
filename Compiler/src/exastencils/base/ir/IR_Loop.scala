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

package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.parallelization.ir._
import exastencils.prettyprinting.PpStream

/// IR_ForLoop

object IR_ForLoop {
  def apply(begin : IR_Statement, end : IR_Expression, inc : IR_Statement, body : IR_Statement*) =
    new IR_ForLoop(begin, end, inc, body.to[ListBuffer])
}

case class IR_ForLoop(
    var begin : IR_Statement,
    var end : IR_Expression,
    var inc : IR_Statement,
    var body : ListBuffer[IR_Statement],
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo()) extends IR_ScopedStatement with IR_HasParallelizationInfo {

  def maxIterationCount() = {
    if (hasAnnotation("numLoopIterations"))
      getAnnotation("numLoopIterations").get.asInstanceOf[Int]
    else
      0 // TODO: warning?
  }

  override def prettyprint(out : PpStream) : Unit = {
    // BEGIN AMAZING HACK as workaround for IBM XL compiler
    var realEnd = end.prettyprint(out.env)
    if (realEnd.length > 2 && realEnd(0) == '(')
      realEnd = realEnd.substring(1, realEnd.length - 1)
    var realInc = inc.prettyprint(out.env)
    if (realInc.length > 2 && realInc(0) == '(')
      realInc = realInc.substring(1, realInc.length - 1)
    out << "for (" << begin << ' ' << realEnd << "; " << realInc
    // END HACK
    //out << "for (" << begin << ' ' << end << "; " << inc
    val last = out.last()
    if (last == ';' || last == ')') // ')' in case of upper hack removed the ';' instead of the closing bracket
      out.removeLast()
    out << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

/// IR_WhileLoop

object IR_WhileLoop {
  def apply(comparison : IR_Expression, body : IR_Statement*) = new IR_WhileLoop(comparison, body.to[ListBuffer])
}

case class IR_WhileLoop(var comparison : IR_Expression, var body : ListBuffer[IR_Statement]) extends IR_ScopedStatement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "while (" << comparison << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}

/// IR_BreakStatement

case class IR_Break() extends IR_Statement {
  override def prettyprint(out : PpStream) = out << "break;"
}

/// IR_ContinueStatement

case class IR_Continue() extends IR_Statement {
  override def prettyprint(out : PpStream) = out << "continue;"
}
