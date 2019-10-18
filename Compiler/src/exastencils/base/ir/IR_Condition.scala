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

import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

/// IR_IfCondition

object IR_IfCondition {
  def apply(condition : IR_Expression, trueBody : ListBuffer[IR_Statement]) = new IR_IfCondition(condition, trueBody, ListBuffer())
  def apply(condition : IR_Expression, trueBranch : IR_Statement) = new IR_IfCondition(condition, ListBuffer(trueBranch), ListBuffer())

  def apply(condition : IR_Expression, trueBranch : IR_Statement, falseBranch : IR_Statement) = new IR_IfCondition(condition, ListBuffer(trueBranch), ListBuffer(falseBranch))
  def apply(condition : IR_Expression, trueBody : ListBuffer[IR_Statement], falseBranch : IR_Statement) = new IR_IfCondition(condition, trueBody, ListBuffer(falseBranch))
  def apply(condition : IR_Expression, trueBranch : IR_Statement, falseBody : ListBuffer[IR_Statement]) = new IR_IfCondition(condition, ListBuffer(trueBranch), falseBody)
}

case class IR_IfCondition(var condition : IR_Expression, var trueBody : ListBuffer[IR_Statement], var falseBody : ListBuffer[IR_Statement]) extends IR_ScopedStatement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "if (" << condition << ") {\n"
    out <<< (trueBody, "\n") << '\n'
    out << '}'
    falseBody match {
      case ListBuffer() =>
        // nothing
      case ListBuffer(c : IR_IfCondition) =>
        out << " else " << c // print "else if"
      case _ =>
        out << " else {\n"
        out <<< (falseBody, "\n") << '\n'
        out << '}'
    }
  }
}

/// IR_TernaryCondition

case class IR_TernaryCondition(var condition : IR_Expression, var trueBody : IR_Expression, var falseBody : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(trueBody.datatype, falseBody.datatype)
  override def prettyprint(out : PpStream) : Unit = out << '(' << condition << " ? " << trueBody << " : " << falseBody << ')'
}

/// IR_Case

object IR_Case {
  def apply(toMatch : IR_Expression, body : IR_Statement*) = new IR_Case(toMatch, body.to[ListBuffer])
}

case class IR_Case(var toMatch : IR_Expression, var body : ListBuffer[IR_Statement]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "case " << toMatch << ": {\n"
    out <<< (body, "\n") << '\n'
    out << "} break;"
  }
}

/// IR_Switch

object IR_Switch {
  def apply(what : IR_Expression, body : IR_Case*) = new IR_Switch(what, body.to[ListBuffer])
}

case class IR_Switch(var what : IR_Expression, var body : ListBuffer[IR_Case]) extends IR_ScopedStatement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "switch (" << what << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}
