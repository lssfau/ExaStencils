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

package exastencils.base.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.prettyprinting.PpStream

/// L4_ForLoop

object L4_ForLoop {
  def apply(number : Int, iterator : Option[L4_Access], body : List[L4_Statement]) =
    new L4_ForLoop(number, iterator, body.to[ListBuffer])
}

case class L4_ForLoop(
    var number : Int,
    var iterator : Option[L4_Access],
    var body : ListBuffer[L4_Statement]) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "repeat " << number << " times"
    if (iterator.isDefined) out << " count " << iterator.get
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress : IR_Statement = ProgressLocation {
    // FIXME: refactor -> access needs to be variable access, no StringLit, etc
    val (loopVar, begin) =
      if (iterator.isDefined) {
        val lv = iterator.get.progress
        (lv, IR_Assignment(lv, IR_IntegerConstant(0)))
      } else {
        val lv = "someRandomIndexVar" // FIXME: someRandomIndexVar
        (IR_VariableAccess(lv, IR_IntegerDatatype), IR_VariableDeclaration(IR_IntegerDatatype, lv, Some(IR_IntegerConstant(0))))
      }

    val ret = IR_ForLoop(
      begin,
      IR_Lower(Duplicate(loopVar), IR_IntegerConstant(number)),
      IR_Assignment(Duplicate(loopVar), IR_IntegerConstant(1), "+="),
      body.map(_.progress))

    // TODO: move annotation to OptimizationInfo trait
    ret.annotate("numLoopIterations", number)

    ret
  }
}

/// L4_WhileLoop

object L4_WhileLoop {
  def apply(comparison : L4_Expression, body : L4_Statement*) = new L4_WhileLoop(comparison, body.to[ListBuffer])
}

case class L4_WhileLoop(var comparison : L4_Expression, var body : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "repeat while " << comparison << " {\n"
    out <<< (body, "\n")
    out << "\n}"
  }

  override def progress = ProgressLocation(IR_WhileLoop(comparison.progress, body.map(_.progress)))
}

/// L4_UntilLoop

object L4_UntilLoop {
  def apply(comparison : L4_Expression, body : L4_Statement*) = new L4_UntilLoop(comparison, body.to[ListBuffer])
}

case class L4_UntilLoop(var comparison : L4_Expression, var body : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "repeat until " << comparison << " {\n"
    out <<< (body, "\n")
    out << "\n}"
  }

  // TODO: internally process L4_UntilLoops to L4_WhileLoops and remove progress
  override def progress = ProgressLocation(IR_WhileLoop(IR_Negation(comparison.progress), body.map(_.progress)))
}

/// L4_Break

case class L4_Break() extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "break\n"
  override def progress = ProgressLocation(IR_Break())
}
