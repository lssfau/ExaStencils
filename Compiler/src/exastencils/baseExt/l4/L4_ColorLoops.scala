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

package exastencils.baseExt.l4

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_ColorLoops

case class L4_ColorLoops(var colorExps : ListBuffer[L4_Modulo], var stmts : ListBuffer[L4_Statement]) extends L4_Statement {

  for (cExp <- colorExps)
    cExp.right match {
      case L4_IntegerConstant(i) if (i > 0) => // everything is fine
      case _                                =>
        Logger.error("the divisor of all color expressions for a color with statement must be a positive integer constant")
    }

  override def prettyprint(out : PpStream) = out << "color with {\n" <<< (colorExps, ",\n") << ",\n" <<< (stmts, "\n") << "\n}"
  override def progress = Logger.error("Trying to progress " + this.getClass.getName + " which is unsupported")

  def toRepeatLoops() : L4_RepeatLoops = {
    val conjNF = ArrayBuffer[ListBuffer[L4_Expression]]()
    for (colorExp @ L4_Modulo(_, L4_IntegerConstant(nrCols)) <- colorExps) {
      val disjTerm = ListBuffer[L4_Expression]()
      for (c <- 0L until nrCols)
        disjTerm += L4_EqEq(colorExp, L4_IntegerConstant(c))
      conjNF += disjTerm
    }

    // can be seen as a transformation from conjunctive normal form to disjunctive normal form:
    //   perform cross product from all intermediate results with next list of possibilities
    var disjNF = ListBuffer[L4_Expression]() // only boolean expressions
    // reverse here to get a better ordering
    //   normally, the first coloring expression given is the innermost - in terms of the memory layout - so ensure this is the fastest varying: the last
    val disjTerms = conjNF.reverseIterator
    disjNF = disjTerms.next()
    while (disjTerms.hasNext) {
      val disjTerm = disjTerms.next()
      disjNF = for (left <- disjNF; right <- disjTerm) yield L4_AndAnd(left, right) // cross product using L4_AndAnd as combinator
    }

    L4_RepeatLoops(disjNF, stmts)
  }
}

/// L4_ResolveColorLoops

object L4_ResolveColorLoops extends DefaultStrategy("Resolve color with loops") {
  this += new Transformation("Resolve", {
    case cl : L4_ColorLoops => cl.toRepeatLoops()
  })
}
