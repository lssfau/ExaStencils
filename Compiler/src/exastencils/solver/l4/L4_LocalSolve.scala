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

package exastencils.solver.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.prettyprinting._
import exastencils.solver.ir._

/// L4_LocalSolve

object L4_LocalSolve {
  // parser interface
  def apply(unknowns : List[L4_UnresolvedAccess], equations : List[L4_Equation], jacobiType : Boolean, relax : Option[L4_Expression])
  = new L4_LocalSolve(unknowns.map(_.asInstanceOf[L4_Expression]).to[ListBuffer], equations.to[ListBuffer], jacobiType, relax)
}

case class L4_LocalSolve(
    var unknowns : ListBuffer[L4_Expression],
    var equations : ListBuffer[L4_Equation],
    var jacobiType : Boolean,
    var relax : Option[L4_Expression]) extends L4_Statement {

  // used to generate loop over field when coming from L3
  var fieldForLoop : L4_Access = _

  override def prettyprint(out : PpStream) : Unit = {
    out << "solve locally "
    if (jacobiType) out << "with jacobi "
    if (relax.isDefined) out << "relax " << relax.get << ' '
    out << "{\n"
    for (i <- unknowns.indices)
      out << unknowns(i) << " => " << equations(i) << "\n"
    out << "}"
  }

  override def progress = ProgressLocation {
    IR_LocalSolve(
      unknowns.map(_.progress.asInstanceOf[IR_FieldLikeAccess]),
      equations.map(_.progress),
      jacobiType,
      L4_ProgressOption(relax)(_.progress))
  }
}

/// L4_AddLoopsToLocalSolve

object L4_AddLoopsToLocalSolve extends DefaultStrategy("Add loop statements around local solve statements") {
  this += new Transformation("Add loops", {
    case solve : L4_LocalSolve =>
      L4_LoopOverField(Duplicate(solve.fieldForLoop), solve)
  }, false /* recursion must be switched of due to wrapping mechanism */)
}
