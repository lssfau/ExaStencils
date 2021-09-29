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

package exastencils.parallelization.api.omp

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Node
import exastencils.prettyprinting._

/// OMP_Clause

abstract class OMP_Clause extends Node with PrettyPrintable

/// OMP_Reduction

object OMP_Reduction {
  def apply(red : IR_Reduction) = new OMP_Reduction(red.op, red.target, red.targetName)
}

case class OMP_Reduction(var op : String, var target : IR_Expression, var targetName : String) extends OMP_Clause {
  override def prettyprint(out : PpStream) : Unit = out << "reduction(" << op << " : " << target << ')'
}



/// OMP_LastPrivate

case class OMP_LastPrivate(var vars : ListBuffer[IR_VariableAccess]) extends OMP_Clause {
  def this(v : IR_VariableAccess) = this(ListBuffer(v))
  override def prettyprint(out : PpStream) : Unit = out << "lastprivate(" <<< (vars, ", ") << ')'
}

/// OMP_Private

case class OMP_Private(var vars : ListBuffer[IR_VariableAccess]) extends OMP_Clause {
  def this(v : IR_VariableAccess) = this(ListBuffer(v))
  override def prettyprint(out : PpStream) : Unit = out << "private(" <<< (vars, ", ") << ')'
}
