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

package exastencils.parallelization.api.cuda

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.prettyprinting._

/// CUDA_FunctionCall

case class CUDA_FunctionCall(
    var name : String,
    var arguments : ListBuffer[IR_Expression],
    var executionConfiguration: CUDA_ExecutionConfiguration) extends IR_Expression {

  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = {
    out << name << executionConfiguration << '(' <<< (arguments, ", ") << ')'
  }
}
