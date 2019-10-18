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

case object OMP_WaitForFlag extends IR_FuturePlainFunction {
  exastencils.core.Duplicate.registerConstant(this)

  override var name = "waitForFlag"
  allowInlining = false

  override def prettyprint_decl() : String = prettyprint

  def flag = IR_VariableAccess("flag", IR_PointerDatatype(IR_VolatileDatatype(IR_BooleanDatatype)))

  override def generateFct() = {
    IR_PlainFunction(name, IR_UnitDatatype, IR_FunctionArgument(flag),
      ListBuffer[IR_Statement]( // add busy waiting loop
        IR_WhileLoop(IR_Negation(IR_DerefAccess(flag)), ListBuffer[IR_Statement]()),
        IR_Assignment(IR_DerefAccess(flag), IR_BooleanConstant(false))))
  }

  def generateFctAccess() = IR_PlainInternalFunctionReference(name, IR_UnitDatatype)
}
