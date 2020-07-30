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

package exastencils.applications.swe.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures._
import exastencils.prettyprinting.PpStream

/// IR_IV_FragmentNlock

case class IR_IV_FragmentNlock(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"fragmentNlock" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(-1)
}

/// IR_ResolveFragmentNlock

object IR_ResolveFragmentNlock extends DefaultStrategy("ResolveFragmentNlock") {
  this += new Transformation("ResolveFunctionCalls", {
    case IR_FunctionCall(IR_UnresolvedFunctionReference("getFragmentNlock", _), args) =>
      // usage: getFragmentNlock ( fragmentIdx )
      IR_IV_FragmentNlock(args(0))

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("setFragmentNlock", _), args)) =>
      // usage: setFragmentNlock ( fragmentIdx, nlock )
      IR_Assignment(IR_IV_FragmentNlock(args(0)), args(1))
  })
}
