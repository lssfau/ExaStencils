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

package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.field.ir.IR_SlotAccess
import exastencils.fieldlike.ir.IR_FieldLikeAccess

/// IR_LocalConstInvert

object IR_LocalConstInvert {
  def vecComponentAccess(vec : IR_VariableAccess, i0 : Int) = IR_HighDimAccess(vec, IR_ConstIndex(i0))
  def matComponentAccess(mat : IR_VariableAccess, i0 : Int, i1 : Int) = IR_HighDimAccess(mat, IR_ConstIndex(i0, i1))

  def apply(AInv : IR_MatrixExpression, fVals : ListBuffer[IR_Expression], unknowns : ListBuffer[IR_FieldLikeAccess],
      jacobiType : Boolean, relax : Option[IR_Expression]) = {

    invert(AInv, fVals, unknowns, jacobiType, relax)
  }

  def invert(AInv : IR_MatrixExpression, fVals : ListBuffer[IR_Expression], unknowns : ListBuffer[IR_FieldLikeAccess],
      jacobiType : Boolean, relax : Option[IR_Expression]) : ListBuffer[IR_Statement] = {

    def isNonZeroEntry(ex : IR_Expression) = {
      ex match {
        case n : IR_Number if 0 == n.value => false
        case _                             => true
      }
    }

    val stmts = ListBuffer[IR_Statement]()

    def onTheFlyRhs = true

    // declare local variables
    def f = IR_VariableAccess("_local_rhs", IR_MatrixDatatype(IR_RealDatatype, unknowns.length, 1))
    if (!onTheFlyRhs)
      stmts += IR_VariableDeclaration(f, IR_MatrixExpression(IR_MatrixDatatype(IR_RealDatatype, unknowns.length, 1), fVals))

    // write back results
    for (i <- unknowns.indices) {
      val dest = Duplicate(unknowns(i))
      if (jacobiType) dest.slot.asInstanceOf[IR_SlotAccess].offset += 1

      def newU = IR_Addition(unknowns.indices.map(j =>
        AInv.get(i, j) * (if (onTheFlyRhs) fVals(j) else vecComponentAccess(f, j)) : IR_Expression).to[ListBuffer])

      if (relax.isEmpty)
        stmts += IR_Assignment(dest, newU)
      else
        stmts += IR_Assignment(dest, Duplicate(unknowns(i)) * (1.0 - relax.get) + relax.get * newU)
    }

    stmts
  }
}
