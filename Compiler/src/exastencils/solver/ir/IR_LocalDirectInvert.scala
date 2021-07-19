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
import exastencils.boundary.ir.IR_IsValidComputationPoint
import exastencils.core.Duplicate
import exastencils.field.ir._
import exastencils.util.ir.IR_ResultingDatatype

/// IR_LocalDirectInvert

object IR_LocalDirectInvert {
  def vecComponentAccess(vec : IR_VariableAccess, i0 : Int) = IR_HighDimAccess(vec, IR_ConstIndex(i0))
  def matComponentAccess(mat : IR_VariableAccess, i0 : Int, i1 : Int) = IR_HighDimAccess(mat, IR_ConstIndex(i0, i1))

  def apply(AVals : ListBuffer[ListBuffer[IR_Expression]], fVals : ListBuffer[IR_Expression], unknowns : ListBuffer[IR_FieldAccess],
      jacobiType : Boolean, relax : Option[IR_Expression], omitConditions : Boolean, msi : IR_MatShape) = {

    invert(AVals, fVals, unknowns, jacobiType, relax, omitConditions, msi)
  }

  def invert(AVals : ListBuffer[ListBuffer[IR_Expression]], fVals : ListBuffer[IR_Expression], unknowns : ListBuffer[IR_FieldAccess],
      jacobiType : Boolean, relax : Option[IR_Expression], omitConditions : Boolean, msi: IR_MatShape) : ListBuffer[IR_Statement] = {

    def isNonZeroEntry(ex : IR_Expression) = {
      ex match {
        case n : IR_Number if 0 == n.value => false
        case _                             => true
      }
    }

    val stmts = ListBuffer[IR_Statement]()

    val innerDt : IR_Datatype = IR_ResultingDatatype(
      AVals(0)(0).datatype.resolveBaseDatatype,
      fVals(0).datatype.resolveBaseDatatype,
      unknowns(0).field.layout.datatype
    )


    def u = IR_VariableAccess("_local_unknowns", IR_MatrixDatatype(innerDt, unknowns.length, 1))
    def f = IR_VariableAccess("_local_rhs", IR_MatrixDatatype(innerDt, unknowns.length, 1))
    //def A = IR_VariableAccess("_local_matrix", IR_MatrixDatatype(IR_RealDatatype, unknowns.length, unknowns.length))
    def A(i : Int, j : Int) = IR_VariableAccess(s"_local_matrix_${ i }_${ j }", innerDt)

    // declare local variables -> to be merged later
    stmts += IR_VariableDeclaration(u, 0.0)
    stmts += IR_VariableDeclaration(f, 0.0)

    for (i <- unknowns.indices)
      for (j <- unknowns.indices)
        if (i == j || isNonZeroEntry(AVals(i)(j)))
          stmts += IR_VariableDeclaration(A(i, j)) // no init value necessary

    // construct rhs and matrix
    for (i <- unknowns.indices) {
      var innerStmts = ListBuffer[IR_Statement]()
      var boundaryStmts = ListBuffer[IR_Statement]()

      innerStmts += IR_Assignment(vecComponentAccess(f, i), fVals(i))
      for (j <- unknowns.indices)
        if (i == j || isNonZeroEntry(AVals(i)(j)))
          innerStmts += IR_Assignment(A(i, j), AVals(i)(j))

      boundaryStmts += IR_Assignment(vecComponentAccess(f, i), Duplicate(unknowns(i)))
      for (j <- unknowns.indices)
        if (i == j || isNonZeroEntry(AVals(i)(j)))
          boundaryStmts += IR_Assignment(A(i, j), if (i == j) 1 else 0)

      // check if current unknown is on/ beyond boundary - if required
      if (omitConditions) {
        stmts ++= innerStmts
      } else {
        stmts += IR_IfCondition(
          IR_IsValidComputationPoint(unknowns(i).field, Duplicate(unknowns(i).index)),
          innerStmts,
          boundaryStmts)
      }
    }

    // compile matrix from single entries
    def AMat = IR_MatrixExpression(Some(IR_MatrixDatatype(innerDt, unknowns.length, unknowns.length)),
      unknowns.indices.map(i =>
        unknowns.indices.map(j =>
          AVals(i)(j) match {
            case n : IR_Number if i != j && 0 == n.value =>
              IR_RealConstant(0) // explicitly mark zero entries not located on the diagonal
            case _                                       => A(i, j)
          }).to[ListBuffer]).to[ListBuffer]
    )

    // solve local system - TODO: replace inverse function call with internal function
    // TODO: set return value of the fct call
    AMat.shape = Some(msi)
    stmts += IR_SolveMatrixSystem(AMat, u, f)


    // write back results
    for (i <- unknowns.indices) {
      val dest = Duplicate(unknowns(i))
      if (jacobiType) dest.slot.asInstanceOf[IR_SlotAccess].offset += 1

      if (omitConditions) {
        if (relax.isEmpty)
          stmts += IR_Assignment(dest, vecComponentAccess(u, i))
        else
          stmts += IR_Assignment(dest, Duplicate(unknowns(i)) * (1.0 - relax.get) + relax.get * vecComponentAccess(u, i))
      } else {
        stmts += IR_IfCondition( // don't write back result on boundaries
          IR_IsValidComputationPoint(unknowns(i).field, Duplicate(unknowns(i).index)),
          if (relax.isEmpty)
            IR_Assignment(dest, vecComponentAccess(u, i))
          else
            IR_Assignment(dest, Duplicate(unknowns(i)) * (1.0 - relax.get) + relax.get * vecComponentAccess(u, i))
        )
      }
    }

    stmts
  }
}
