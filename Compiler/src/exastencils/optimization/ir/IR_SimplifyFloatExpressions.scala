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

package exastencils.optimization.ir

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.fieldlike.ir._

object IR_SimplifyFloatExpressions extends DefaultStrategy("Simplify floating expressions") {

  private final val DEBUG : Boolean = false

  this += new Transformation("optimize", {
    case d @ IR_VariableDeclaration(IR_RealDatatype, _, Some(expr), _) =>
      d.initialValue = Some(simplify(expr))
      d

    case a @ IR_Assignment(IR_VariableAccess(_, IR_RealDatatype), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(IR_VariableAccess(_, IR_PointerDatatype(IR_RealDatatype)), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(IR_VariableAccess(_, IR_CUDAConstPointerDatatype(IR_RealDatatype)), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(IR_VariableAccess(_, IR_ConstPointerDatatype(IR_RealDatatype)), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(IR_VariableAccess(_, IR_ArrayDatatype(IR_RealDatatype, _)), _, _), src, op) =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(IR_ArrayAccess(fd : IR_IV_AbstractFieldLikeData, _, _), src, op) //
      if fd.field.resolveBaseDatatype == IR_RealDatatype =>
      a.src = simplify(src)
      a

    case a @ IR_Assignment(fa : IR_MultiDimFieldLikeAccess, src, op) =>
      a.src = simplify(src)
      a
  }, isParallel = true)

  private final def simplify(expr : IR_Expression) : IR_Expression = {
    try {
      IR_SimplifyExpression.simplifyFloatingExpr(expr)
    } catch {
      case x : EvaluationException =>
        if (DEBUG)
          println("[simplify]  cannot simplify float expression: " + x.msg) // print directly, logger may be silenced by any surrounding strategy
        expr
    }
  }
}
