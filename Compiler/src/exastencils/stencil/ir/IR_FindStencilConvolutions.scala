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

package exastencils.stencil.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FieldIteratorAccess
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.logger.Logger
import exastencils.operator.ir._
import exastencils.scheduling.NoStrategyWrapper

/// IR_FindStencilConvolutions

object IR_FindStencilConvolutions extends DefaultStrategy("Find and mark stencil-stencil and stencil-field convolutions") {
  var changed : Boolean = false

  def transformMultiplication(exp : IR_Multiplication) : IR_Multiplication = {
    val facts : ListBuffer[IR_Expression] = exp.factors
    var result = new ListBuffer[IR_Expression]()
    var prev : IR_Expression = null

    // check for StencilLike * FieldLike
    result = ListBuffer[IR_Expression]()
    prev = null
    for (f <- facts)
      (prev, f) match {
        case (left : IR_StencilAccess, right : IR_FieldLikeAccess)      =>
          result += IR_StencilConvolution(left, right)
          prev = null
        case (left : IR_StencilFieldAccess, right : IR_FieldLikeAccess) =>
          result += IR_StencilFieldConvolution(left, right)
          prev = null
        case _                                                      =>
          if (prev != null) result += prev
          prev = f
      }
    if (prev != null)
      result += prev
    changed |= facts.length != result.length
    if (facts.length != result.length)
      return IR_Multiplication(result)

    // check for StencilLike * Stencil
    result = ListBuffer[IR_Expression]()
    prev = null
    for (f <- facts)
      (prev, f) match {
        case (left : IR_StencilAccess, right : IR_StencilAccess)      =>
          result += IR_StencilStencilConvolution(left, right)
          prev = null
        case (left : IR_StencilFieldAccess, right : IR_StencilAccess) =>
          result += IR_StencilFieldStencilConvolution(left, right)
          prev = null
        case _                                                        =>
          if (prev != null) result += prev
          prev = f
      }
    if (prev != null)
      result += prev
    changed |= facts.length != result.length
    if (facts.length != result.length)
      return IR_Multiplication(result)

    // check for other convolutions
    result = ListBuffer[IR_Expression]()
    prev = null
    for (f <- facts)
      (prev, f) match {
        case (left : IR_StencilAccess, right : IR_StencilFieldAccess)      =>
          ??? // TODO
        case (left : IR_StencilFieldAccess, right : IR_StencilFieldAccess) =>
          ??? // TODO
        case _                                                             =>
          if (prev != null) result += prev
          prev = f
      }
    if (prev != null)
      result += prev
    changed |= facts.length != result.length
    if (facts.length != result.length)
      return IR_Multiplication(result)

    exp
  }

  this += new Transformation("SearchAndMark", {
    case exp : IR_Multiplication =>
      val newMult = transformMultiplication(exp)
      newMult.factors.size match {
        case 0 => IR_NullExpression
        case 1 => newMult.factors.head
        case _ => newMult
      }
  })
}

/// IR_WrapStencilConvolutions

object IR_WrapStencilConvolutions extends DefaultStrategy("Wrap stencil-field convolutions with relevant column strides") {

  object IR_ReplaceStencil extends QuietDefaultStrategy("Replace something with something else") {
    var toReplace : IR_Stencil = null
    var replacement : IR_Stencil = null

    this += new Transformation("Search and replace", {
      case access : IR_StencilAccess if access.target == toReplace =>
        access.target = replacement
        access
    }, false)
  }

  def processStatement(statement : IR_Statement, stencil : IR_Stencil) : ListBuffer[IR_Statement] = {
    val numDims = stencil.numDims

    // compile cases
    val cases = stencil.assembleCases()

    cases.map(c => {
      // compile condition
      val condition = (0 until numDims).map(d => IR_EqEq(c(d), IR_Modulo(IR_FieldIteratorAccess(d), stencil.numCases(d)))).reduce(IR_AndAnd)

      // duplicate assignment
      val alteredStmt = Duplicate(statement)

      // replace stencil in assignment
      IR_ReplaceStencil.toReplace = stencil
      IR_ReplaceStencil.replacement = IR_StencilOps.filterForSpecCase(stencil, c)
      IR_ReplaceStencil.applyStandalone(alteredStmt)

      // finalize new statement
      IR_IfCondition(condition, alteredStmt)
    })
  }

  this += new Transformation("Wrap", {
    // TODO: loop level or assignment level?
    case assignment : IR_Assignment if StateManager.findFirst({ sc : IR_StencilConvolution => sc.left.target.colStride.exists(_ < 1.0) }, assignment).isDefined =>
      val stencilConvolution = StateManager.findFirst[IR_StencilConvolution]({ sc : IR_StencilConvolution => sc.left.target.colStride.exists(_ < 1.0) }, assignment)
      processStatement(assignment, stencilConvolution.get.left.target)

    case assignment : IR_CompoundAssignment if StateManager.findFirst({ sc : IR_StencilConvolution => sc.left.target.colStride.exists(_ < 1.0) }, assignment).isDefined =>
      val stencilConvolution = StateManager.findFirst[IR_StencilConvolution]({ sc : IR_StencilConvolution => sc.left.target.colStride.exists(_ < 1.0) }, assignment)
      processStatement(assignment, stencilConvolution.get.left.target)

    case conv : IR_StencilConvolution if conv.left.target.colStride.exists(_ < 1.0) =>
      Logger.warn(s"Found stencil convolution outside of an assignment: ${ conv.left.target.name } times ${ conv.right.field.name }")
      conv
  }, false)
}

/// IR_StencilConvolutionWrapper

object IR_StencilConvolutionWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    var convChanged = false
    do {
      IR_FindStencilConvolutions.changed = false
      IR_FindStencilConvolutions.apply()
      convChanged = IR_FindStencilConvolutions.changed

      IR_WrapStencilConvolutions.apply()

      if (Knowledge.useFasterExpand)
        IR_ExpandInOnePass.apply()
      else
        IR_Expand.doUntilDone()
    } while (convChanged)
  }
}
