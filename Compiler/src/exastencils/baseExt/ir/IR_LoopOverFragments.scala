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

package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.parallelization.ir._
import exastencils.util.ir.IR_ReplaceVariableAccess

object IR_LoopOverFragments {
  def apply(body : IR_Statement*) = new IR_LoopOverFragments(body.to[ListBuffer])
  def apply(body : IR_Statement, parallelization : IR_ParallelizationInfo) = new IR_LoopOverFragments(ListBuffer(body), parallelization)

  def defIt = IR_VariableAccess("fragmentIdx", IR_IntegerDatatype)
}

case class IR_LoopOverFragments(
    var body : ListBuffer[IR_Statement],
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo()) extends IR_LoopOverProcessLocalBlocks {

  def expandSpecial() : Output[IR_ForLoop] = {
    // TODO: separate omp and potentiallyParallel
    parallelization.potentiallyParallel = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && parallelization.potentiallyParallel

    val loop = IR_ForLoop(
      IR_VariableDeclaration(defIt, 0),
      IR_Lower(defIt, Knowledge.domain_numFragmentsPerBlock),
      IR_PreIncrement(defIt),
      body,
      parallelization)
    loop.annotate("numLoopIterations", Knowledge.domain_numFragmentsPerBlock)
    loop
  }
}

/// IR_ResolveLoopOverFragments

object IR_ResolveLoopOverFragments extends DefaultStrategy("Resolve LoopOverFragments nodes") {
  this += new Transformation("Resolve", {
    case loop : IR_LoopOverFragments =>
      if (Knowledge.experimental_resolveUnreqFragmentLoops && Knowledge.domain_numFragmentsPerBlock <= 1) {
        // eliminate fragment loops in case of only one fragment per block
        val scope = IR_Scope(loop.body)

        // replace references to old loop iterator
        IR_ReplaceVariableAccess.replace = Map(IR_LoopOverFragments.defIt.name -> IR_IntegerConstant(0))
        IR_ReplaceVariableAccess.applyStandalone(scope)

        // check if scoping is necessary
        var scoping = false
        scope.body.foreach {
          case _ : IR_VariableDeclaration => scoping = true
          case _                          =>
        }

        if (scoping)
          scope
        else
          scope.body
      } else {
        // no elimination - simple expand
        loop.expandSpecial()
      }
  })
}
