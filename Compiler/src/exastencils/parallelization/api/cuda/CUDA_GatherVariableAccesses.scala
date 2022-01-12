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

import scala.collection.mutable

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.optimization.ir.EvaluationException
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.parallelization.api.cuda.CUDA_Util._

object CUDA_GatherVariableAccesses extends QuietDefaultStrategy("Gather local VariableAccess nodes") {
  var reductionTarget : Option[IR_Expression] = None

  var accesses = mutable.HashMap[String, (IR_Access, IR_Datatype)]()
  var ignoredAccesses = mutable.SortedSet[String]()
  var ignoredMatrixVariableAccesses = mutable.SortedSet[String]()

  def arrayAccessAsString(base : IR_VariableAccess, idx : IR_Expression) = base.name + idx.prettyprint()
  def containsArrayAccess(base : IR_VariableAccess, idx : IR_Expression) = accesses.contains(arrayAccessAsString(base, idx))

  def isEvaluable(idx : IR_Expression) = {
    var ret = true
    try {
      IR_SimplifyExpression.evalIntegral(idx)
    } catch {
      case _ : EvaluationException => ret = false
      case _ : MatchError          => ret = false
    }
    ret
  }

  val fragIdx = IR_LoopOverFragments.defIt

  def clear() = {
    reductionTarget = None
    accesses = mutable.HashMap[String, (IR_Access, IR_Datatype)]()
    ignoredMatrixVariableAccesses = mutable.SortedSet[String]()
    ignoredAccesses = mutable.SortedSet[String]()
    ignoredAccesses += "std::cout"
    ignoredAccesses += "std::cerr"
    ignoredAccesses += "std::endl"
  }

  this += new Transformation("Searching", {
    case decl : IR_VariableDeclaration =>
      ignoredAccesses += decl.name
      decl

    case arrAcc @ IR_ArrayAccess(base : IR_VariableAccess, idx, _) if !ignoredAccesses.contains(base.name) =>
      ignoredMatrixVariableAccesses += base.name

      if (isEvaluable(idx))
        accesses.put(arrayAccessAsString(base, idx), (arrAcc, base.datatype.resolveBaseDatatype))

      // it can happen that no fragmentIdx is accessed in a loop, but the resulting CudaReductionBuffer requires it
      if (Knowledge.domain_numFragmentsPerBlock > 1 && isReductionVariableAccess(reductionTarget, arrAcc))
        accesses.put(fragIdx.name, (fragIdx, fragIdx.datatype))

      arrAcc

    case vAcc : IR_VariableAccess if !ignoredAccesses.contains(vAcc.name) && !ignoredMatrixVariableAccesses.contains(vAcc.name) =>
      accesses.put(vAcc.name, (vAcc, vAcc.datatype))
      vAcc

    // same phenomenon: fragmentIdx is required by CudaReductionBuffer, but not present in loop body
    case expr : IR_Expression if Knowledge.domain_numFragmentsPerBlock > 1 && isReductionTarget(reductionTarget, expr) =>
      accesses.put(fragIdx.name, (fragIdx, fragIdx.datatype))
      expr
  })
}
