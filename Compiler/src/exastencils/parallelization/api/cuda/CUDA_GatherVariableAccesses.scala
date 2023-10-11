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
import exastencils.baseExt.ir.IR_ProcessLocalBlockLoopVariable
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.ir.EvaluationException
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.parallelization.api.cuda.CUDA_Util._

object CUDA_GatherVariableAccesses extends QuietDefaultStrategy("Gather local VariableAccess nodes") {
  var reductionTarget : Option[IR_Expression] = None
  var kernelCount : Int = 0
  var fctName : String = ""

  var evaluableAccesses = mutable.HashMap[String, (IR_Access, IR_Datatype)]()
  var nonEvaluableAccesses = mutable.HashMap[String, (IR_VariableAccess, IR_Datatype)]()
  var ignoredAccesses = mutable.SortedSet[String]()
  var ignoredArrayVariableAccesses = mutable.SortedSet[String]()

  def basePrefix(base : IR_VariableAccess) = s"${fctName}_${base.name}"
  // regular, evaluable indexed array accesses
  def arrayAccessAsString(base : IR_VariableAccess, idx : IR_Expression) = s"${basePrefix(base)}_kernelarg_${idx.prettyprint()}"
  def containsArrayAccess(base : IR_VariableAccess, idx : IR_Expression) = evaluableAccesses.contains(arrayAccessAsString(base, idx))
  // array variable accesses in case that a kernel is passed whole array as argument (for non-evaluable indices)
  def arrayVariableAccessAsString(base : IR_VariableAccess) = s"${basePrefix(base)}_deviceCopy_$kernelCount"
  def containsArrayVariableAccess(base : IR_VariableAccess) = nonEvaluableAccesses.contains(arrayVariableAccessAsString(base))

  def isReplaceable(base : IR_VariableAccess, idx : IR_Expression) =
    containsArrayAccess(base, idx) || containsArrayVariableAccess(base)

  def replaceAccess(base : IR_VariableAccess, idx : IR_Expression) : Option[IR_Expression] = {
    if (isReplaceable(base, idx)) {
      if (containsArrayAccess(base, idx)) {
        val name = arrayAccessAsString(base, idx)
        Some(IR_VariableAccess(name, evaluableAccesses(name)._2))
      } else if (containsArrayVariableAccess(base)) {
        val name = arrayVariableAccessAsString(base)
        Some(IR_ArrayAccess(IR_VariableAccess(name, base.datatype), idx))
      } else {
        Logger.error("Error while gathering variables for CUDA kernels")
      }
    } else {
      None
    }
  }

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
    evaluableAccesses = mutable.HashMap[String, (IR_Access, IR_Datatype)]()
    nonEvaluableAccesses = mutable.HashMap[String, (IR_VariableAccess, IR_Datatype)]()
    ignoredArrayVariableAccesses = mutable.SortedSet[String]()
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
      ignoredArrayVariableAccesses += base.name

      if (isEvaluable(idx)) {
        // single, evaluable array accesses -> count "base[idx]" as variable access
        evaluableAccesses.put(arrayAccessAsString(base, idx), (arrAcc, base.datatype.resolveBaseDatatype))
      } else {
        // we found a non-evaluable index -> remove previous evaluable accesses
        evaluableAccesses.foreach {
          case (k, _) if k.startsWith(basePrefix(base)) && k.length > basePrefix(base).length => evaluableAccesses.remove(k)
          case _ =>
        }

        // copy "base" to device data and pass device pointer to the kernel -> count as single variable access to "base"
        nonEvaluableAccesses.put(arrayVariableAccessAsString(base), (base, base.datatype))
      }

      // it can happen that no fragmentIdx is accessed in a loop, but the resulting CudaReductionBuffer requires it
      if (Knowledge.domain_numFragmentsPerBlock > 1 && isReductionVariableAccess(reductionTarget, arrAcc))
        evaluableAccesses.put(fragIdx.name, (fragIdx, fragIdx.datatype))

      arrAcc

    case vAcc : IR_VariableAccess if !ignoredAccesses.contains(vAcc.name) && !ignoredArrayVariableAccesses.contains(vAcc.name) =>
      evaluableAccesses.put(vAcc.name, (vAcc, vAcc.datatype))
      vAcc

    case vAcc : IR_ProcessLocalBlockLoopVariable if !ignoredAccesses.contains(vAcc.resolveName()) && !ignoredArrayVariableAccesses.contains(vAcc.resolveName()) =>
      evaluableAccesses.put(vAcc.resolveName(), (vAcc, vAcc.datatype))
      vAcc

    // same phenomenon: fragmentIdx is required by CudaReductionBuffer, but not present in loop body
    case expr : IR_Expression if Knowledge.domain_numFragmentsPerBlock > 1 && isReductionTarget(reductionTarget, expr) =>
      evaluableAccesses.put(fragIdx.name, (fragIdx, fragIdx.datatype))
      expr
  })
}
