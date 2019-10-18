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

import scala.collection.mutable.HashMap

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir._

object CUDA_IdentifierFromFieldAccess {
  def apply(access : IR_LinearizedFieldAccess) = {
    val field = access.field
    var identifier = field.codeName

    // TODO: array fields
    if (field.numSlots > 1) {
      access.slot match {
        case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case _                        => identifier += s"_s${ access.slot.prettyprint }"
      }
    }

    access.fragIdx match {
      case frag : IR_VariableAccess if frag.name == IR_LoopOverFragments.defIt.name => // ignore default case

      case other =>
        identifier += s"_f_${ other.prettyprint().replace("[", "_").replace("]", "_") }"
    }

    identifier
  }
}

/// CUDA_GatherLinearizedFieldAccess

object CUDA_GatherLinearizedFieldAccess extends QuietDefaultStrategy("Gather local LinearizedFieldAccess nodes") {
  var fieldAccesses = HashMap[String, IR_LinearizedFieldAccess]()

  def clear() = fieldAccesses.clear()

  def mapFieldAccess(access : IR_LinearizedFieldAccess) = {
    fieldAccesses.put(CUDA_IdentifierFromFieldAccess(access), access)
  }

  this += new Transformation("Searching", {
    case access : IR_LinearizedFieldAccess =>
      mapFieldAccess(access)
      access
  }, false)
}

/// CUDA_ReplaceLinearizedFieldAccess

object CUDA_ReplaceLinearizedFieldAccess extends QuietDefaultStrategy("Replace local LinearizedFieldAccess nodes") {
  var fieldAccesses = HashMap[String, IR_LinearizedFieldAccess]()

  def extractIdentifier(access : IR_LinearizedFieldAccess) = {
    IR_VariableAccess(CUDA_IdentifierFromFieldAccess(access), IR_PointerDatatype(access.field.resolveDeclType))
  }

  this += new Transformation("Searching", {
    case access : IR_LinearizedFieldAccess =>
      val identifier = extractIdentifier(access)
      IR_ArrayAccess(identifier, access.index)
  })
}

/// CUDA_GatherLinearizedFieldAccessWrites

object CUDA_GatherLinearizedFieldAccessWrites extends QuietDefaultStrategy("Gather local write accesses to LinearizedFieldAccess nodes for read-only cache usage") {
  var writtenFieldAccesses = HashMap[String, IR_LinearizedFieldAccess]()

  def mapFieldAccess(access : IR_LinearizedFieldAccess) = {
    writtenFieldAccesses.put(CUDA_IdentifierFromFieldAccess(access), access)
  }

  this += new Transformation("Searching", {
    case stmt @ IR_Assignment(access : IR_LinearizedFieldAccess, _, _) =>
      mapFieldAccess(access)
      stmt
  })
}
