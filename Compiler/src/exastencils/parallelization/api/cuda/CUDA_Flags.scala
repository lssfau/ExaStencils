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

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir._
import exastencils.config._
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.fieldlike.ir.IR_IV_FieldLikeFlag
import exastencils.prettyprinting._

/// CUDA_DirtyFlagHelper

object CUDA_DirtyFlagHelper {
  def fragmentIdxIsValid(fragIdx : IR_Expression, domainIdx : IR_Expression) = {
    if (fragIdx != IR_LoopOverFragments.defIt)
      fragIdx >= 0 AndAnd fragIdx < Knowledge.domain_numFragmentsPerBlock AndAnd IR_IV_IsValidForDomain(domainIdx, fragIdx)
    else
      IR_IV_IsValidForDomain(domainIdx, fragIdx)
  }
}

/// CUDA_DirtyFlagCase

object CUDA_DirtyFlagCase extends Enumeration {
  type Access = Value
  final val ANNOT : String = "DirtyFlagCase"

  // CLEAR       : field/buffer was not updated -> no transfer needed
  // INTERMEDIATE: field/buffer was updated     -> possibly need to wait for event before setting to DIRTY
  // DIRTY       : field/buffer was updated     -> transfer needed if execution hardware changes
  final val CLEAR, INTERMEDIATE, DIRTY = Value
}

/// CUDA_HostDataUpdated

// TODO: move to communication package?
case class CUDA_HostDataUpdated(override var field : IR_FieldLike, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FieldLikeFlag {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index, field.level, IR_NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveName() = s"hostDataUpdated" + resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, field.level.toString, "")
  override def resolveDefValue() = Some(CUDA_DirtyFlagCase.DIRTY.id)

  override def resolveDatatype() = {
    if (field.numSlots > 1)
      IR_ArrayDatatype(IR_IntegerDatatype, field.numSlots)
    else
      IR_IntegerDatatype
  }
}

/// CUDA_DeviceDataUpdated

// TODO: move to communication package?
case class CUDA_DeviceDataUpdated(override var field : IR_FieldLike, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FieldLikeFlag {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index, field.level, IR_NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveName() = s"deviceDataUpdated" + resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, field.level.toString, "")
  override def resolveDefValue() = Some(CUDA_DirtyFlagCase.CLEAR.id)

  override def resolveDatatype() = {
    if (field.numSlots > 1)
      IR_ArrayDatatype(IR_IntegerDatatype, field.numSlots)
    else
      IR_IntegerDatatype
  }
}

/// CUDA_HostBufferDataUpdated

// TODO: move to communication package?
case class CUDA_HostBufferDataUpdated(
    var field : IR_FieldLike,
    var send : Boolean,
    var neighIdx : IR_Expression,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_CommVariable with IR_HasMessageDirection {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName() = s"hostBufferDataUpdated_$direction" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(CUDA_DirtyFlagCase.CLEAR.id)
}

/// CUDA_DeviceBufferDataUpdated

// TODO: move to communication package?
case class CUDA_DeviceBufferDataUpdated(
    var field : IR_FieldLike,
    var send : Boolean,
    var neighIdx : IR_Expression,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_CommVariable with IR_HasMessageDirection {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName() = s"deviceBufferDataUpdated_$direction" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(CUDA_DirtyFlagCase.CLEAR.id)
}

/// CUDA_ExecutionMode

abstract class CUDA_ExecutionMode(name : String) extends IR_UnduplicatedVariable {
  override def resolveName() : String = name
  override def resolveDatatype() : IR_Datatype = IR_BooleanDatatype
}

// currently tracked execution mode
case class CUDA_CurrentExecutionMode() extends CUDA_ExecutionMode("currentExecutionMode") {
  override def resolveDefValue() : Option[IR_Expression] = CUDA_CPUExecutionMode().resolveDefValue()
}

case class CUDA_CPUExecutionMode() extends CUDA_ExecutionMode("cpuExecutionMode") {
  override def resolveDefValue() : Option[IR_Expression] = Some(true)
}

case class CUDA_GPUExecutionMode() extends CUDA_ExecutionMode("gpuExecutionMode") {
  override def resolveDefValue() : Option[IR_Expression] = Some(false)
}