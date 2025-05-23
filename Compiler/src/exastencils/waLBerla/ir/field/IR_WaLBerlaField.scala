package exastencils.waLBerla.ir.field

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_MatShape
import exastencils.boundary.ir.IR_BoundaryCondition
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_Domain
import exastencils.domain.ir.IR_DomainCollection
import exastencils.field.ir.IR_FieldAccess
import exastencils.fieldlike.ir._
import exastencils.logger.Logger
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_RealType
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_UintType

/// IR_WaLBerlaField

object IR_WaLBerlaField {
  // adaptor for regular field accesses
  def apply(fieldAccess : IR_FieldAccess) : IR_WaLBerlaField = {
    IR_WaLBerlaFieldCollection.getByIdentifier(fieldAccess.field.name, fieldAccess.field.level, suppressError = true).get
  }
}

case class IR_WaLBerlaField(
    var name : String,
    var level : Int,
    var index : Int,
    var codeName : String,
    var layout : IR_WaLBerlaFieldLayout,
    var numSlots : Int,
    var boundary : IR_BoundaryCondition,
    var matShape: Option[IR_MatShape],
    var gpuCompatible : Boolean = true
) extends IR_FieldLike {

  override def createDuplicate() : IR_WaLBerlaField = {
    IR_WaLBerlaField(name, level, index, codeName, Duplicate(layout), numSlots, Duplicate(boundary), Duplicate(matShape))
  }

  def stringIdentifier(slot : Int, onGPU : Boolean) = codeName + s"_s$slot" + (if (onGPU) "_GPU" else "")

  // functions to add fields to block storage.
  // cpu fields are allocated per level and slot using waLBerla's allocation
  // gpu fields are merely memcpy'd from host to device
  def addToStorageCPU(blockForestAcc : IR_VariableAccess, slot : Int, initVal : IR_FunctionArgument, calculateSize : IR_VariableAccess) = {

    val wbFieldTemplate = IR_WaLBerlaDatatypes.WB_FieldDatatype(this, onGPU = false).prettyprint()
    val fieldBaseType = gridDatatype.resolveBaseDatatype.prettyprint()
    val funcRefName = s"field::addToStorage< $wbFieldTemplate >"

    val numGhosts = layout.layoutsPerDim(0).numGhostLayersLeft
    if (layout.layoutsPerDim.forall(layoutPerDim => layoutPerDim.numGhostLayersLeft != numGhosts || layoutPerDim.numGhostLayersRight != numGhosts))
      Logger.error("IR_AddFieldToStorage: Number of ghost layers (left & right) must be identical for all dimensions.")

    val args = ListBuffer[IR_Expression]()
    args += blockForestAcc                                                           // blockstorage
    args += IR_StringConstant(stringIdentifier(slot, onGPU = false))                 // identifier
    args += calculateSize                                                            // calculateSize
    args += IR_Cast(WB_RealType, initVal.access)                                     // initValue
    args += IR_VariableAccess(s"field::${layout.layoutName}", IR_IntegerDatatype)    // layout
    args += IR_Cast(WB_UintType, numGhosts)                                          // nrOfGhostLayers

    // use constructor with StdFieldAlloc allocator to avoid inconsistencies between waLBerla's automatic padding and exa's padding
    if (layout.useFixedLayoutSizes) {
      // TODO: StdFieldAlloc does not use padding, but we cannot use fixed layout sizes otherwise
      //  -> maybe generate own allocator class with fixed size padding?
      args += IR_BooleanConstant(false)                                                                // alwaysInitialize
      args += IR_Native(s"std::function< void ( $wbFieldTemplate * field, IBlock * const block ) >()") // initFunction
      args += IR_Native("Set<SUID>::emptySet()")                                                       // requiredSelectors
      args += IR_Native("Set<SUID>::emptySet()")                                                       // incompatibleSelectors
      args += IR_Native(s"make_shared < field::StdFieldAlloc< $fieldBaseType > >()")                   // alloc
    }

    IR_FunctionCall(IR_ExternalFunctionReference(funcRefName), args)
  }

  def addToStorageGPU(blockForestAcc : IR_VariableAccess, slot : Int, cpuFieldID : IR_WaLBerlaBlockDataID) = {
    val funcRefName = s"gpu::addGPUFieldToStorage<${ IR_WaLBerlaDatatypes.WB_FieldDatatype(this, onGPU = false).prettyprint() }>"

    // TODO: mapping pitched <-> non-pitched fields? maybe possible with help of IVs
    val usePitchedMemory = false // exa fields do not employ pitched memory -> can lead to inconsistent indexing

    val args = ListBuffer[IR_Expression](
      blockForestAcc,
      cpuFieldID,
      IR_StringConstant(stringIdentifier(slot, onGPU = true)),
      IR_BooleanConstant(usePitchedMemory)
    )

    IR_FunctionCall(IR_ExternalFunctionReference(funcRefName), args)
  }

  def domain : IR_Domain = IR_DomainCollection.getByIdentifier("global").get

  override def getFieldAccess(slot : IR_Expression, fragIdx : IR_Expression, index : IR_ExpressionIndex, offset : Option[IR_ConstIndex], frozen : Boolean, matIndex : Option[IR_MatIndex]) : IR_FieldLikeAccess = {
    IR_WaLBerlaFieldAccess(this, slot, fragIdx, index, offset, frozen, matIndex)
  }

  override def getDirectFieldAccess(slot : IR_Expression, fragIdx : IR_Expression, index : IR_ExpressionIndex) : IR_DirectFieldLikeAccess = {
    IR_DirectWaLBerlaFieldAccess(this, slot, fragIdx, index)
  }

  override def getLinearizedFieldAccess(slot : IR_Expression, fragIdx : IR_Expression, index : IR_Expression) : IR_LinearizedFieldLikeAccess = {
    IR_LinearizedWaLBerlaFieldAccess(this, slot, fragIdx, index)
  }

  override def getFieldData(slot : IR_Expression, fragIdx : IR_Expression) : IR_IV_AbstractFieldLikeData = {
    IR_IV_WaLBerlaFieldData(this, slot, fragIdx)
  }
}