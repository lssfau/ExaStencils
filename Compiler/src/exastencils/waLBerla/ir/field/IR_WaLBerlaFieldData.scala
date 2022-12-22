package exastencils.waLBerla.ir.field

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.fieldlike.ir.IR_IV_AbstractFieldLikeData
import exastencils.logger.Logger
import exastencils.parallelization.api.cuda.CUDA_Util
import exastencils.prettyprinting.PpStream
import exastencils.util.NoDuplicateWrapper
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks.defIt
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_FieldDatatype
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

/// IR_IV_AbstractWaLBerlaFieldData

abstract class IR_IV_AbstractWaLBerlaFieldData extends IR_IV_AbstractFieldLikeData(true, false, true, false, false) {
  var field : IR_WaLBerlaField
}

/// IR_IV_GetWaLBerlaFieldFromScope

sealed trait IR_IV_GetWaLBerlaFieldFromScope extends IR_InternalVariable {
  // don't use as global variables
  override def getCtor() : Option[IR_Statement] = None
  override def getDtor() : Option[IR_Statement] = None
  override def registerIV(declarations : mutable.HashMap[String, IR_VariableDeclaration], ctors : mutable.HashMap[String, IR_Statement], dtors : mutable.HashMap[String, IR_Statement]) : Unit = {}

  def getDeclarationBlockLoop(executionChoice : NoDuplicateWrapper[IR_Expression]) : ListBuffer[IR_Statement]
}

/// IR_IV_WaLBerlaGetField

// TODO: rewrite declare functions for IR_IV_WaLBerlaGetField & IR_IV_WaLBerlaFieldData

case class IR_IV_WaLBerlaGetField(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var onGPU : Boolean,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_InternalVariable(true, false, true, false, false) with IR_IV_GetWaLBerlaFieldFromScope {

  var level : IR_Expression = field.level

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def datatype : IR_SpecialDatatype = WB_FieldDatatype(field, onGPU)

  override def resolveDatatype() : IR_Datatype = {
    if (field.numSlots > 1)
      IR_ArrayDatatype(IR_PointerDatatype(datatype), field.numSlots)
    else
      IR_PointerDatatype(datatype)
  }

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index, level, IR_NullExpression)

  override def resolveDefValue() = Some(0)

  override def resolveName() : String = field.codeName

  private val acc = IR_VariableAccess(resolveName(), resolveDatatype())

  private def getSlottedFieldData(onGPU : Boolean) : ListBuffer[IR_Statement] = if (field.numSlots > 1)
    (0 until field.numSlots).map(s => IR_Assignment(IR_ArrayAccess(acc, s), getFieldData(s, onGPU)) : IR_Statement).to[ListBuffer]
  else
    ListBuffer[IR_Statement](IR_Assignment(acc, getFieldData(0, onGPU)))

  private def getFieldData(slotIt : IR_Expression, onGPU : Boolean) = defIt.getData(IR_WaLBerlaBlockDataID(field, slotIt, onGPU))

  def getDeclarationBlockLoop() : ListBuffer[IR_Statement] = {
    IR_VariableDeclaration(acc) +: getSlottedFieldData(onGPU)
  }

  override def getDeclarationBlockLoop(executionChoice : NoDuplicateWrapper[IR_Expression]) : ListBuffer[IR_Statement] = {
    // TODO: separate CUDA handling?
    if (Knowledge.cuda_enabled) {
      val branch = IR_IfCondition(IR_VariableAccess("replaceIn_CUDA_AnnotateLoops", IR_BooleanDatatype),
        getSlottedFieldData(false),
        getSlottedFieldData(true))
      branch.annotate(CUDA_Util.CUDA_BRANCH_CONDITION, executionChoice)
      ListBuffer[IR_Statement](branch)

      ListBuffer[IR_Statement](
        IR_VariableDeclaration(acc),
        branch)
    } else {
      IR_VariableDeclaration(acc) +: getSlottedFieldData(false)
    }
  }

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = super.resolveAccess(baseAccess, fragment, domain, field, level, neigh)
    if (this.field.numSlots > 1)
      access = IR_ArrayAccess(access, slot)
    access
  }
}

/// IR_IV_WaLBerlaFieldData

object IR_IV_WaLBerlaFieldData {
  def apply(fAcc : IR_MultiDimWaLBerlaFieldAccess) : IR_IV_WaLBerlaFieldData = new IR_IV_WaLBerlaFieldData(fAcc.target, fAcc.slot, fAcc.fragIdx)

  def apply(fAcc : IR_WaLBerlaFieldAccess) : IR_IV_WaLBerlaFieldData = new IR_IV_WaLBerlaFieldData(fAcc.target, fAcc.slot, fAcc.fragIdx)
}

case class IR_IV_WaLBerlaFieldData(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_IV_AbstractWaLBerlaFieldData with IR_IV_GetWaLBerlaFieldFromScope {

  override var level : IR_Expression = field.level

  override def resolveName() : String = s"data_${ field.codeName }_"

  override def getDeclarationBlockLoop(executionChoice : NoDuplicateWrapper[IR_Expression]) : ListBuffer[IR_Statement] = {

    def getFieldDataPtr(slotIt : IR_Expression, onGPU : Boolean) = {
      val fieldFromBlock = IR_IV_WaLBerlaGetField(field, slotIt, onGPU, fragmentIdx)

      if (field.layout.useFixedLayoutSizes) {
        // get ptr without offset -> referenceOffset handled by ExaStencils
        val ptr = new IR_MemberFunctionCallArrowWithDt(fieldFromBlock, "data", ListBuffer())
        if (onGPU)
          IR_Cast(IR_PointerDatatype(field.resolveBaseDatatype), ptr) // "data" function of GPU fields returns pitched void*
        else
          ptr
      } else {
        // dataAt(0, 0, 0, 0) already points to first inner iteration point at "referenceOffset" -> referenceOffset not handled by ExaStencils
        if (field.layout.referenceOffset.forall(_ != IR_IntegerConstant(0)))
          Logger.error("IR_IV_WaLBerlaFieldDataAt assumes a referenceOffset of zero")

        // index handling for waLBerla accessors
        val index = Duplicate(field.layout.referenceOffset)
        val newIndex = IR_WaLBerlaUtil.adaptIndexForAccessors(index, field.gridDatatype, field.numDimsGrid, field.layout.numDimsData)

        // dataAt requires 4 arguments: x, y, z, f
        newIndex.indices = Duplicate(newIndex.indices).padTo(4, 0 : IR_Expression)

        // get field pointer at first inner iteration point at "referenceOffset"
        if (newIndex.length != 4)
          Logger.warn("waLBerla's \"dataAt\" function expects four arguments: x, y, z, f")

        new IR_MemberFunctionCallArrowWithDt(fieldFromBlock, "dataAt", newIndex.toExpressionIndex.indices.to[ListBuffer])
      }
    }

    val acc = IR_VariableAccess(resolveName(), resolveDatatype())
    def getSlottedFieldPtrs(onGPU : Boolean) = if (field.numSlots > 1)
      (0 until field.numSlots).map(s => IR_Assignment(IR_ArrayAccess(acc, s), getFieldDataPtr(s, onGPU)) : IR_Statement).to[ListBuffer]
    else
      ListBuffer[IR_Statement](IR_Assignment(acc, getFieldDataPtr(0, onGPU)))

    def getField(onGPU : Boolean) = IR_IV_WaLBerlaGetField(field, slot, onGPU, fragmentIdx).getDeclarationBlockLoop()

    // TODO: hacky
    if (Knowledge.cuda_enabled) {
      val branch = IR_IfCondition(IR_VariableAccess("replaceIn_CUDA_AnnotateLoops", IR_BooleanDatatype),
        IR_IfCondition(1, getField(false) ++ getSlottedFieldPtrs(false)),
        IR_IfCondition(1, IR_Scope(getField(true) ++ getSlottedFieldPtrs(true))))
      branch.annotate(CUDA_Util.CUDA_BRANCH_CONDITION, executionChoice)
      ListBuffer[IR_Statement](branch)

      ListBuffer[IR_Statement](
        IR_VariableDeclaration(acc),
        branch)
    } else {
      ListBuffer[IR_Statement](
        IR_VariableDeclaration(acc),
        IR_IfCondition(1, IR_Scope(getField(false) ++ getSlottedFieldPtrs(false))))
    }
  }
}