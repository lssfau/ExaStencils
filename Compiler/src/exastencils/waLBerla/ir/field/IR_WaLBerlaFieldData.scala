package exastencils.waLBerla.ir.field

import scala.collection.mutable.AbstractMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.fieldlike.ir.IR_IV_AbstractFieldLikeData
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLocalBlocks
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlocks
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_FieldDatatype

/// IR_IV_AbstractWaLBerlaFieldData

abstract class IR_IV_AbstractWaLBerlaFieldData extends IR_IV_AbstractFieldLikeData(false, false, true, false, false) {
  var field : IR_WaLBerlaField
}

sealed trait IR_IV_GetWaLBerlaFieldFromScope extends IR_InternalVariable {
  // don't use as global variables
  override def getCtor() : Option[IR_Statement] = None
  override def getDtor() : Option[IR_Statement] = None
  override def registerIV(declarations : AbstractMap[String, IR_VariableDeclaration], ctors : AbstractMap[String, IR_Statement], dtors : AbstractMap[String, IR_Statement]) : Unit = {}
}

/// IR_IV_WaLBerlaGetFieldPointer

abstract class IR_IV_WaLBerlaGetFieldPointer extends IR_WaLBerlaInterfaceMember(true, true, false) {

  def field : IR_WaLBerlaField
  def level : IR_Expression
  def fragmentIdx : IR_Expression
  def slot : IR_Expression

  private val levels = IR_WaLBerlaFieldCollection.getAllByIdentifier(field.name, suppressError = true).map(_.level)
  override def minLevel : Int = levels.min
  override def maxLevel : Int = levels.max

  def baseDatatype() : IR_Datatype
  override def resolveDatatype() : IR_Datatype = {
    var dt : IR_Datatype = IR_PointerDatatype(baseDatatype())

    if (field.numSlots > 1)
      dt = IR_StdArrayDatatype(dt, field.numSlots)
    dt
  }

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess()

  override def resolveDefValue() = Some(0)

  protected def resolveAccess() : IR_Expression = resolveAccess(resolveMemberBaseAccess(), fragmentIdx, level, IR_NullExpression)

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var baseAccess : IR_Access = resolveMemberBaseAccess()
    var access = super.resolveAccess(baseAccess, fragment, level, neigh)

    if (this.field.numSlots > 1)
      access = IR_ArrayAccess(access, slot)
    access
  }
}

/// IR_IV_WaLBerlaGetField

case class IR_IV_WaLBerlaGetField(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var onGPU : Boolean,
    var fragmentIdx : IR_Expression = IR_WaLBerlaLoopOverLocalBlocks.defIt
) extends IR_IV_WaLBerlaGetFieldPointer {

  override def baseDatatype() : IR_Datatype = WB_FieldDatatype(field, onGPU)

  var level : IR_Expression = field.level

  // init function sets up field instances for all levels and slots
  override def getCtor() : Option[IR_Statement] = {
    if (!Knowledge.waLBerla_cacheFieldPointers)
      None
    else
      Some(IR_FunctionCall(IR_WaLBerlaInitFieldInstances(onGPU, field).name))
  }

  def name : String = field.name + (if (onGPU) "_onGPU" else "")
  override def isPrivate : Boolean = true
}

/// IR_IV_WaLBerlaGetFieldData

case class IR_IV_WaLBerlaGetFieldData(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var onGPU : Boolean,
    var fragmentIdx : IR_Expression = IR_WaLBerlaLoopOverLocalBlocks.defIt
) extends IR_IV_WaLBerlaGetFieldPointer {

  var level : IR_Expression = field.level

  // init function sets up field data pointers for all levels and slots
  override def getCtor() : Option[IR_Statement] = {
    if (!Knowledge.waLBerla_cacheFieldPointers)
      None
    else
      Some(IR_FunctionCall(IR_WaLBerlaInitFieldDataPtrs(onGPU, field).name))
  }

  def name : String = field.name + "dataPtr" + (if (onGPU) "_onGPU" else "")
  override def isPrivate : Boolean = true

  override def baseDatatype() : IR_Datatype = field.resolveBaseDatatype
}

/// IR_IV_WaLBerlaFieldData

object IR_IV_WaLBerlaFieldData {
  def apply(fAcc : IR_MultiDimWaLBerlaFieldAccess) : IR_IV_WaLBerlaFieldData = new IR_IV_WaLBerlaFieldData(fAcc.target, fAcc.slot, fAcc.fragIdx)

  def apply(fAcc : IR_WaLBerlaFieldAccess) : IR_IV_WaLBerlaFieldData = new IR_IV_WaLBerlaFieldData(fAcc.target, fAcc.slot, fAcc.fragIdx)
}

case class IR_IV_WaLBerlaFieldData(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var fragmentIdx : IR_Expression = IR_WaLBerlaLoopOverLocalBlocks.defIt
) extends IR_IV_AbstractWaLBerlaFieldData with IR_IV_GetWaLBerlaFieldFromScope {

  override var level : IR_Expression = field.level

  private def fragmentIdxName = fragmentIdx.prettyprint().replaceAll("[\\[\\](){}]", "") // remove brackets
  override def resolveName() : String = s"data_${ field.codeName }_$fragmentIdxName" // get unique name per field name, level and fragmentIdx

  private val acc = IR_VariableAccess(resolveName(), resolveDatatype())

  override def getDeclaration() = IR_VariableDeclaration(acc)

  // CPU/GPU execution information not incorporated in class -> we need to make sure it is initialized in the correct mode (see IR_WaLBerlaLoopOverBlocks)
  def initInBlockLoop(onGPU : Boolean) : ListBuffer[IR_Statement] = {
    def getFieldDataPtr(slotIt : IR_Expression) = {
      val dataPtr = IR_IV_WaLBerlaGetFieldData(field, slotIt, onGPU, fragmentIdx)

      if (fragmentIdxName != IR_WaLBerlaLoopOverLocalBlocks.defIt.name)
        IR_TernaryCondition(fragmentIdx >= 0 AndAnd fragmentIdx < IR_WaLBerlaLocalBlocks().size(), dataPtr, IR_VariableAccess("nullptr", IR_UnknownDatatype))
      else
        dataPtr
    }

    var body : ListBuffer[IR_Statement] = ListBuffer()

    // if not already cached in interface: fetch field instances and data pointers in loop
    if (!Knowledge.waLBerla_cacheFieldPointers) {
      body += IR_WaLBerlaInitFieldInstances.initRoutine(onGPU, field)
      body += IR_WaLBerlaInitFieldDataPtrs.initRoutine(onGPU, field)
    }

    if (field.numSlots > 1)
      body ++= (0 until field.numSlots).map(s => IR_Assignment(IR_ArrayAccess(acc, s), getFieldDataPtr(s)) : IR_Statement)
    else
      body += IR_Assignment(acc, getFieldDataPtr(0))

    body
  }
}