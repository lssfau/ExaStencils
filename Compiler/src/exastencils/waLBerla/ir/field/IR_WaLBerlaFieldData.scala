package exastencils.waLBerla.ir.field

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.field.ir.IR_FieldAccess
import exastencils.fieldlike.ir.IR_IV_AbstractFieldLikeData
import exastencils.logger.Logger
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks.defIt
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_FieldDatatype
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

object IR_IV_WaLBerlaGetFieldData {
  def apply(fAcc : IR_FieldAccess) : IR_IV_WaLBerlaGetFieldData = {
    val wbfield = IR_WaLBerlaFieldCollection.getByIdentifier(fAcc.name, fAcc.level, suppressError = true).get
    new IR_IV_WaLBerlaGetFieldData(wbfield, fAcc.slot, fAcc.fragIdx)
  }

  def apply(fAcc : IR_WaLBerlaFieldAccess) : IR_IV_WaLBerlaGetFieldData = new IR_IV_WaLBerlaGetFieldData(fAcc.target, fAcc.slot, fAcc.fragIdx)
}

case class IR_IV_WaLBerlaGetFieldData(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_AbstractFieldLikeData(true, false, true, false, false) {

  var level : IR_Expression = field.level

  override def datatype : IR_SpecialDatatype = WB_FieldDatatype(field)

  override def resolveDatatype() : IR_Datatype = {
    if (field.numSlots > 1)
      IR_ArrayDatatype(IR_PointerDatatype(datatype), field.numSlots)
    else
      IR_PointerDatatype(datatype)
  }

  override def resolveDefValue() = Some(0)

  // don't use as global variables
  override def getCtor() : Option[IR_Statement] = None
  override def getDtor() : Option[IR_Statement] = None
  override def registerIV(declarations : mutable.HashMap[String, IR_VariableDeclaration], ctors : mutable.HashMap[String, IR_Statement], dtors : mutable.HashMap[String, IR_Statement]) : Unit = {}

  override def resolveName() : String = field.codeName

  override def getDeclaration() : IR_VariableDeclaration = {

    def getFieldData(slotIt : IR_Expression) = defIt.getData(IR_WaLBerlaBlockDataID(field, slotIt))

    val getSlottedFieldData = if (field.numSlots > 1) {
      IR_InitializerList((0 until field.numSlots).map(s => getFieldData(s)) : _*)
    } else {
      getFieldData(0)
    }

    IR_VariableDeclaration(resolveDatatype(), resolveName(), Some(getSlottedFieldData))
  }

  def getData() = {
    // get field pointer without offset
    new IR_MemberFunctionCallArrowWithDt(this, "data", ListBuffer())
  }

  def getDataAt(index : IR_Index) = {
    // get field pointer at first inner iteration point at "referenceOffset"
    if (index.length() != 4)
      Logger.warn("waLBerla's \"dataAt\" function expects four arguments: x, y, z, f")

    new IR_MemberFunctionCallArrowWithDt(this, "dataAt", index.toExpressionIndex.indices.to[ListBuffer])
  }
}

object IR_IV_WaLBerlaFieldDataAt {
  def apply(fAcc : IR_WaLBerlaFieldAccess) : IR_IV_WaLBerlaFieldDataAt = new IR_IV_WaLBerlaFieldDataAt(fAcc.target, fAcc.slot, fAcc.fragIdx)
}

case class IR_IV_WaLBerlaFieldDataAt(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_IV_AbstractFieldLikeData(true, false, true, false, false)  {

  override var level : IR_Expression = field.level

  // don't use as global variables
  override def getCtor() : Option[IR_Statement] = None
  override def getDtor() : Option[IR_Statement] = None
  override def registerIV(declarations : mutable.HashMap[String, IR_VariableDeclaration], ctors : mutable.HashMap[String, IR_Statement], dtors : mutable.HashMap[String, IR_Statement]) : Unit = {}

  override def resolveName() : String = s"data_${field.codeName}_"

  override def getDeclaration() : IR_VariableDeclaration = {

    def getFieldDataPtr(slotIt : IR_Expression) = {
      if (field.layout.useFixedLayoutSizes) {
        // get ptr without offset -> referenceOffset handled by ExaStencils
        IR_IV_WaLBerlaGetFieldData(field, slotIt, fragmentIdx).getData()
      } else {
        // dataAt(0, 0, 0, 0) already points to first inner iteration point at "referenceOffset" -> referenceOffset not handled by ExaStencils
        if (field.layout.referenceOffset.forall(_ != IR_IntegerConstant(0)))
          Logger.error("IR_IV_WaLBerlaFieldDataAt assumes a referenceOffset of zero")

        // index handling for waLBerla accessors
        val index = Duplicate(field.layout.referenceOffset)
        val newIndex = IR_WaLBerlaUtil.adaptIndexForAccessors(index, field.gridDatatype, field.numDimsGrid, field.layout.numDimsData)

        // dataAt requires 4 arguments: x, y, z, f
        newIndex.indices = Duplicate(newIndex.indices).padTo(4, 0 : IR_Expression)

        IR_IV_WaLBerlaGetFieldData(field, slotIt, fragmentIdx).getDataAt(newIndex)
      }
    }

    val getSlottedFieldPtrs = if (field.numSlots > 1) {
      IR_InitializerList((0 until field.numSlots).map(s => getFieldDataPtr(s)) : _*)
    } else {
      getFieldDataPtr(0)
    }

    IR_VariableDeclaration(resolveDatatype(), resolveName(), Some(getSlottedFieldPtrs))
  }
}