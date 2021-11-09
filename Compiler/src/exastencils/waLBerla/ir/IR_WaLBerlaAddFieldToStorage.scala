package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Cast
import exastencils.base.ir.IR_ConstReferenceDatatype
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_InitializerList
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Return
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_VariableAccess
import exastencils.logger.Logger
import exastencils.waLBerla.ir.IR_WaLBerlaUtil.blockForestPtr

case class IR_WaLBerlaAddFieldToStorage(wbField : IR_WaLBerlaField) extends IR_WaLBerlaFutureLeveledFunction {

  /*  TODO: make use of alternative ctor with parameter for (e.g. leveled) waLBerla fields:
    const std::function< Vector3< uint_t > ( const shared_ptr< StructuredBlockStorage > &, IBlock * const ) >& calculateSize
  */

  def level = wbField.level
  def blocks = IR_FunctionArgument(IR_VariableAccess(blockForestPtr.name, IR_ConstReferenceDatatype(blockForestPtr.datatype)))
  def initValue = IR_FunctionArgument("initVal", IR_RealDatatype) // TODO: maybe define on L4
  def layoutSpecifier = IR_VariableAccess(s"field::${wbField.layout.layoutName}", IR_IntegerDatatype)
  def numGhosts : Int = wbField.layout.layoutsPerDim(0).numGhostLayersLeft

  def isSlotted = wbField.numSlots > 1
  def fieldIdentifier(slot : Int) = wbField.name + (if (isSlotted) s"_$slot" else "")
  def baseType = IR_WaLBerlaDatatypes.WB_BlockDataID
  def datatype : IR_SpecialDatatype = if (isSlotted) IR_SpecialDatatype(s"std::array< ${baseType.prettyprint()}, ${wbField.numSlots} >") else baseType

  if (wbField.layout.layoutsPerDim.forall(layoutPerDim => layoutPerDim.numGhostLayersLeft != numGhosts || layoutPerDim.numGhostLayersRight != numGhosts))
    Logger.error("IR_AddFieldToStorage: Number of ghost layers (left & right) must be identical for all dimensions.")

  override def generateFct() : IR_WaLBerlaLeveledFunction = {
    // add deps
    IR_WaLBerlaCollection.get.addExternalDependency("field/Field.h")
    IR_WaLBerlaCollection.get.addExternalDependency("field/AddToStorage.h")

    var params : ListBuffer[IR_FunctionArgument] = ListBuffer()
    params += blocks
    params += initValue

    val init = (0 until wbField.numSlots).map(slot => {
      new IR_FunctionCall(IR_ExternalFunctionReference(s"field::addToStorage<${IR_WaLBerlaDatatypes.WB_FieldDatatype(wbField).prettyprint()}>"),
        ListBuffer(
          blocks.access,
          IR_StringConstant(fieldIdentifier(slot)),
          IR_Cast(IR_SpecialDatatype("real_t"), initValue.access),
          layoutSpecifier,
          IR_Cast(IR_SpecialDatatype("uint_t"), numGhosts)))
    })

    var body : ListBuffer[IR_Statement] = ListBuffer()
    body += IR_Return(IR_InitializerList(init : _*))

    val func = IR_WaLBerlaLeveledFunction(name, level, datatype, params, body)
    func.isInterfaceFunction = false
    func
  }

  override def prettyprint_decl() : String = prettyprint()
  override def name : String = s"addToStorage_${wbField.name}"
  override def name_=(newName : String) : Unit = name = newName
}
