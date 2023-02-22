package exastencils.waLBerla.ir.field

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.interfacing._
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes

case class IR_WaLBerlaAddFieldToStorage(wbFields : IR_WaLBerlaField*) extends IR_WaLBerlaFuturePlainFunction {

  def blockForest = IR_WaLBerlaBlockForest()
  def blocks = blockForest.ctorParameter
  def initValue = IR_FunctionArgument("initVal", IR_RealDatatype)

  if (!wbFields.forall(_.name == wbFields.head.name))
    Logger.error("\"IR_WaLBerlaAddFieldToStorage\" used incorrectly. Assumes fields with identical name but potentially different slots and levels.")

  override def isInterfaceFunction : Boolean = false
  override def inlineImplementation : Boolean = false

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    var params : ListBuffer[IR_FunctionArgument] = ListBuffer()
    params += blocks
    params += initValue

    val init = wbFields.sortBy(_.level).flatMap(leveledField => {
      def layout = leveledField.layout

      val numGhosts = layout.layoutsPerDim(0).numGhostLayersLeft
      if (layout.layoutsPerDim.forall(layoutPerDim => layoutPerDim.numGhostLayersLeft != numGhosts || layoutPerDim.numGhostLayersRight != numGhosts))
        Logger.error("IR_AddFieldToStorage: Number of ghost layers (left & right) must be identical for all dimensions.")

      val wbFieldTemplate = IR_WaLBerlaDatatypes.WB_FieldDatatype(leveledField, onGPU = false).prettyprint()

      (0 until leveledField.numSlots).map(slot =>
        IR_FunctionCall(s"${IR_WaLBerlaAddFieldToStorageWrapper().name} < $wbFieldTemplate >",
          blocks.access,
          IR_StringConstant(leveledField.stringIdentifier(slot)),
          leveledField.level,
          initValue.access,
          IR_VariableAccess(s"field::${ layout.layoutName }", IR_IntegerDatatype),
          numGhosts,
          leveledField.layout.useFixedLayoutSizes // TODO: StdFieldAlloc does not use padding, but we cannot use fixed layout sizes otherwise
        ))
    })

    var body : ListBuffer[IR_Statement] = ListBuffer()

    body += IR_Return(IR_InitializerList(init : _*))

    val returnType = IR_WaLBerlaBlockDataID(wbFields.head, slot = 0, onGPU = false).getWrappedDatatype()

    IR_WaLBerlaPlainFunction(name, returnType, params, body)
  }

  override def prettyprint_decl() : String = prettyprint()
  override def name : String = s"addToStorage_${ wbFields.head.name }"
  override def name_=(newName : String) : Unit = name = newName
}
