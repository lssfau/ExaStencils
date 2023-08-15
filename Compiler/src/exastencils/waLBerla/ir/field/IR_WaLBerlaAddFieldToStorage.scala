package exastencils.waLBerla.ir.field

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.interfacing._
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes

case class IR_WaLBerlaAddFieldToStorage(wbFields : IR_WaLBerlaField*) extends IR_Statement with IR_SpecialExpandable {
  if (!wbFields.forall(_.name == wbFields.head.name))
    Logger.error("\"IR_WaLBerlaAddFieldToStorage\" used incorrectly. Assumes fields with identical name but potentially different slots and levels.")

  def blockForest = IR_WaLBerlaBlockForest()
  def initValue = IR_RealConstant(0)

  def expandSpecial() = {
    var body : ListBuffer[IR_Statement] = ListBuffer()

    wbFields.sortBy(_.level).flatMap(leveledField => {
      def refArrayToInit(slot : IR_Expression) = IR_WaLBerlaBlockDataID(leveledField, slot, onGPU = false)

      def layout = leveledField.layout

      val numGhosts = layout.layoutsPerDim(0).numGhostLayersLeft
      if (layout.layoutsPerDim.forall(layoutPerDim => layoutPerDim.numGhostLayersLeft != numGhosts || layoutPerDim.numGhostLayersRight != numGhosts))
        Logger.error("IR_AddFieldToStorage: Number of ghost layers (left & right) must be identical for all dimensions.")

      val wbFieldTemplate = IR_WaLBerlaDatatypes.WB_FieldDatatype(leveledField, onGPU = false).prettyprint()

      (0 until leveledField.numSlots).map(slot =>
        body += IR_Assignment(
          refArrayToInit(slot),
          IR_FunctionCall(s"${ IR_WaLBerlaAddFieldToStorageWrapper().name } < $wbFieldTemplate >",
            blockForest,
            IR_StringConstant(leveledField.stringIdentifier(slot, onGPU = false)),
            leveledField.level,
            initValue,
            IR_VariableAccess(s"field::${ layout.layoutName }", IR_IntegerDatatype),
            numGhosts,
            leveledField.layout.useFixedLayoutSizes // TODO: StdFieldAlloc does not use padding, but we cannot use fixed layout sizes otherwise
          )))
    })

    IR_Scope(body)
  }
}
