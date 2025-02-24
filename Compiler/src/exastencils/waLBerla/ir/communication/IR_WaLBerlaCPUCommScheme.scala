package exastencils.waLBerla.ir.communication

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_CommScheme
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_FieldDatatype
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_StencilTemplate
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil.make_shared

case class IR_WaLBerlaCPUCommScheme(var wbField : IR_WaLBerlaField, var slot : IR_Expression) extends IR_WaLBerlaCommScheme {

  def basetype = IR_UniquePointerDatatype(WB_CommScheme(onGPU = false))

  def createUniformPackInfo() = {
    val packInfoType = if (Knowledge.waLBerla_useRefinement)
      if (!Knowledge.waLBerla_useConservingRefinementPackInfo) "field::refinement::PackInfoQuadratic" else "field::refinement::PackInfo"
    else
      "field::communication::PackInfo"

    var templateArgs = WB_FieldDatatype(wbField, onGPU = false).prettyprint()
    if (Knowledge.waLBerla_useRefinement)
      templateArgs += s", $WB_StencilTemplate"

    var ctorArgs : ListBuffer[IR_Expression] = ListBuffer(blockDataID)
    if (Knowledge.waLBerla_useRefinement && !Knowledge.waLBerla_useConservingRefinementPackInfo) {
      ctorArgs.prepend(blockForest)

      Knowledge.refinement_interpOrderF2C match {
        case 1 => templateArgs += ", false"
        case 2 => templateArgs += ", true"
        case v => Logger.error(s"Invalid value $v for flag 'refinement_interpOrderF2C' when using comm schemes with refinement.")
      }
    }

    make_shared(s"$packInfoType< $templateArgs >", ctorArgs : _*)
  }

  def name = s"cpuCommScheme_${ wbField.name }"

  def blockDataID = IR_WaLBerlaBlockDataID(wbField, slot, onGPU = false)
  def blockForest = IR_WaLBerlaBlockForest()

  override def isPrivate : Boolean = true

  // init comm scheme function sets up comm scheme for all levels and slots
  override def getCtor() : Option[IR_Statement] = Some(
    IR_FunctionCall(IR_WaLBerlaInitCommSchemes(onGPU = false, wbField).name)
  )
}
