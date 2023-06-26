package exastencils.waLBerla.ir.communication

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.logger.Logger
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.gpu.GPU_WaLBerlaGPUCommScheme
import exastencils.waLBerla.ir.field._
import exastencils.waLBerla.ir.interfacing._
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil._

case class IR_WaLBerlaInitCommSchemes(onGPU :  Boolean, wbFields : IR_WaLBerlaField*) extends IR_WaLBerlaFuturePlainFunction {
  override def name : String =  s"initCommSchemes_${ wbFields.head.name }" + (if (onGPU) "_onGPU" else "")
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint

  if (!wbFields.forall(_.name == wbFields.head.name))
    Logger.error("\"IR_WaLBerlaAddGPUFieldToStorage\" used incorrectly. Assumes fields with identical name but potentially different slots and levels.")

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    val blockForest = IR_WaLBerlaBlockForest()

    var body = ListBuffer[IR_Statement]()

    // wrap comm scheme setup with guard
    def setupCommScheme(commScheme: IR_WaLBerlaCommScheme, setupBody : ListBuffer[IR_Statement]) =
      commScheme.commSchemeNecessaryWrapper(setupBody)

    // comm scheme wrapper
    def getCommScheme(wbf : IR_WaLBerlaField, slotIt : IR_Expression) : IR_WaLBerlaCommScheme = if (onGPU)
      GPU_WaLBerlaGPUCommScheme(wbf, slotIt)
    else
      IR_WaLBerlaCPUCommScheme(wbf, slotIt)

    if (Knowledge.waLBerla_generateCommSchemes) {
      // init comm scheme array
      for (wbf <- wbFields) {
        val slotIt = IR_VariableAccess("slotIt", IR_IntegerDatatype)
        val commScheme = getCommScheme(wbf, slotIt)
        val tagString = if (onGPU) "waLBerla_on_gpu" else "waLBerla"
        val tag = tagString.chars().sum() + wbf.index // increment default tag value per created comm scheme

        // ctors for CPU/GPU comm schemes have slightly different signature
        val args : List[IR_Expression] = if (onGPU)
          List(blockForest, IR_BooleanConstant(Platform.hw_gpu_gpuDirectAvailable), tag) // GPU params
        else
          List(blockForest, tag) // CPU params

        val initCommScheme = IR_ForLoop(IR_VariableDeclaration(slotIt, 0), slotIt < wbf.numSlots, IR_PreIncrement(slotIt),
          ListBuffer[IR_Statement](
            IR_Assignment(commScheme, make_unique(commScheme.basetype.resolveBaseDatatype.prettyprint, args : _*)),
            // TODO: temporary solution. remove after local communication is implemented
            // use buffer mode for local communication
            if (Knowledge.waLBerla_useRefinement && Knowledge.waLBerla_useQuadraticF2CInterpolation)
              IR_MemberFunctionCallArrow(commScheme, "setLocalMode", IR_Native("blockforest::BUFFER"))
            else
              IR_NullStatement
          )
        )

        body += setupCommScheme(commScheme, ListBuffer(initCommScheme))
      }

      // add pack info
      for (wbf <- wbFields) {
        val slotIt = IR_VariableAccess("slotIt", IR_IntegerDatatype)
        val commScheme = getCommScheme(wbf, slotIt)

        val addPackInfo = IR_ForLoop(IR_VariableDeclaration(slotIt, 0), slotIt < wbf.numSlots, IR_PreIncrement(slotIt),
          commScheme.addPackInfo() : IR_Statement)

        body += setupCommScheme(commScheme, ListBuffer(addPackInfo))
      }
    }

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)
  }
}
