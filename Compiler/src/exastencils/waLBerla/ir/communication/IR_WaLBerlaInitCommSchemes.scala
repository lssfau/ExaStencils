package exastencils.waLBerla.ir.communication

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.cuda.CUDA_WaLBerlaGPUCommScheme
import exastencils.waLBerla.ir.field._
import exastencils.waLBerla.ir.interfacing._
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil._

object IR_WaLBerlaInitCommSchemes {
  var fctName : String = "initCommSchemes"
}

case class IR_WaLBerlaInitCommSchemes() extends IR_WaLBerlaFuturePlainFunction {
  override def name : String = IR_WaLBerlaInitCommSchemes.fctName
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint

  override def isInterfaceFunction : Boolean = true
  override def inlineImplementation : Boolean = true

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {

    // add deps
    if (Knowledge.cuda_enabled) {
      IR_WaLBerlaCollection.get.addExternalDependency("cuda/communication/GPUPackInfo.h")
      IR_WaLBerlaCollection.get.addExternalDependency("cuda/communication/MemcpyPackInfo.h")
      IR_WaLBerlaCollection.get.addExternalDependency("cuda/communication/UniformGPUScheme.h")
    }

    val blockForest = IR_WaLBerlaBlockForest()
    val wbFieldsPerLevel = IR_WaLBerlaFieldCollection.objects.groupBy(_.codeName).map(_._2.head).to[ListBuffer] // find unique wb fields per level
      .sortBy(_.level).sortBy(_.name)

    var body = ListBuffer[IR_Statement]()

    if (Knowledge.waLBerla_generateCommSchemes) {
      // init comm scheme array
      var ctr = 0
      val slotsPerField = wbFieldsPerLevel.map(_.numSlots)
      val maxSlots = if (slotsPerField.nonEmpty) slotsPerField.max else 1
      for (wbf <- wbFieldsPerLevel) {
        for (s <- 0 until wbf.numSlots) {
          val commSchemes : ListBuffer[IR_WaLBerlaCommScheme] = ListBuffer(IR_WaLBerlaCPUCommScheme(wbf, s))
          val tagCPU = "waLBerla".chars().sum() + ctr // increment default tag value per created comm scheme
          val tagGPU = "waLBerla_on_gpu".chars().sum() + wbFieldsPerLevel.size * maxSlots + ctr

          if (Knowledge.cuda_enabled)
            commSchemes += CUDA_WaLBerlaGPUCommScheme(wbf, s)

          for (commScheme <- commSchemes) {
            // ctors for CPU/GPU comm schemes have slightly different signature
            val args : List[IR_Expression] = if (commScheme.isInstanceOf[CUDA_WaLBerlaGPUCommScheme])
              List(blockForest, IR_BooleanConstant(Platform.hw_gpu_gpuDirectAvailable), tagGPU) // GPU params
            else
              List(blockForest, tagCPU) // CPU params

            body += IR_Assignment(commScheme.resolveAccess(), make_unique(commScheme.basetype.resolveBaseDatatype.prettyprint, args : _*))
            ctr += 1
          }
        }
      }

      // add pack info
      var addPackInfo = ListBuffer[IR_Statement]()
      for (wbf <- wbFieldsPerLevel) {
        val slotIt = IR_VariableAccess("slotIt", IR_IntegerDatatype)
        val commSchemes : ListBuffer[IR_WaLBerlaCommScheme] = ListBuffer(IR_WaLBerlaCPUCommScheme(wbf, slotIt))

        if (Knowledge.cuda_enabled)
          commSchemes += CUDA_WaLBerlaGPUCommScheme(wbf, slotIt)

        for (commScheme <- commSchemes) {
          addPackInfo += IR_ForLoop(IR_VariableDeclaration(slotIt, 0), slotIt < wbf.numSlots, IR_PreIncrement(slotIt),
            commScheme.addPackInfo() : IR_Statement)
        }
      }
      body += (if (Knowledge.waLBerla_useGridFromExa)
        IR_IfCondition(Knowledge.domain_numFragmentsTotal > 1, addPackInfo)
      else
        IR_IfCondition(blockForest.getNumberOfAllRootBlocks() > 1, addPackInfo))
    }

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)
  }
}
