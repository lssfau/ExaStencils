package exastencils.waLBerla.ir.gpu

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.communication.ir.IR_IV_AbstractCommBuffer
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_SlotAccess
import exastencils.fieldlike.ir._
import exastencils.parallelization.api.cuda._
import exastencils.waLBerla.ir.blockforest._
import exastencils.waLBerla.ir.field._
import exastencils.waLBerla.ir.grid.IR_WaLBerlaAABB

object GPU_WaLBerlaAdaptKernels extends DefaultStrategy("Handling for CUDA kernels with waLBerla fields") {

  private var waLBerlaFieldPointersForKernel : mutable.HashMap[String, ListBuffer[IR_IV_WaLBerlaFieldData]] = mutable.HashMap()

  private var waLBerlaBufferPointersForKernel : mutable.HashMap[String, ListBuffer[GPU_WaLBerlaBufferDeviceData]] = mutable.HashMap()

  private var adaptedKernelCalls : mutable.HashSet[CUDA_FunctionCall] = mutable.HashSet()
  private var adaptedWrapperFunctions : mutable.HashSet[String] = mutable.HashSet()

  // filter complex data structures which shall not be passed to the kernel (e.g. wb block)
  private val filterList = ListBuffer[IR_Datatype](
    IR_WaLBerlaBlock().resolveDatatype(), IR_WaLBerlaIBlock().resolveDatatype(), IR_WaLBerlaAABB.datatype
  )

  object FindKernelCall extends QuietDefaultStrategy("Find CUDA kernel call in function") {
    var kernelCall : Option[CUDA_FunctionCall] = None

    override def applyStandalone(node : Node) : Unit = {
      kernelCall = None
      super.applyStandalone(node)
    }

    this += Transformation("Find", {
      case fc : CUDA_FunctionCall if kernelCall.isEmpty =>
        kernelCall = Some(fc)
        fc
    })
  }

  def isWrapperFunction(func : IR_Function) = {
    FindKernelCall.applyStandalone(func)
    FindKernelCall.kernelCall.isDefined && func.functionQualifiers == "extern \"C\""
  }

  def applyFilterArg(args : ListBuffer[IR_FunctionArgument]) = args.collect {
    case v : IR_FunctionArgument if !filterList.contains(v.datatype) =>
      v
  }
  def applyFilterExpr(args : ListBuffer[IR_Expression]) = args.collect {
    case v : IR_Expression if !filterList.contains(v.datatype) =>
      v
  }

  def toWrapperName(name : String) = name + CUDA_Kernel.wrapperPostfix

  def getSlottedName(field : IR_WaLBerlaField, slot : IR_Expression, fragmentIdx : IR_Expression) = {
    var identifier = s"${ IR_IV_WaLBerlaFieldData(field, slot, fragmentIdx).resolveName() }_deviceData"
    if (field.numSlots > 1) {
      slot match {
        case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case other                    => identifier += s"_s${ other.prettyprint() }"
      }
    }
    identifier
  }

  def getWaLBerlaField(fieldData : IR_IV_AbstractFieldLikeData) = IR_WaLBerlaFieldCollection.getByIdentifier(fieldData.field.name, fieldData.field.level).get

  def getFunctionArgForWaLBerlaField(waLBerlaField : IR_WaLBerlaField, slot : IR_Expression, fragmentIdx : IR_Expression) =
    IR_FunctionArgument(getSlottedName(waLBerlaField, slot, fragmentIdx), IR_PointerDatatype(waLBerlaField.resolveDeclType))

  def getFunctionArgForWaLBerlaBuffer(waLBerlaBuffer : GPU_WaLBerlaBufferDeviceData) =
    IR_FunctionArgument(waLBerlaBuffer.resolveName(), waLBerlaBuffer.baseDatatype)

  this += Transformation("Prepare wrapper function for new field/buffer args and apply filter", {
    case func : IR_Function if CUDA_KernelFunctions.get.functions.contains(func) && isWrapperFunction(func) =>
      val kernelCall = FindKernelCall.kernelCall.get
      val wbFieldData : ListBuffer[IR_IV_AbstractFieldLikeData] = kernelCall.arguments.collect {
        case fieldData : IR_IV_AbstractFieldLikeData if IR_WaLBerlaFieldCollection.exists(fieldData.field.name, fieldData.field.level) => fieldData
      }
      val wbBufferData : ListBuffer[GPU_WaLBerlaBufferDeviceData] = kernelCall.arguments.collect {
        case bufferData : IR_IV_AbstractCommBuffer if IR_WaLBerlaFieldCollection.exists(bufferData.field.name, bufferData.field.level) =>
          GPU_WaLBerlaBufferDeviceData(bufferData.field, bufferData.send, bufferData.size, bufferData.neighIdx, bufferData.concurrencyId, bufferData.indexOfRefinedNeighbor, bufferData.fragmentIdx)
      }

      // remember field data pointers associated with kernel
      if (wbFieldData.nonEmpty || wbBufferData.nonEmpty) {
        adaptedKernelCalls += kernelCall
        adaptedWrapperFunctions += func.name

        // extend wrapper parameters
        val wbFieldDataParams = wbFieldData.map(fieldData => getFunctionArgForWaLBerlaField(getWaLBerlaField(fieldData), fieldData.slot, fieldData.fragmentIdx))
        val wbBufferDataParams = wbBufferData.map(bufferData => getFunctionArgForWaLBerlaBuffer(bufferData))

        func.parameters ++= wbFieldDataParams
        func.parameters ++= wbBufferDataParams

        // save employed wb field/buffer data for further processing
        waLBerlaFieldPointersForKernel += (func.name -> wbFieldData.map(fieldData => IR_IV_WaLBerlaFieldData(getWaLBerlaField(fieldData), fieldData.slot, fieldData.fragmentIdx)))
        waLBerlaBufferPointersForKernel += (func.name -> wbBufferData)
      }

      // apply argument/parameter filter
      val filtered = applyFilterArg(func.parameters)
      if (filtered.size != func.parameters.size) {
        adaptedKernelCalls += kernelCall
        adaptedWrapperFunctions += func.name

        func.parameters = filtered
      }

      func
  })

  this += Transformation("Apply filter to wrapper kernel function call", {
    case funcCall : IR_FunctionCall if adaptedWrapperFunctions.contains(funcCall.name) =>
      val filtered = applyFilterExpr(funcCall.arguments)
      if (filtered.size != funcCall.arguments.size)
        funcCall.arguments = filtered

      funcCall
  })

  this += Transformation("Adapt call to kernel wrapper function for fields/buffers", {
    case funcCall @ IR_FunctionCall(func, _) if waLBerlaFieldPointersForKernel.contains(func.name) && adaptedWrapperFunctions.contains(funcCall.name) =>
      funcCall.arguments = Duplicate(funcCall.arguments) ++
        waLBerlaFieldPointersForKernel(func.name) ++ // use wb field data pointers
        waLBerlaBufferPointersForKernel(func.name)   // use wb comm buffer pointers

      funcCall
  })

  this += Transformation("Apply filter to kernel function parameters", {
    case func : IR_Function if adaptedWrapperFunctions.contains(toWrapperName(func.name)) =>
      val filtered = applyFilterArg(func.parameters)
      if (filtered.size != func.parameters.size)
        func.parameters = filtered

      func
  })

  this += Transformation("Change kernel call args for fields/buffers and apply filter", {
    case fc : CUDA_FunctionCall if adaptedKernelCalls.contains(fc) =>
      // use waLBerla fieldData in kernel call
      val newArgs = Duplicate(fc.arguments)
        .map { // re-map pointers to waLBerla data
          case deviceData : CUDA_FieldDeviceData if IR_WaLBerlaFieldCollection.exists(deviceData.field.name, deviceData.field.level)                                            =>
            getFunctionArgForWaLBerlaField(getWaLBerlaField(deviceData), deviceData.slot, deviceData.fragmentIdx).access
          case _ @ CUDA_BufferDeviceData(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx) if IR_WaLBerlaFieldCollection.objects.contains(field) =>
            val wbDeviceBuffer = GPU_WaLBerlaBufferDeviceData(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)
            getFunctionArgForWaLBerlaBuffer(wbDeviceBuffer).access
          case arg                                                                                                                                                              =>
            arg
        }

      // also apply filter to kernel call args
      val filtered = applyFilterExpr(newArgs)
      if (newArgs.size != filtered.size)
        fc.arguments = filtered
      else
        fc.arguments = newArgs

      fc
  })
}
