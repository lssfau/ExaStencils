package exastencils.performance

import scala.collection.mutable
import scala.collection.mutable._

import java.io.PrintWriter

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.util.ir.IR_ReplaceVariableAccess

/// util classes

case class PerformanceEstimate(var host : Double, var device : Double) {
  // TODO: integrate host/device choices to estimate final execution time
  def +(other : PerformanceEstimate) = PerformanceEstimate(host + other.host, device + other.device)
}

/// control function

object AddPerformanceEstimates {
  def apply() = {
    CollectFunctionStatements.apply()
    EvaluatePerformanceEstimates.doUntilDone()
  }
}

/// top level strategies

object CollectFunctionStatements extends DefaultStrategy("Collecting internal function statements") with ObjectWithState {
  var internalFunctions = HashSet[String]()

  override def clear() = internalFunctions.clear()

  override def apply(node : Option[Node] = None) : Unit = {
    clear()
    super.apply(node)
  }

  this += new Transformation("Collecting", {
    case fct : IR_Function =>
      internalFunctions += fct.name
      fct
  }, false)
}

object EvaluatePerformanceEstimates extends DefaultStrategy("Evaluating performance estimates") with ObjectWithState {
  var completeFunctions = HashMap[String, PerformanceEstimate]()
  var unknownFunctionCalls = true
  var outputStream : PrintWriter = null

  override def clear() = {
    completeFunctions.clear()
    unknownFunctionCalls = true
    outputStream = null
  }

  override def apply(node : Option[Node] = None) : Unit = {
    realApplyAndDo(false, node)
  }

  def doUntilDone(node : Option[Node] = None) : Unit = {
    realApplyAndDo(true, node)
  }

  private def realApplyAndDo(doUntilDone : Boolean, node : Option[Node] = None) : Unit = {
    if (doUntilDone) {
      // doUntilDone
      var cnt = 0
      unknownFunctionCalls = true
      while (unknownFunctionCalls && cnt < 128) {
        unknownFunctionCalls = false
        super.apply(node)
        cnt += 1
      }
    } else { // apply() once
      unknownFunctionCalls = false
      super.apply(node)
    }

    if (true) {
      // TODO: add flag to control behavior

      val targetFile = Settings.performanceEstimateOutputFile
      if (!new java.io.File(targetFile).exists) {
        val file = new java.io.File(targetFile)
        if (!file.getParentFile.exists()) file.getParentFile.mkdirs()
      }

      outputStream = new PrintWriter(targetFile)

      val sep = Settings.csvSeparator
      for (fct <- completeFunctions.toList.sortBy(_._1)) {
        val fctName = fct._1
        val estimate = fct._2
        outputStream.println("%s%s%s%s%e".formatLocal(java.util.Locale.US,
          "function", sep, fctName, sep, estimate.host * 1000.0)) //ms
      }

      outputStream.close()
      outputStream = null
    }
  }

  this += new Transformation("Processing function statements", {
    case fct : IR_Function if !completeFunctions.contains(fct.name) &&
      CollectFunctionStatements.internalFunctions.contains(fct.name) =>
      // process function body
      EvaluatePerformanceEstimates_SubAST.applyStandalone(fct.body)

      if (EvaluatePerformanceEstimates_SubAST.unknownFunctionCalls) {
        unknownFunctionCalls = true
      } else {
        val estimatedTime = EvaluatePerformanceEstimates_SubAST.lastEstimate

        fct.annotate("perf_timeEstimate_host", estimatedTime.host)
        fct.annotate("perf_timeEstimate_device", estimatedTime.device)

        val hostTimeMs : Double = estimatedTime.host * 1000.0

        completeFunctions.put(fct.name, estimatedTime)
        fct.body.prepend(
          IR_Comment(s"Estimated host time for function: ${ hostTimeMs } ms"),
          IR_Comment(s"Estimated device time for function: ${ estimatedTime.device * 1000.0 } ms"))

      }
      fct
  })
}

/// local util strategies

object EvaluatePerformanceEstimates_SubAST extends QuietDefaultStrategy("Estimating performance for sub-ASTs") {
  // TODO:  loops with conditions, reductions
  //        while loops
  //        condition statements

  var unknownFunctionCalls = false
  var estimatedTimeSubAST = Stack[PerformanceEstimate]()
  var lastEstimate = PerformanceEstimate(0.0, 0.0)

  def addTimeToStack(estimate : PerformanceEstimate) : Unit = {
    estimatedTimeSubAST.push(estimatedTimeSubAST.pop + estimate)
  }
  def addTimeToStack(nodeWithAnnotation : Node) : Unit = {
    addTimeToStack(PerformanceEstimate(
      nodeWithAnnotation.getAnnotation("perf_timeEstimate_host").get.asInstanceOf[Double],
      nodeWithAnnotation.getAnnotation("perf_timeEstimate_device").get.asInstanceOf[Double]))
  }

  def addLoopTimeToStack(loop : IR_ForLoop) : Unit = {
    //    Knowledge.experimental_cuda_preferredExecution match {
    //      case "Host"   => addTimeToStack(loop.getAnnotation("perf_timeEstimate_host").get.value.asInstanceOf[Double])
    //      case "Device" => addTimeToStack(loop.getAnnotation("perf_timeEstimate_device").get.value.asInstanceOf[Double])
    //      case "Performance" => addTimeToStack(
    //        math.min(
    //          loop.getAnnotation("perf_timeEstimate_host").get.value.asInstanceOf[Double],
    //          loop.getAnnotation("perf_timeEstimate_device").get.value.asInstanceOf[Double]))
    //    }
    addTimeToStack(loop)
  }

  def dataPerIteration(fieldAccesses : HashMap[String, IR_Datatype], offsets : HashMap[String, ListBuffer[Long]]) : Int = {
    if (fieldAccesses.values.map(_.typicalByteSize).toList.distinct.length > 1)
      Logger.error("Unsupported: Not the same Datatype")

    val dataTypeSize = fieldAccesses.values.head.typicalByteSize
    //like old estimation model, for perfect blocking (after find blocking factor)
    if (Knowledge.opt_loopBlocked) {
      return dataTypeSize * fieldAccesses.keys.size
    }
    var cacheSize = Platform.hw_cacheSize
    val numberOfThreadsUsed = Knowledge.omp_numThreads
    // multi thread
    val numberOfCaches = Platform.hw_numCacheSharingThreads / Platform.hw_cpu_numCoresPerCPU

    if (numberOfThreadsUsed > numberOfCaches) {
      cacheSize = (cacheSize / numberOfThreadsUsed) * numberOfCaches
    }

    val maxWindowCount : Int = offsets.map(_._2.length).sum

    for (windowCount <- 1 to maxWindowCount) {
      val windowSize = cacheSize / windowCount / dataTypeSize
      var empty : ListBuffer[Boolean] = ListBuffer.empty[Boolean]
      var windowsUsed = 0

      fieldAccesses.keys.foreach(ident => {
        var sortedOffsets = offsets(ident).sorted.reverse
        val length = sortedOffsets.length
        var i = 1
        do {
          val maxOffset = sortedOffsets.head
          sortedOffsets = sortedOffsets.drop(1).filter(offset => math.abs(maxOffset - offset) > windowSize)
          windowsUsed += 1
          i = i + 1
        } while (i < length && i <= windowCount && sortedOffsets.nonEmpty)
        empty += sortedOffsets.isEmpty

      })
      if (windowsUsed <= windowCount && !empty.contains(false))
        return windowsUsed * dataTypeSize
    }
    maxWindowCount * dataTypeSize
  }

  def computeRelativeStencilOffsets(stencil : ListBuffer[Long]) : ListBuffer[Long] = {
    val rel_by : ListBuffer[Long] = ListBuffer()
    if (stencil.length < 2) {
      rel_by += stencil.head
      return rel_by
    }
    for (i <- 0 to stencil.length - 2) {
      val tmp = Math.abs(stencil(i) - stencil(i + 1))
      rel_by += tmp
    }
    rel_by.sorted.reverse
  }

  def findBlockingFactor(fieldAcesses : HashMap[String, IR_Datatype], fieldSize : Array[Long], stencilOffsets : HashMap[String, ListBuffer[Long]]) : Array[Long] = {

    //Multi thread:
    var cacheSize : Double = (Platform.hw_cacheSize * Platform.hw_usableCache) / stencilOffsets.size //Bereich fuer jedes Feld
    val numberOfThreadsUsed = Knowledge.omp_numThreads
    val numberOfCaches = Platform.hw_numCacheSharingThreads / Platform.hw_cpu_numCoresPerCPU
    if (numberOfThreadsUsed > numberOfCaches) {
      cacheSize = (cacheSize / numberOfThreadsUsed) * numberOfCaches
    }
    var factors = mutable.HashMap.empty[String, Array[Long]]
    //each field:
    stencilOffsets.keys.foreach(ident => {
      var numberOfSlices = 1
      var stencil = stencilOffsets(ident).sorted.reverse
      if (stencil.nonEmpty) {
        val relO : ListBuffer[Long] = ListBuffer()
        var maxSlice : Long = 0

        //1.Version, auf komplette groesse blocken
        var cacheRequired : Double = math.abs(stencil.head)
        if (stencil(stencil.length - 1) <= 0)
          cacheRequired += math.abs(stencil(stencil.length - 1))
        cacheRequired = cacheRequired * fieldAcesses(ident).typicalByteSize
        //if (cacheRequired == 0){
        //  cacheRequired = cacheSize
        //}
        var ny = cacheSize / cacheRequired
        if (ny >= 1) {
          ny = 1
        }
        //wenn das zu kleine Bloecke werden:
        if (ny > 0.1) {
          var numberOfBlocks : Long = (1 / ny).toLong
          if (1 % ny != 0) {
            numberOfBlocks += 1
          }
          if (fieldSize.length == 3)
            factors(ident) = Array(fieldSize(0), fieldSize(1) / numberOfBlocks, fieldSize(2))
          else
            factors(ident) = Array(fieldSize(0) / numberOfBlocks, fieldSize(1))
          Logger.warn(s"Stencil: ${ stencil.head }, ${ stencil(stencil.length - 1) }")
          Logger.warn(s"Meike: cache_required1 ${ cacheRequired }, numberOfBlocks = ${ numberOfBlocks }, cacheSize = ${ cacheSize }, ny = ${ ny }")
        }
        //ansonsten 2.Version
        else {
          //3D:
          if (fieldSize.length == 3) {
            // 3D slice in +y direction
            val yOffset = fieldSize(0) * fieldSize(1)
            var stencil_biggery = stencil.filter(_ > yOffset)
            if (stencil_biggery.nonEmpty) {
              stencil = stencil.filter(_ < yOffset)
              var i = 1
              do {
                var part = stencil_biggery.filter(_ < i * yOffset)
                if (part.nonEmpty) {
                  stencil_biggery = stencil_biggery.filter(_ > i * yOffset)
                  part = computeRelativeStencilOffsets(part)
                  relO += part.sum
                  numberOfSlices += 1

                }
                i += 1
              } while (stencil_biggery.nonEmpty)

            }
            else {
              relO += 0
            }
            //3D slice in -y direction
            var stencil_smallery = stencil.filter(_ < -yOffset)
            if (stencil_smallery.nonEmpty) {
              stencil = stencil.filter(_ > -yOffset)
              var i = 1
              do {
                var part = stencil_smallery.filter(_ < i * yOffset)
                if (part.nonEmpty) {
                  stencil_smallery = stencil_smallery.filter(_ > i * yOffset)
                  part = computeRelativeStencilOffsets(part)
                  relO += part.sum
                  numberOfSlices += 1

                }
                i += 1
              } while (stencil_smallery.nonEmpty)
            }
            else {
              relO += 0
            }
            maxSlice = relO.max
          }
          //2D
          if (stencil.nonEmpty) {
            val rel_stencil = computeRelativeStencilOffsets(stencil)
            if (rel_stencil.sum > maxSlice) {
              maxSlice = rel_stencil.sum
            }
          }

          cacheRequired = maxSlice * numberOfSlices * fieldAcesses(ident).typicalByteSize
          if (cacheRequired == 0) {
            cacheRequired = cacheSize
          }
          var ny = cacheSize / cacheRequired
          if (ny > 1) {
            ny = 1
          }
          var numberOfBlocks = (1 / ny).round
          if (1 % ny != 0) {
            numberOfBlocks += 1
          }
          Logger.warn(s"Meike: cache_required2 ${ cacheRequired }, numberOfBlocks = ${ numberOfBlocks }, cacheSize = ${ cacheSize }")
          //numberOfBlocks = 1 / numberOfBlocks
          if (fieldSize.length == 3) {
            factors(ident) = Array(fieldSize(0), fieldSize(1) / numberOfBlocks, fieldSize(2))

          }
          else {
            factors(ident) = Array(fieldSize(0) / numberOfBlocks, fieldSize(1))
          }
        }
      }
      else {
        factors(ident) = Array(fieldSize(0), fieldSize(1), fieldSize(2))
      }

    })
    //minimum finden
    factors(factors.minBy(value => value._2(1))._1)

  }
  override def applyStandalone(node : Node) : Unit = {
    unknownFunctionCalls = false
    estimatedTimeSubAST.push(PerformanceEstimate(0.0, 0.0))
    super.applyStandalone(node)
    lastEstimate = estimatedTimeSubAST.pop
  }

  this += new Transformation("Progressing key statements", {
    // function calls
    case fct : IR_FunctionCall =>

      if (!CollectFunctionStatements.internalFunctions.contains(fct.name))
        () // external functions -> no estimate
      else if (EvaluatePerformanceEstimates.completeFunctions.contains(fct.name))
        addTimeToStack(EvaluatePerformanceEstimates.completeFunctions(fct.name))
      else
        unknownFunctionCalls = true
      fct

    case loop : IR_LoopOverDimensions =>

      if (loop.hasAnnotation("perf_timeEstimate_host") || loop.hasAnnotation("perf_timeEstimate_device")) {
        addTimeToStack(loop)
        loop
      } else {
        var maxIterations = loop.maxIterationCount().product

        EvaluatePerformanceEstimates_FieldAccess.fieldAccesses.clear // has to be done manually
        EvaluatePerformanceEstimates_FieldAccess.offsets.clear
        EvaluatePerformanceEstimates_FieldAccess.applyStandalone(loop)
        EvaluatePerformanceEstimates_Ops.applyStandalone(loop)

        val coresPerRank = (Platform.hw_numNodes * Platform.hw_numHWThreadsPerNode).toDouble / Knowledge.mpi_numThreads // could be fractions of cores; regard SMT

        val optimisticDataPerIt = dataPerIteration(EvaluatePerformanceEstimates_FieldAccess.fieldAccesses, EvaluatePerformanceEstimates_FieldAccess.offsets)
        val effectiveHostBW = Platform.hw_cpu_bandwidth / (coresPerRank * Knowledge.omp_numThreads) // assumes full parallelization - TODO: adapt values according to (OMP) parallel loops
        val optimisticTimeMem_host = (optimisticDataPerIt * maxIterations) / Platform.hw_cpu_bandwidth
        val optimisticTimeMem_device = (optimisticDataPerIt * maxIterations) / Platform.hw_gpu_bandwidth

        val cyclesPerIt = (Math.max(EvaluatePerformanceEstimates_Ops.numAdd, EvaluatePerformanceEstimates_Ops.numMul)
          + EvaluatePerformanceEstimates_Ops.numDiv * Platform.hw_cpu_numCyclesPerDiv)
        var estimatedTimeOps_host = (cyclesPerIt * maxIterations) / Platform.hw_cpu_frequency
        var estimatedTimeOps_device = (cyclesPerIt * maxIterations) / Platform.hw_gpu_frequency
        estimatedTimeOps_host /= Math.min(coresPerRank, Knowledge.omp_numThreads) // adapt for omp threading and hardware utilization
        estimatedTimeOps_host /= Platform.simd_vectorSize // adapt for vectorization - assume perfect vectorizability
        estimatedTimeOps_device /= Math.min(maxIterations, Platform.hw_gpu_numCores) // assumes perfect utilization as far as possible

        val totalEstimate = PerformanceEstimate(Math.max(estimatedTimeOps_host, optimisticTimeMem_host), Math.max(estimatedTimeOps_device, optimisticTimeMem_device))
        totalEstimate.device += Platform.sw_cuda_kernelCallOverhead

        var dim : Array[Long] = Array(0, 0, 0)
        if (Knowledge.opt_loopBlocked) {
          dim = findBlockingFactor(EvaluatePerformanceEstimates_FieldAccess.fieldAccesses, loop.maxIterationCount(), EvaluatePerformanceEstimates_FieldAccess.offsets)
          Logger.warn(s"Meike2: dims = ${ dim(0) }, ${ dim(1) }")
          loop.tileSize = dim.map(_.toInt)
          maxIterations = dim.product
          //IR_Comment(s"min loop dimentsion: ${dim(0)} ,${ dim(1)}, elements"),
        }
        loop.annotate("perf_timeEstimate_host", totalEstimate.host)
        loop.annotate("perf_timeEstimate_device", totalEstimate.device)
        addTimeToStack(totalEstimate)

        ListBuffer(
          IR_Comment(s"Max iterations: $maxIterations"),
          IR_Comment(s"Optimistic memory transfer per iteration: $optimisticDataPerIt byte"),
          IR_Comment(s"Optimistic host time for memory ops: ${ optimisticTimeMem_host * 1000.0 } ms"),
          IR_Comment(s"Optimistic device time for memory ops: ${ optimisticTimeMem_device * 1000.0 } ms"),
          IR_Comment(s"Optimistic host time for computational ops: ${ estimatedTimeOps_host * 1000.0 } ms"),
          IR_Comment(s"Optimistic device time for computational ops: ${ estimatedTimeOps_device * 1000.0 } ms"),
          IR_Comment(s"Assumed kernel call overhead: ${ Platform.sw_cuda_kernelCallOverhead * 1000.0 } ms"),
          IR_Comment(s"Found accesses: ${ EvaluatePerformanceEstimates_FieldAccess.fieldAccesses.keys.mkString(", ") }"),
          IR_Comment(s"min loop dimentsion: ${ dim(0) } ,${ dim(1) }, elements"),
          loop)
      }

    case loop : IR_ForLoop =>

      if (loop.hasAnnotation("perf_timeEstimate_host") || loop.hasAnnotation("perf_timeEstimate_device")) {
        addLoopTimeToStack(loop)
      } else {
        applyStandalone(loop.body)

        if (unknownFunctionCalls) {
          unknownFunctionCalls = true
        } else {
          val estimatedTime_host = lastEstimate.host * loop.maxIterationCount
          val estimatedTime_device = lastEstimate.device * loop.maxIterationCount

          loop.annotate("perf_timeEstimate_host", estimatedTime_host)
          loop.annotate("perf_timeEstimate_device", estimatedTime_device)

          addLoopTimeToStack(loop)

          loop.body = ListBuffer[IR_Statement](
            IR_Comment(s"Estimated host time for loop: ${ estimatedTime_host * 1000.0 } ms"),
            IR_Comment(s"Estimated device time for loop: ${ estimatedTime_device * 1000.0 } ms")) ++ loop.body
        }
      }
      loop
  }, false)
}

object EvaluatePerformanceEstimates_FieldAccess extends QuietDefaultStrategy("Evaluating performance for FieldAccess nodes") {
  var fieldAccesses = HashMap[String, IR_Datatype]()
  var offsets = HashMap[String, ListBuffer[Long]]()
  var multiDimOffsets = HashMap[String, ListBuffer[IR_ConstIndex]]()
  var inWriteOp = true

  def mapFieldAccess(access : IR_MultiDimFieldAccess) = {
    val field = access.field
    var identifier = field.codeName

    identifier = (if (inWriteOp) "write_" else "read_") + identifier

    if (field.numSlots > 1) {
      access.slot match {
        case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case _                        => identifier += s"_s${ access.slot.prettyprint }"
      }
    }

    fieldAccesses.put(identifier, field.gridDatatype) // TODO: optimize for array fields / HODT

    // evaluate and store offset
    val offsetIndex = Duplicate(access.index)
    IR_ReplaceVariableAccess.replace = (0 until access.index.length).map(d => (IR_LoopOverDimensions.defItForDim(d).name, IR_IntegerConstant(0))).toMap
    IR_ReplaceVariableAccess.applyStandalone(offsetIndex)

    var offset = 0L
    var mdOffset = IR_ConstIndex()
    try {
      offset = IR_SimplifyExpression.evalIntegral(
        IR_Linearization.linearizeIndex(offsetIndex,
          IR_ExpressionIndex((0 until access.index.length).map(field.layout(_).total).toArray)))
      mdOffset = offsetIndex.toConstIndex
    } catch {
      case _ : EvaluationException => Logger.warn("Could not evaluate offset for " + offsetIndex.prettyprint())
    }

    offsets.put(identifier, offsets.getOrElse(identifier, ListBuffer()) :+ offset)
    multiDimOffsets.put(identifier, multiDimOffsets.getOrElse(identifier, ListBuffer()) :+ mdOffset)
  }

  this += new Transformation("Searching", {
    case assign : IR_Assignment          =>
      inWriteOp = true
      EvaluatePerformanceEstimates_FieldAccess.applyStandalone(IR_ExpressionStatement(assign.dest))
      inWriteOp = false
      // honor read-allocate
      EvaluatePerformanceEstimates_FieldAccess.applyStandalone(IR_ExpressionStatement(assign.dest))
      EvaluatePerformanceEstimates_FieldAccess.applyStandalone(IR_ExpressionStatement(assign.src))
      assign
    case access : IR_MultiDimFieldAccess =>
      mapFieldAccess(access)
      access
  }, false)
}

object EvaluatePerformanceEstimates_Ops extends QuietDefaultStrategy("Evaluating performance for numeric operations") {
  var numAdd = 0
  var numMul = 0
  var numDiv = 0

  override def applyStandalone(node : Node) : Unit = {
    numAdd = 0
    numMul = 0
    numDiv = 0
    super.applyStandalone(node)
  }

  // FIXME: incorporate number of operands
  this += new Transformation("Searching", {
    case exp : IR_Addition       =>
      numAdd += 1
      exp
    case exp : IR_Subtraction    =>
      numAdd += 1
      exp
    case exp : IR_Multiplication =>
      numMul += 1
      exp
    case exp : IR_Division       =>
      exp.right match {
        case _ : IR_IntegerConstant => numMul += 0
        case _ : IR_RealConstant    => numMul += 1
        case _                      => numDiv += 1
      }
      exp
  })
}