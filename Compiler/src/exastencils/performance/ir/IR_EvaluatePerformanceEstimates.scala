//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.performance.ir

import scala.collection.mutable._

import java.io.PrintWriter

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.performance.PlatformUtils
import exastencils.util.ir._

/// IR_EvaluatePerformanceEstimates

object IR_EvaluatePerformanceEstimates extends DefaultStrategy("Evaluating performance estimates") with ObjectWithState {
  var completeFunctions = HashMap[String, IR_PerformanceEstimate]()
  var unknownFunctionCalls = true

  override def clear() = {
    completeFunctions.clear()
    unknownFunctionCalls = true
  }

  override def apply(node : Option[Node] = None) : Unit = Logger.error("Unsupported")

  def doUntilDone(node : Option[Node] = None) : Unit = {
    var cnt = 0
    unknownFunctionCalls = true
    while (unknownFunctionCalls && cnt < 128) {
      unknownFunctionCalls = false
      super.apply(node)
      cnt += 1
    }

    if (Knowledge.performance_printEstimation)
      printEstimationToFile()
  }

  def printEstimationToFile() = {
    val targetFile = Settings.performanceEstimateOutputFile
    if (!new java.io.File(targetFile).exists) {
      val file = new java.io.File(targetFile)
      if (!file.getParentFile.exists()) file.getParentFile.mkdirs()
    }

    val outputStream = new PrintWriter(targetFile)

    for (fct <- completeFunctions.toList.sortBy(_._1)) {
      val fctName = fct._1
      val estimate = fct._2
      outputStream.println("%s%s%s%s%e".formatLocal(java.util.Locale.US,
        "function", Settings.csvSeparator, fctName, Settings.csvSeparator, estimate.host * 1000.0)) // ms
    }

    outputStream.close()
  }

  this += new Transformation("Processing function statements", {
    case fct : IR_Function if !completeFunctions.contains(fct.name) && IR_CollectFunctionStatements.internalFunctions.contains(fct.name) =>
      // process function body
      EvaluateSubAST.applyStandalone(fct.body)

      if (EvaluateSubAST.unknownFunctionCalls) {
        unknownFunctionCalls = true
      } else {
        val estimatedTime = EvaluateSubAST.lastEstimate

        fct.annotate("perf_timeEstimate_host", estimatedTime.host)
        fct.annotate("perf_timeEstimate_device", estimatedTime.device)

        completeFunctions.put(fct.name, estimatedTime)
        fct.body.prepend(
          IR_Comment(s"Estimated host time for function: ${ estimatedTime.host * 1000.0 } ms"),
          IR_Comment(s"Estimated device time for function: ${ estimatedTime.device * 1000.0 } ms"))
      }
      fct
  })

  // encapsulated strategies

  object EvaluateSubAST extends QuietDefaultStrategy("Estimating performance for sub-ASTs") {
    // TODO:  loops with conditions, reductions
    //        while loops
    //        condition statements

    var unknownFunctionCalls = false
    var estimatedTimeSubAST = Stack[IR_PerformanceEstimate]()
    var lastEstimate = IR_PerformanceEstimate(0.0, 0.0)

    override def applyStandalone(node : Node) : Unit = {
      unknownFunctionCalls = false
      estimatedTimeSubAST.push(IR_PerformanceEstimate(0.0, 0.0))
      super.applyStandalone(node)
      lastEstimate = estimatedTimeSubAST.pop
    }

    def addTimeToStack(estimate : IR_PerformanceEstimate) : Unit = {
      estimatedTimeSubAST.push(estimatedTimeSubAST.pop + estimate)
    }
    def addTimeToStack(nodeWithAnnotation : Node) : Unit = {
      addTimeToStack(IR_PerformanceEstimate(
        nodeWithAnnotation.getAnnotation("perf_timeEstimate_host").get.asInstanceOf[Double],
        nodeWithAnnotation.getAnnotation("perf_timeEstimate_device").get.asInstanceOf[Double]))
    }

    def addLoopTimeToStack(loop : IR_ForLoop) : Unit = {
      addTimeToStack(loop)
    }

    def dataPerIteration(fieldAccesses : HashMap[String, IR_Datatype], offsets : HashMap[String, ListBuffer[Long]]) : Int = {
      if (fieldAccesses.values.map(_.typicalByteSize).toList.distinct.length > 1)
        Logger.error("Unsupported: Not the same Datatype")

      val dataTypeSize = fieldAccesses.values.head.typicalByteSize

      // assume perfect blocking if opt_loopBlocked is activated ...
      if (Knowledge.opt_loopBlocked)
        return dataTypeSize * fieldAccesses.keys.size

      // ... otherwise determine the number of data that needs to be loaded/ stored taking cache sizes into account
      val effCacheSize = Platform.hw_usableCache * PlatformUtils.cacheSizePerThread
      val maxWindowCount : Int = offsets.map(_._2.length).sum

      for (windowCount <- 1 to maxWindowCount) {
        val windowSize = effCacheSize / windowCount / dataTypeSize
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
            i += 1
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

    this += new Transformation("Progressing key statements", {
      case fct : IR_FunctionCall =>

        if (!IR_CollectFunctionStatements.internalFunctions.contains(fct.name))
          () // external functions -> no estimate
        else if (IR_EvaluatePerformanceEstimates.completeFunctions.contains(fct.name))
          addTimeToStack(IR_EvaluatePerformanceEstimates.completeFunctions(fct.name))
        else
          unknownFunctionCalls = true
        fct

      case loop : IR_LoopOverDimensions =>

        if (loop.hasAnnotation("perf_timeEstimate_host") || loop.hasAnnotation("perf_timeEstimate_device")) {
          addTimeToStack(loop)
          loop
        } else {
          val stmts = ListBuffer[IR_Statement]()

          var maxIterations = loop.maxIterationCount().product
          var iterationsPerThread = maxIterations
          if (Knowledge.omp_parallelizeLoopOverDimensions)
            iterationsPerThread /= Knowledge.omp_numThreads
          val iterationsPerMpi = maxIterations * Knowledge.domain_numFragmentsPerBlock

          stmts += IR_Comment(s"Max iterations: $maxIterations")
          stmts += IR_Comment(s"Iterations per thread: $iterationsPerThread")

          EvaluateFieldAccess.reset()
          EvaluateFieldAccess.applyStandalone(loop)
          EvaluateForOps.applyStandalone(loop)

          stmts += IR_Comment(s"Accesses: ${ EvaluateFieldAccess.fieldAccesses.keys.mkString(", ") }")

          // memory transfer
          val dataPerIt = dataPerIteration(EvaluateFieldAccess.fieldAccesses, EvaluateFieldAccess.offsets)

          val optimisticTimeMem_host = dataPerIt * iterationsPerThread / PlatformUtils.cpu_bandwidthPerThread
          val optimisticTimeMem_device = dataPerIt * iterationsPerMpi / PlatformUtils.gpu_bandwidthPerMpi

          stmts += IR_Comment(s"Memory transfer per iteration: $dataPerIt byte")
          stmts += IR_Comment(s"Host time for memory ops: ${ optimisticTimeMem_host * 1000.0 } ms")
          stmts += IR_Comment(s"Device time for memory ops: ${ optimisticTimeMem_device * 1000.0 } ms")

          // computational operations
          val cyclesPerIt = Math.max(EvaluateForOps.numAdd, EvaluateForOps.numMul) + EvaluateForOps.numDiv * Platform.hw_cpu_numCyclesPerDiv

          val estimatedTimeOps_host = cyclesPerIt * iterationsPerThread / PlatformUtils.cpu_opsPerThread
          val estimatedTimeOps_device = cyclesPerIt * iterationsPerMpi / PlatformUtils.gpu_opsPerMpi(maxIterations)

          stmts += IR_Comment(s"Host time for computational ops: ${ estimatedTimeOps_host * 1000.0 } ms")
          stmts += IR_Comment(s"Device time for computational ops: ${ estimatedTimeOps_device * 1000.0 } ms")

          // roofline
          val totalEstimate = IR_PerformanceEstimate(Math.max(estimatedTimeOps_host, optimisticTimeMem_host), Math.max(estimatedTimeOps_device, optimisticTimeMem_device))
          totalEstimate.device += Platform.sw_cuda_kernelCallOverhead

          stmts += IR_Comment(s"Assumed kernel call overhead: ${ Platform.sw_cuda_kernelCallOverhead * 1000.0 } ms")

          loop.annotate("perf_timeEstimate_host", totalEstimate.host)
          loop.annotate("perf_timeEstimate_device", totalEstimate.device)
          addTimeToStack(totalEstimate)

          // use information gathered so far to add blocking if enabled
          if (Knowledge.opt_loopBlocked) {
            val blockingFactors = IR_DetermineBlockingFactors(EvaluateFieldAccess.fieldAccesses, EvaluateFieldAccess.offsets, loop.maxIterationCount())
            loop.tileSize = blockingFactors.map(_.toInt)

            stmts += IR_Comment(s"Loop blocking factors: ${ blockingFactors.mkString(", ") }")
          }

          stmts :+ loop
        }

      case loop : IR_ForLoop =>

        if (loop.hasAnnotation("perf_timeEstimate_host") || loop.hasAnnotation("perf_timeEstimate_device")) {
          addLoopTimeToStack(loop)
        } else {
          applyStandalone(loop.body)

          if (unknownFunctionCalls) {
            unknownFunctionCalls = true
          } else {
            var parallelFactor = loop.maxIterationCount()
            if (loop.parallelization.potentiallyParallel && Knowledge.omp_parallelizeLoopOverFragments)
              parallelFactor /= Knowledge.omp_numThreads

            val estimatedTime_host = lastEstimate.host * parallelFactor
            val estimatedTime_device = lastEstimate.device * parallelFactor

            loop.annotate("perf_timeEstimate_host", estimatedTime_host)
            loop.annotate("perf_timeEstimate_device", estimatedTime_device)

            addLoopTimeToStack(loop)

            loop.body = ListBuffer[IR_Statement](
              IR_Comment(s"Estimated host time for loop: ${ estimatedTime_host * 1000.0 } ms"),
              IR_Comment(s"Estimated device time for loop: ${ estimatedTime_device * 1000.0 } ms")
            ) ++ loop.body
          }
        }

        loop
    }, false)
  }

  object EvaluateFieldAccess extends QuietDefaultStrategy("Evaluating performance for FieldAccess nodes") {
    var fieldAccesses = HashMap[String, IR_Datatype]()
    var offsets = HashMap[String, ListBuffer[Long]]()
    var multiDimOffsets = HashMap[String, ListBuffer[IR_ConstIndex]]()
    var inWriteOp = true

    override def reset() = {
      fieldAccesses.clear
      offsets.clear
      multiDimOffsets.clear

      super.reset()
    }

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

      // honor component accesses for HODT
      // TODO: more cases?
      val accessedDt = access match {
        case fAcc : IR_FieldAccess       => // determine dt dependent if component access or not
          if (fAcc.matIndex.isDefined) {
            field.resolveBaseDatatype
          } else {
            field.gridDatatype
          }
        case fAcc : IR_DirectFieldAccess => // handled in IR_DirectFieldAccess depending on length of access index
          fAcc.datatype
        case acc : IR_Access             =>
          Logger.error("EvaluateFieldAccess: Match error. Did not expect access: " + acc.name + " of type: " + acc.getClass)
      }

      fieldAccesses.put(identifier, accessedDt)

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
        EvaluateFieldAccess.applyStandalone(IR_ExpressionStatement(assign.dest))
        inWriteOp = false
        // honor read-allocate
        EvaluateFieldAccess.applyStandalone(IR_ExpressionStatement(assign.dest))
        EvaluateFieldAccess.applyStandalone(IR_ExpressionStatement(assign.src))
        assign
      case access : IR_MultiDimFieldAccess =>
        mapFieldAccess(access)
        access
    }, false)
  }

  object EvaluateForOps extends QuietDefaultStrategy("Evaluating performance for numeric operations") {
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

}
