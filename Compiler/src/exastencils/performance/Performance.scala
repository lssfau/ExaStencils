package exastencils.performance

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

import exastencils.data._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge._

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

object CollectFunctionStatements extends DefaultStrategy("Collecting internal function statements") {
  var internalFunctions = HashSet[String]()

  override def apply(node : Option[Node] = None) : Unit = {
    internalFunctions.clear
    super.apply(node)
  }

  this += new Transformation("Collecting", {
    case fct : FunctionStatement => {
      internalFunctions += fct.name
      fct
    }
  }, false)
}

object EvaluatePerformanceEstimates extends DefaultStrategy("Evaluating performance estimates") {
  var completeFunctions = HashMap[String, PerformanceEstimate]()
  var unknownFunctionCalls = true

  override def apply(node : Option[Node] = None) : Unit = {
    unknownFunctionCalls = false
    super.apply(node)
  }

  def doUntilDone(node : Option[Node] = None) = {
    var cnt = 0
    unknownFunctionCalls = true
    while (unknownFunctionCalls && cnt < 128) {
      unknownFunctionCalls = false
      super.apply(node)
      cnt += 1
    }
  }

  this += new Transformation("Processing function statements", {
    case fct : FunctionStatement if !completeFunctions.contains(fct.name) && CollectFunctionStatements.internalFunctions.contains(fct.name) => {
      // process function body
      EvaluatePerformanceEstimates_SubAST.applyStandalone(fct.body)

      if (EvaluatePerformanceEstimates_SubAST.unknownFunctionCalls) {
        unknownFunctionCalls = true
      } else {
        val estimatedTime = EvaluatePerformanceEstimates_SubAST.lastEstimate

        fct.annotate("perf_timeEstimate_host", estimatedTime.host)
        fct.annotate("perf_timeEstimate_device", estimatedTime.device)

        completeFunctions.put(fct.name, estimatedTime)
        fct.body = ListBuffer[Statement](
          CommentStatement(s"Estimated host time for function: ${estimatedTime.host * 1000.0} ms"),
          CommentStatement(s"Estimated device time for function: ${estimatedTime.device * 1000.0} ms")) ++ fct.body
      }

      fct
    }
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

  def addLoopTimeToStack(loop : ForLoopStatement) : Unit = {
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

  override def applyStandalone(node : Node) : Unit = {
    unknownFunctionCalls = false
    estimatedTimeSubAST.push(PerformanceEstimate(0.0, 0.0))
    super.applyStandalone(node)
    lastEstimate = estimatedTimeSubAST.pop
  }

  this += new Transformation("Progressing key statements", {
    // function calls
    case fct : FunctionCallExpression => {
      if (!CollectFunctionStatements.internalFunctions.contains(fct.name))
        () // external functions -> no estimate
      else if (EvaluatePerformanceEstimates.completeFunctions.contains(fct.name))
        addTimeToStack(EvaluatePerformanceEstimates.completeFunctions.get(fct.name).get)
      else
        unknownFunctionCalls = true
      fct
    }

    case loop : LoopOverDimensions => {
      if (loop.hasAnnotation("perf_timeEstimate_host") || loop.hasAnnotation("perf_timeEstimate_device")) {
        addTimeToStack(loop)
        loop
      } else {
        val maxIterations = loop.maxIterationCount.reduce(_ * _)

        EvaluatePerformanceEstimates_FieldAccess.fieldAccesses.clear // has to be done manually
        EvaluatePerformanceEstimates_FieldAccess.applyStandalone(loop)
        EvaluatePerformanceEstimates_Ops.applyStandalone(loop)

        val coresPerRank = (Platform.hw_numNodes * Platform.hw_numHWThreadsPerNode).toDouble / Knowledge.mpi_numThreads // could be fractions of cores; regard SMT

        val optimisticDataPerIt = EvaluatePerformanceEstimates_FieldAccess.fieldAccesses.map(_._2.typicalByteSize).fold(0)(_ + _)
        val effectiveHostBW = Platform.hw_cpu_bandwidth / (coresPerRank * Knowledge.omp_numThreads) // assumes full parallelization - TODO: adapt values according to (OMP) parallel loops
        val optimisticTimeMem_host = (optimisticDataPerIt * maxIterations) / effectiveHostBW
        val optimisticTimeMem_device = (optimisticDataPerIt * maxIterations) / Platform.hw_gpu_bandwidth

        val cyclesPerIt = (Math.max(EvaluatePerformanceEstimates_Ops.numAdd, EvaluatePerformanceEstimates_Ops.numMul)
          + EvaluatePerformanceEstimates_Ops.numDiv * Platform.hw_cpu_numCyclesPerDiv)
        var estimatedTimeOps_host = (cyclesPerIt * maxIterations) / Platform.hw_cpu_frequency
        var estimatedTimeOps_device = (cyclesPerIt * maxIterations) / Platform.hw_gpu_frequency
        estimatedTimeOps_host /= Math.min(coresPerRank, Knowledge.omp_numThreads) // adapt for omp threading and hardware utilization
        estimatedTimeOps_host /= Platform.simd_vectorSize // adapt for vectorization - assume perfect vectorizability
        estimatedTimeOps_device /= Platform.hw_gpu_numCores // assumes perfect utilization - TODO: annotate max number of iterations in loop and use it here if smaller than number of cuda cores

        var totalEstimate = PerformanceEstimate(Math.max(estimatedTimeOps_host, optimisticTimeMem_host), Math.max(estimatedTimeOps_device, optimisticTimeMem_device))
        totalEstimate.device += Platform.sw_cuda_kernelCallOverhead

        loop.annotate("perf_timeEstimate_host", totalEstimate.host)
        loop.annotate("perf_timeEstimate_device", totalEstimate.device)
        addTimeToStack(totalEstimate)

        ListBuffer(
          CommentStatement(s"Max iterations: $maxIterations"),
          CommentStatement(s"Optimistic memory transfer per iteration: $optimisticDataPerIt byte"),
          CommentStatement(s"Optimistic host time for memory ops: ${optimisticTimeMem_host * 1000.0} ms"),
          CommentStatement(s"Optimistic device time for memory ops: ${optimisticTimeMem_device * 1000.0} ms"),
          CommentStatement(s"Optimistic host time for computational ops: ${estimatedTimeOps_host * 1000.0} ms"),
          CommentStatement(s"Optimistic device time for computational ops: ${estimatedTimeOps_device * 1000.0} ms"),
          CommentStatement(s"Assumed kernel call overhead: ${Platform.sw_cuda_kernelCallOverhead * 1000.0} ms"),
          CommentStatement(s"Found accesses: ${EvaluatePerformanceEstimates_FieldAccess.fieldAccesses.map(_._1).mkString(", ")}"),
          loop)
      }
    }

    case loop : ForLoopStatement => {
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

          loop.body = ListBuffer[Statement](
            CommentStatement(s"Estimated host time for loop: ${estimatedTime_host * 1000.0} ms"),
            CommentStatement(s"Estimated device time for loop: ${estimatedTime_device * 1000.0} ms")) ++ loop.body
        }
      }
      loop
    }
  }, false)
}

object EvaluatePerformanceEstimates_FieldAccess extends QuietDefaultStrategy("Evaluating performance for FieldAccess nodes") {
  var fieldAccesses = HashMap[String, Datatype]()
  var inWriteOp = false

  def mapFieldAccess(access : FieldAccessLike) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

    identifier = (if (inWriteOp) "write_" else "read_") + identifier

    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case SlotAccess(_, offset) => identifier += s"_o$offset"
        case IntegerConstant(slot) => identifier += s"_s$slot"
        case _                     => identifier += s"_s${access.fieldSelection.slot.prettyprint}"
      }
    }

    fieldAccesses.put(identifier, field.gridDatatype) // TODO: optimize for array fields / HODT
  }

  this += new Transformation("Searching", {
    case assign : AssignmentStatement =>
      inWriteOp = true
      EvaluatePerformanceEstimates_FieldAccess.applyStandalone(ExpressionStatement(assign.dest))
      inWriteOp = false
      EvaluatePerformanceEstimates_FieldAccess.applyStandalone(ExpressionStatement(assign.src))
      assign
    case access : FieldAccessLike =>
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

  this += new Transformation("Searching", {
    case exp : AdditionExpression =>
      numAdd += 1
      exp
    case exp : SubtractionExpression =>
      numAdd += 1
      exp
    case exp : MultiplicationExpression =>
      numMul += 1
      exp
    case exp : DivisionExpression =>
      if (exp.right.isInstanceOf[IntegerConstant])
        numMul += 0 // ignore integer divs for now
      else if (exp.right.isInstanceOf[FloatConstant]) // TODO: replace with eval float exp?
        numMul += 1
      else
        numDiv += 1
      exp
  })
}
