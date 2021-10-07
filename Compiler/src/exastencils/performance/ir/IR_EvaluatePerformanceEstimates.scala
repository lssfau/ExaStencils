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
import exastencils.baseExt.ir.IR_MatNodes.IR_GetElement
import exastencils.baseExt.ir.IR_MatNodes.IR_GetSliceCT
import exastencils.baseExt.ir.IR_MatNodes.IR_RuntimeMNode
import exastencils.baseExt.ir.IR_MatNodes.IR_SetElement
import exastencils.baseExt.ir.IR_MatNodes.IR_SetSlice
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.EvaluationException
import exastencils.optimization.ir.IR_SimplifyExpression
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

    // cache access pattern for consecutive vector/matrix component accesses
    var accessedMatrixEntries = HashMap[String, HashSet[(Long, Long)]]()

    override def reset() = {
      fieldAccesses.clear
      offsets.clear
      multiDimOffsets.clear
      accessedMatrixEntries.clear

      super.reset()
    }

    def mapIndexToOffsets(field : IR_Field, identifier : String, offsetIndex : IR_ExpressionIndex, len : Int) = {
      // replace loop variables with 0
      IR_ReplaceVariableAccess.replace = (0 until len).map(d => (IR_LoopOverDimensions.defItForDim(d).name, IR_IntegerConstant(0))).toMap
      IR_ReplaceVariableAccess.applyStandalone(offsetIndex)

      // evaluate offsets and store
      var offset = 0L
      var mdOffset = IR_ConstIndex()
      try {
        offset = IR_SimplifyExpression.evalIntegral(
          IR_Linearization.linearizeIndex(offsetIndex,
            IR_ExpressionIndex((0 until len).map(field.layout(_).total).toArray)))
        mdOffset = offsetIndex.toConstIndex
      } catch {
        case _ : EvaluationException => Logger.warn("Could not evaluate offset for " + offsetIndex.prettyprint())
      }

      offsets.put(identifier, offsets.getOrElse(identifier, ListBuffer()) :+ offset)
      multiDimOffsets.put(identifier, multiDimOffsets.getOrElse(identifier, ListBuffer()) :+ mdOffset)
    }

    def getIdentifier(field : IR_Field, slot : IR_Expression, isWrite : Boolean = inWriteOp) = {
      var identifier = field.codeName

      identifier = (if (isWrite) "write_" else "read_") + identifier

      if (field.numSlots > 1) {
        slot match {
          case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
          case IR_IntegerConstant(slot) => identifier += s"_s$slot"
          case _                        => identifier += s"_s${ slot.prettyprint }"
        }
      }

      identifier
    }

    def mapFieldAccess(access : IR_MultiDimFieldAccess) = {
      val field = access.field
      val identifier = getIdentifier(field, access.slot)

      fieldAccesses.put(identifier, access match {
        case fAcc : IR_FieldAccess       =>
          // determine dt dependent if component access or not
          if (fAcc.matIndex.isDefined) {
            val matIdx = fAcc.matIndex.get

            // get number of elements accessed with matIndex
            def getActualRange(left : Option[IR_Expression], defaultLeft : Int, right : Option[IR_Expression], defaultRight : Int) : (IR_Expression, IR_Expression) =
              (left.getOrElse(IR_IntegerConstant(defaultLeft)), right.getOrElse(IR_IntegerConstant(defaultRight)))
            def evalSliceSize(range : (IR_Expression, IR_Expression)) : Int =
              IR_SimplifyExpression.evalIntegral(range._2 + IR_IntegerConstant(1) - range._1).toInt

            val numRowsAccessed = matIdx.y match {
              // index range, e.g. [start:end]
              case _ @ IR_RangeIndex(range) if fAcc.field.gridDatatype.isInstanceOf[IR_MatrixDatatype] =>
                getActualRange(range(0).begin, 0, range(0).end, fAcc.field.gridDatatype.asInstanceOf[IR_MatrixDatatype].sizeM)
              // index, e.g. [0]
              case idx : IR_Index if idx.length == 1 =>
                val index = idx.toExpressionIndex.head
                (index, index)
            }

            val numColsAccessed = if (matIdx.x.isDefined) {
              matIdx.x.get match {
                case _ @ IR_RangeIndex(range) if fAcc.field.gridDatatype.isInstanceOf[IR_MatrixDatatype] =>
                  getActualRange(range(0).begin, 0, range(0).end, fAcc.field.gridDatatype.asInstanceOf[IR_MatrixDatatype].sizeN)
                case idx : IR_Index if idx.length == 1                                                   =>
                  val index = idx.toExpressionIndex.head
                  (index, index)
              }
            } else {
              (IR_IntegerConstant(0), IR_IntegerConstant(0)) // no idx provided -> assumed to be vector component access
            }

            field.gridDatatype match {
              case _ : IR_VectorDatatype | _ : IR_MatrixDatatype =>
                val newAccesses = (0 until evalSliceSize(numRowsAccessed)).flatMap(r => {
                  val row = IR_SimplifyExpression.evalIntegral(numRowsAccessed._1) + r

                  (0 until evalSliceSize(numColsAccessed)).map(c => {
                    val col = IR_SimplifyExpression.evalIntegral(numColsAccessed._1) + c

                    (row, col)
                  })
                })

                accessedMatrixEntries.put(identifier, accessedMatrixEntries.getOrElse(identifier, HashSet.empty) ++= newAccesses.toSet)
            }

            IR_MatrixDatatype(field.resolveBaseDatatype, evalSliceSize(numRowsAccessed), evalSliceSize(numColsAccessed))
          } else {
           field.gridDatatype
          }
        case fAcc : IR_DirectFieldAccess => // handled in IR_DirectFieldAccess depending on length of access index
          fAcc.datatype
        case acc : IR_Access             =>
          Logger.error("EvaluateFieldAccess: Match error. Did not expect access: " + acc.prettyprint())
      })

      mapIndexToOffsets(field, identifier, Duplicate(access.index), access.index.length)
    }

    // matIndex not set for matrix functions -> need separate handling
    def mapCompiletimeMatrixFunctions(matFunc : IR_Expression) {
      matFunc match {
        // get functions: only read access
        case getElement @ IR_GetElement(ListBuffer(inMat : IR_MultiDimFieldAccess, idxy, idxx)) =>
          val field = inMat.field
          val identifier = getIdentifier(field, inMat.slot)

          // TODO use idxy and idxx for caching

          // determine dt for field access in getSlice
          fieldAccesses.put(identifier, getElement.datatype)

          mapIndexToOffsets(field, identifier, Duplicate(inMat.index), inMat.index.length)
        case getSlice @ IR_GetSliceCT(inMat : IR_MultiDimFieldAccess, _) =>
          val field = inMat.field
          val identifier = getIdentifier(field, inMat.slot)

          // TODO caching with offsets and strides

          // determine dt for field access in getSlice
          fieldAccesses.put(identifier, getSlice.datatype)

          mapIndexToOffsets(field, identifier, Duplicate(inMat.index), inMat.index.length)

        /*
        // TODO move

        // set functions: read and write access
        case setElement @ IR_SetElement(ListBuffer(lhs : IR_MultiDimFieldAccess, idxy, idxx, rhs : IR_MultiDimFieldAccess)) =>
          val lhsField = lhs.field
          val lhsWriteIdentifier = getIdentifier(lhsField, lhs.slot, isWrite = true)
          val lhsReadIdentifier = getIdentifier(lhsField, lhs.slot, isWrite = false)
          val dt = setElement.datatype

          // TODO use idxy and idxx for caching

          if (IR_MatNodeUtils.isScalar(idxy) && IR_MatNodeUtils.isScalar(idxx)) {
            // write and read allocate for lhs
            fieldAccesses.put(lhsWriteIdentifier, dt)
            fieldAccesses.put(lhsReadIdentifier, dt)

            mapIndexToOffsets(lhsField, lhsWriteIdentifier, Duplicate(lhs.index), lhs.index.length)
            mapIndexToOffsets(lhsField, lhsReadIdentifier, Duplicate(lhs.index), lhs.index.length)

            // read rhs
            inWriteOp = false
            EvaluateFieldAccess.applyStandalone(IR_ExpressionStatement(rhs))
          } else {
            Logger.warn("Cannot determine field accesses for setSlice with runtime values.")
          }

        case IR_SetSlice(ListBuffer(matrix : IR_MultiDimFieldAccess, rowOffs, colOffs, nrows, ncols, newVal)) =>
          if (IR_MatNodeUtils.isScalar(nrows) && IR_MatNodeUtils.isScalar(ncols)) {
            val field = matrix.field
            val identifier = getIdentifier(field, matrix.slot)
            val rows = IR_SimplifyExpression.evalIntegral(nrows).toInt
            val cols = IR_SimplifyExpression.evalIntegral(ncols).toInt

            // TODO caching with offsets and strides

            // TODO rhs is const/vAcc -> extra handling (const: no read for rhs)

            // TODO: this is also an assignment -> put newVal as well
            fieldAccesses.put(identifier, IR_MatrixDatatype(matrix.datatype.resolveBaseDatatype, rows, cols))

            mapIndexToOffsets(field, identifier, Duplicate(matrix.index), matrix.index.length)
          } else {
            Logger.warn("Cannot determine field accesses for setSlice with runtime values.")
          }

        */
        case _  =>
          Logger.warn("Unsupported matrix function for performance estimation.")
      }
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
      case setSlice: IR_SetSlice =>
        // TODO
        Logger.error("Found setSlice")
      case setElem : IR_SetElement =>
        // TODO
        Logger.error("Found setElement")
      case r : IR_RuntimeMNode if !r.resolveAtRuntime =>
        mapCompiletimeMatrixFunctions(IR_ResolveMatFuncs.ctFctMap(r.name)(r))
        r
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
