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

import scala.collection.mutable
import scala.collection.mutable._

import java.io.PrintWriter

import exastencils.base.ir._
import exastencils.base.l4.L4_SpecialFunctionReferences
import exastencils.baseExt.ir.IR_MatNodes._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.UnrollInnermost.UpdateLoopVarAndNames
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
      val dataTypeSizes = fieldAccesses.map(entry => entry._1 -> entry._2.typicalByteSize)

      // assume perfect blocking if opt_loopBlocked is activated ...
      if (Knowledge.opt_loopBlocked)
        return dataTypeSizes.values.sum

      // ... otherwise determine the number of data that needs to be loaded/stored taking cache sizes into account
      val effCacheSize = (Platform.hw_usableCache * PlatformUtils.cacheSizePerThread).toInt
      val maxWindowCount : Int = offsets.map(_._2.length).sum

      for (windowCount <- 1 to maxWindowCount) {
        val windowSizes = dataTypeSizes.map(entry => entry._1 -> effCacheSize / windowCount / entry._2)
        var empty : ListBuffer[Boolean] = ListBuffer.empty[Boolean]
        var windowsUsed = ListBuffer[Int]()

        fieldAccesses.keys.foreach(ident => {
          var sortedOffsets = offsets(ident).sorted.reverse
          val length = sortedOffsets.length
          var i = 1
          do {
            val maxOffset = sortedOffsets.head
            sortedOffsets = sortedOffsets.drop(1).filter(offset => math.abs(maxOffset - offset) > windowSizes(ident))
            windowsUsed += dataTypeSizes(ident)
            i += 1
          } while (i < length && i <= windowCount && sortedOffsets.nonEmpty)
          empty += sortedOffsets.isEmpty

        })
        if (windowsUsed.size <= windowCount && !empty.contains(false))
          return windowsUsed.sum
      }
      dataTypeSizes.map(entry => offsets(entry._1).length * entry._2).sum
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

          // apply arithmetic simplifications locally before counting ops
          val localLoop = Duplicate(loop)
          IR_SimplifyFloatExpressions.applyStandalone(localLoop)
          IR_GeneralSimplify.doUntilDoneStandalone(localLoop)
          EvaluateForOps.applyStandalone(localLoop)

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

          //stmts += IR_Comment(s"Estimated adds: ${ EvaluateForOps.numAdd }")
          //stmts += IR_Comment(s"Estimated muls: ${ EvaluateForOps.numMul }")
          //stmts += IR_Comment(s"Estimated divs: ${ EvaluateForOps.numDiv }")

          stmts += IR_Comment(s"Host time for computational ops: ${ estimatedTimeOps_host * 1000.0 } ms")
          stmts += IR_Comment(s"Device time for computational ops: ${ estimatedTimeOps_device * 1000.0 } ms")

          stmts += IR_Comment(s"Additions: ${ EvaluateForOps.numAdd }")
          stmts += IR_Comment(s"Multiplications: ${ EvaluateForOps.numMul}")
          stmts += IR_Comment(s"Divisions: ${ EvaluateForOps.numDiv}")

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

    // cache access pattern for consecutive vector/matrix field accesses
    var accessedMatrixEntries = HashMap[String, mutable.BitSet]()
    def getMatrixCache(identifier : String) = accessedMatrixEntries.getOrElse(identifier, mutable.BitSet.empty)
    def putMatrixCache(identifier : String, bs : mutable.BitSet) = accessedMatrixEntries.put(identifier, bs)
    def updateOrPutAccessedDatatype(identifier : String, dt : IR_Datatype) =
      if (fieldAccesses.contains(identifier)) fieldAccesses.update(identifier, dt) else fieldAccesses.put(identifier, dt)

    def evalExpr(expr : IR_Expression) : Int = IR_SimplifyExpression.evalIntegral(expr).toInt

    def updateForMatrixEntry(field : IR_Field, accessIndex : IR_ExpressionIndex, identifier : String, linearizedMatIndex : Int, baseDt : IR_Datatype) : Unit = {
      // cache accessed entry
      val bs = getMatrixCache(identifier)
      bs += linearizedMatIndex
      putMatrixCache(identifier, bs)

      // update or put access with matrix dt
      updateOrPutAccessedDatatype(identifier, IR_ArrayDatatype(baseDt, bs.size))
      mapIndexToOffsets(field, identifier, Duplicate(accessIndex), accessIndex.length)
    }

    def updateForMatrixEntries(field : IR_Field, accessIndex : IR_ExpressionIndex, identifier : String, startRow : Int, startCol : Int, endRow : Int, endCol : Int, baseDt : IR_Datatype) : Unit = {
      // cache accessed entries
      val bs = getMatrixCache(identifier)
      val numCols = field.gridDatatype.asInstanceOf[IR_MatrixDatatype].sizeN
      for (row <- startRow until endRow) {
        for (col <- startCol until endCol) {
          bs += row * numCols + col
        }
      }
      putMatrixCache(identifier, bs)

      // update or put access with matrix dt
      updateOrPutAccessedDatatype(identifier, IR_ArrayDatatype(baseDt, bs.size))
      mapIndexToOffsets(field, identifier, Duplicate(accessIndex), accessIndex.length)
    }

    override def reset() = {
      fieldAccesses.clear
      offsets.clear
      multiDimOffsets.clear
      accessedMatrixEntries.clear()

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

    def getIdentifier(field : IR_Field, slot : IR_Expression, isWrite : Boolean) = {
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
      val identifier = getIdentifier(field, access.slot, inWriteOp)

      // honor component accesses for HODT
      access match {
        case fAcc : IR_FieldAccess       =>
          if (fAcc.matIndex.isDefined) {
            // component access
            val matIdx = fAcc.matIndex.get
            val baseDt = field.resolveBaseDatatype

            // get number of elements accessed with matIndex
            def getActualRange(left : Option[IR_Expression], defaultLeft : Int, right : Option[IR_Expression], defaultRight : Int) : (IR_Expression, IR_Expression) =
              (left.getOrElse(IR_IntegerConstant(defaultLeft)), right.getOrElse(IR_IntegerConstant(defaultRight)))

            def singleElementRange = (IR_IntegerConstant(0), IR_IntegerConstant(0))

            val (startRow, endRow) = matIdx.y match {
              case _ @ IR_RangeIndex(range) if fAcc.field.gridDatatype.isInstanceOf[IR_MatrixDatatype] =>
                getActualRange(range(0).begin, 0, range(0).end, fAcc.field.gridDatatype.asInstanceOf[IR_MatrixDatatype].sizeM)
              case _                                                                                   =>
                singleElementRange
            }

            val (startCol, endCol) = if (matIdx.x.isDefined) {
              matIdx.x.get match {
                case _ @ IR_RangeIndex(range) if fAcc.field.gridDatatype.isInstanceOf[IR_MatrixDatatype] =>
                  getActualRange(range(0).begin, 0, range(0).end, fAcc.field.gridDatatype.asInstanceOf[IR_MatrixDatatype].sizeN)
                case _                                                                                   =>
                  singleElementRange
              }
            } else {
              singleElementRange
            }

            updateForMatrixEntries(field, access.index, identifier,
              evalExpr(startRow), evalExpr(startCol), evalExpr(endRow), evalExpr(endCol), baseDt)
          } else {
            // no matIndex set -> put "whole" grid datatype
            updateOrPutAccessedDatatype(identifier, field.gridDatatype)
            mapIndexToOffsets(field, identifier, Duplicate(access.index), access.index.length)
          }
        case fAcc : IR_DirectFieldAccess =>
          // actual dt determined in IR_DirectFieldAccess depending on length of access index
          // TODO: is caching applicable here? In 2D, indices for matrices have form [i0, i1, i2, i3]
          updateOrPutAccessedDatatype(identifier, fAcc.datatype)
          mapIndexToOffsets(field, identifier, Duplicate(access.index), access.index.length)
        case acc : IR_Access             =>
          Logger.error("EvaluateFieldAccess: Match error. Did not expect access: " + acc.prettyprint())
      }
    }

    // matIndex not set for matrix functions -> need separate handling
    def mapCompiletimeMatrixFctExpr(matFunc : IR_Expression) {
      matFunc match {
        // getElement
        case IR_GetElement(ListBuffer(inMat : IR_MultiDimFieldAccess, idxy, idxx)) =>
          val field = inMat.field
          val identifier = getIdentifier(field, inMat.slot, isWrite = inWriteOp)

          if (IR_CompiletimeMatOps.isConst(idxy) && IR_CompiletimeMatOps.isConst(idxx)) {
            // lookup cache for entry and update if necessary
            val matDims = inMat.field.gridDatatype.asInstanceOf[IR_MatrixDatatype]
            val idx = List(idxx, idxy).map(IR_SimplifyExpression.evalIntegral(_).toInt)
            val linearizedMatIdx = idx(0) + matDims.sizeN * idx(1)

            updateForMatrixEntry(field, inMat.index, identifier, linearizedMatIdx, field.resolveBaseDatatype)
          } else {
            Logger.warn("Cannot determine field accesses for getElement with runtime values.")
          }
        // getSlice
        case IR_GetSliceCT(inMat : IR_MultiDimFieldAccess, ListBuffer(offsetRows : Int, offsetCols : Int, nrows : Int, ncols : Int)) =>
          val field = inMat.field
          val identifier = getIdentifier(field, inMat.slot, isWrite = inWriteOp)

          updateForMatrixEntries(field, inMat.index, identifier, offsetRows, offsetCols, offsetRows + nrows, offsetCols + ncols, field.resolveBaseDatatype)
        case m                                                                                                                       =>
          Logger.warn("Unsupported matrix function for performance estimation: " + m.getClass.getName)
      }
    }

    def mapCompiletimeMatrixFctStmts(matFunc : IR_ResolvableMNode) {
      matFunc match {
        // setElement
        case IR_SetElement(ListBuffer(dst, idxy, idxx, rhs)) =>

          if (IR_CompiletimeMatOps.isConst(idxy) && IR_CompiletimeMatOps.isConst(idxx)) {

            // update "fieldAccesses" if lhs is a field instance
            dst match {
              case lhs : IR_MultiDimFieldAccess =>
                val lhsField = lhs.field
                val lhsWriteIdentifier = getIdentifier(lhsField, lhs.slot, isWrite = true)
                val lhsReadIdentifier = getIdentifier(lhsField, lhs.slot, isWrite = false)
                val dt = lhsField.resolveBaseDatatype

                val matDims = lhs.field.gridDatatype.asInstanceOf[IR_MatrixDatatype]
                val idx = List(idxx, idxy).map(IR_SimplifyExpression.evalIntegral(_).toInt)
                val linearizedMatIdx = idx(0) + matDims.sizeN * idx(1)

                // write and read allocate for lhs
                updateForMatrixEntry(lhsField, lhs.index, lhsWriteIdentifier, linearizedMatIdx, dt)
                updateForMatrixEntry(lhsField, lhs.index, lhsReadIdentifier, linearizedMatIdx, dt)
              case _                            =>
            }

            // read rhs if not const
            if (!IR_CompiletimeMatOps.isConst(rhs)) {
              inWriteOp = false
              EvaluateFieldAccess.applyStandalone(IR_ExpressionStatement(rhs))
            }
          } else {
            Logger.warn("Cannot determine field accesses for setElement with runtime values.")
          }

        // setSlice
        case IR_SetSlice(ListBuffer(dst, rowOffs, colOffs, nrows, ncols, newVal)) =>

          if (IR_CompiletimeMatOps.isConst(nrows) && IR_CompiletimeMatOps.isConst(rowOffs) && IR_CompiletimeMatOps.isConst(ncols) && IR_CompiletimeMatOps.isConst(colOffs)) {

            // update "fieldAccesses" if lhs is a field instance
            dst match {
              case lhs : IR_MultiDimFieldAccess =>
                val field = lhs.field
                val readIdentifier = getIdentifier(field, lhs.slot, isWrite = false)
                val writeIdentifier = getIdentifier(field, lhs.slot, isWrite = true)
                val (startRow, endRow) = (evalExpr(rowOffs), evalExpr(rowOffs + nrows))
                val (startCol, endCol) = (evalExpr(colOffs), evalExpr(colOffs + ncols))

                // honor read-allocate
                updateForMatrixEntries(field, lhs.index, writeIdentifier, startRow, startCol, endRow, endCol, field.resolveBaseDatatype)
                updateForMatrixEntries(field, lhs.index, readIdentifier, startRow, startCol, endRow, endCol, field.resolveBaseDatatype)
              case _                            =>
            }

            // read newval if not constant
            newVal match {
              case matExpr : IR_MatrixExpression if !IR_CompiletimeMatOps.isConstMatrix(matExpr) =>
                inWriteOp = false
                EvaluateFieldAccess.applyStandalone(IR_ExpressionStatement(newVal))
              case expr : IR_Expression if !IR_CompiletimeMatOps.isConst(expr)                   =>
                inWriteOp = false
                EvaluateFieldAccess.applyStandalone(IR_ExpressionStatement(newVal))
              case _                                                                             =>
            }
          } else {
            Logger.warn("Cannot determine field accesses for setSlice with runtime values.")
          }
        case m                                                                    =>
          Logger.warn("Unsupported matrix function for performance estimation: " + m.getClass.getName)
      }
    }

    this += new Transformation("Searching", {
      // inner loop -> unroll
      case loop @ IR_ForLoop(loopStart, loopEnd, inc, body, _) =>
        var prependStmts = ListBuffer[IR_Comment]()
        try {
          val (itVar, start, end, incr) = UnrollInnermost.extractBoundsAndIncrement(loopStart, loopEnd, inc)
          val unrollFact = IR_SimplifyExpression.evalIntegral(end - start) / incr
          val replaceStrat = new UpdateLoopVarAndNames(itVar)
          val dups = new ListBuffer[IR_Statement]()
          for (i <- 0L until unrollFact) {
            val dup = Duplicate(body)

            // replace with unrolled version
            replaceStrat.offset = i * incr
            replaceStrat.applyStandalone(dup)

            // replace loop variable with 0 -> offset remains
            IR_ReplaceVariableAccess.replace = List(itVar -> IR_IntegerConstant(0)).toMap
            IR_ReplaceVariableAccess.applyStandalone(dup)

            dups ++= dup.filter(s => !s.isInstanceOf[IR_Comment])
          }

          EvaluateFieldAccess.applyStandalone(dups)
        } catch {
          case EvaluationException(msg, _) =>
            Logger.warn("Cannot evaluate loop bounds: " + msg)
            prependStmts += IR_Comment(s"--- Loop with variable bounds is handled as loop doing one iteration. Remember to scale accordingly. ---")
            EvaluateFieldAccess.applyStandalone(loop.body)
          case UnrollException(msg)        =>
            Logger.warn("Cannot evaluate fields accessed in loop with variable bounds: " + msg)
            loopStart match {
              case IR_VariableDeclaration(_, runtimeVar, _, _) =>
                prependStmts += IR_Comment(s"-- Loop with variable bounds is handled as loop doing one iteration. Remember to scale with '$runtimeVar'. --")
              case _                                           =>
                prependStmts += IR_Comment(s"-- Loop with variable bounds is handled as loop doing one iteration. Remember to scale accordingly. --")
            }
            EvaluateFieldAccess.applyStandalone(loop.body)
        }
        prependStmts :+ loop
      case loop : IR_WhileLoop                                 =>
        EvaluateFieldAccess.applyStandalone(loop.body)
        loop.body.prepend(IR_Comment(s"-- While loop is handled as loop doing one iteration. Remember to scale accordingly. --"))
        loop
      case assign : IR_Assignment                              =>
        inWriteOp = true
        EvaluateFieldAccess.applyStandalone(IR_ExpressionStatement(assign.dest))
        inWriteOp = false
        // honor read-allocate
        EvaluateFieldAccess.applyStandalone(IR_ExpressionStatement(assign.dest))
        EvaluateFieldAccess.applyStandalone(IR_ExpressionStatement(assign.src))
        assign
      case access : IR_MultiDimFieldAccess                     =>
        mapFieldAccess(access)
        access
      // not to resolve at runtime
      case r : IR_RuntimeMNode if !r.resolveAtRuntime =>
        mapCompiletimeMatrixFctExpr(IR_ResolveMatFuncs.ctFctMap(r.name)(r))
        r
      case mn : IR_ResolvableMNode                    =>
        mn match {
          case _ : IR_Statement  => mapCompiletimeMatrixFctStmts(mn)
          case _ : IR_Expression => mapCompiletimeMatrixFctExpr(mn.asInstanceOf[IR_Expression])
        }
        mn
      case fctCall : IR_FunctionCall if L4_SpecialFunctionReferences.luSolve.pattern.matcher(fctCall.name).matches() =>
        val (m, n) = fctCall.name match { case L4_SpecialFunctionReferences.luSolve(x, y) => (x, y) }

        // FIXME
        fctCall

    }, false)
  }

  object EvaluateForOps extends QuietDefaultStrategy("Evaluating performance for numeric operations") {

    class IR_ForOpsCollector extends Collector {
      var numAdd = 0
      var numMul = 0
      var numDiv = 0

      var inIndex = false

      override def enter(node : Node) : Unit = {
        node match {
          case add : IR_Addition if add.summands.nonEmpty =>
            if (!inIndex)
              numAdd += add.summands.length - 1

          case _ : IR_Subtraction =>
            if (!inIndex)
              numAdd += 1

          case mul : IR_Multiplication if mul.factors.nonEmpty =>
            if (!inIndex)
              numMul += mul.factors.length - 1

          case div : IR_Division =>
            if (!inIndex)
              div.right match {
                case _ : IR_IntegerConstant => numMul += 0
                case _ : IR_RealConstant    => numMul += 1
                case _                      => numDiv += 1
              }

          case _ : IR_Index => // skip index calculations
            inIndex = true

          case _ =>
        }
      }

      override def leave(node : Node) : Unit = {
        node match {
          case _ : IR_Index => // skip index calculations
            inIndex = false

          case _ =>
        }
      }

      override def reset() : Unit = {
        numAdd = 0
        numMul = 0
        numDiv = 0

        inIndex = false
      }
    }

    var collector = new IR_ForOpsCollector()
    this.register(collector)

    def numAdd = collector.numAdd
    def numMul = collector.numMul
    def numDiv = collector.numDiv

    override def applyStandalone(node : Node) : Unit = {
      collector.reset()

      // skip loop sub-nodes that are not part of the body
      node.asInstanceOf[IR_LoopOverDimensions].body.foreach(super.applyStandalone)
    }

    this += new Transformation("Searching", {
      case comment : IR_Comment => comment // TODO: should be an empty transformation
    })
  }

}
