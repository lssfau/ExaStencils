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

package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer
import scala.collection.{ Iterable, mutable }

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.util.NoDuplicateWrapper
import exastencils.util.ir.IR_FctNameCollector

/// CUDA_PrepareHostCode

/**
  * This transformation annotates LoopOverDimensions and LoopOverDimensions enclosed within ContractingLoops.
  * Additionally required statements for memory transfer are added.
  */
object CUDA_PrepareHostCode extends DefaultStrategy("Prepare CUDA relevant code by adding memory transfer statements " +
  "and annotating for later kernel transformation") {
  val collector = new IR_FctNameCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  def syncBeforeHost(access : String, others : Iterable[String]) = {
    var sync = true
    if (access.startsWith("write") && !Knowledge.cuda_syncHostForWrites)
      sync = false // skip write accesses if demanded
    if (access.startsWith("write") && others.exists(_ == "read" + access.substring("write".length)))
      sync = false // skip write access for read/write accesses
    sync
  }

  def syncAfterHost(access : String, others : Iterable[String]) = {
    access.startsWith("write")
  }

  def syncBeforeDevice(access : String, others : Iterable[String]) = {
    var sync = true
    if (access.startsWith("write") && !Knowledge.cuda_syncDeviceForWrites)
      sync = false // skip write accesses if demanded
    if (access.startsWith("write") && others.exists(_ == "read" + access.substring("write".length)))
      sync = false // skip write access for read/write accesses
    sync
  }

  def syncAfterDevice(access : String, others : Iterable[String]) = {
    access.startsWith("write")
  }

  def getHostDeviceSyncStmts(body : ListBuffer[IR_Statement], isParallel : Boolean) = {
    val (beforeHost, afterHost) = (ListBuffer[IR_Statement](), ListBuffer[IR_Statement]())
    val (beforeDevice, afterDevice) = (ListBuffer[IR_Statement](), ListBuffer[IR_Statement]())
    // don't filter here - memory transfer code is still required
    Logger.pushLevel(Logger.WARNING)
    val gatherFields = new CUDA_GatherFieldAccess()
    this.register(gatherFields)
    this.execute(new Transformation("Gather local FieldAccess nodes", PartialFunction.empty), Some(IR_Scope(body)))
    this.unregister(gatherFields)
    val gatherBuffers = new CUDA_GatherBufferAccess()
    this.register(gatherBuffers)
    this.execute(new Transformation("Gather local buffer access nodes", PartialFunction.empty), Some(IR_Scope(body)))
    this.unregister(gatherBuffers)
    Logger.popLevel()

    // host sync stmts

    for (access <- gatherFields.fieldAccesses.toSeq.sortBy(_._1)) {
      // add data sync statements
      if (syncBeforeHost(access._1, gatherFields.fieldAccesses.keys))
        beforeHost += CUDA_UpdateHostData(Duplicate(access._2)).expand().inner // expand here to avoid global expand afterwards

      // update flags for written fields
      if (syncAfterHost(access._1, gatherFields.fieldAccesses.keys))
        afterHost += IR_Assignment(CUDA_HostDataUpdated(access._2.field, Duplicate(access._2.slot)), IR_BooleanConstant(true))
    }

    for (access <- gatherBuffers.bufferAccesses.toSeq.sortBy(_._1)) {
      val buffer = access._2

      // add buffer sync statements
      if (syncBeforeHost(access._1, gatherBuffers.bufferAccesses.keys))
        beforeHost += CUDA_UpdateHostBufferData(Duplicate(access._2)).expand().inner // expand here to avoid global expand afterwards

      // update flags for written buffers
      if (syncAfterHost(access._1, gatherBuffers.bufferAccesses.keys))
        afterHost += IR_Assignment(CUDA_HostBufferDataUpdated(buffer.field, buffer.direction, Duplicate(buffer.neighIdx)), IR_BooleanConstant(true))
    }

    // trigger synchronize if memory transfers are eliminated and the execution mode is switched
    if (Knowledge.cuda_useZeroCopy || List("both", "device_to_host").contains(Knowledge.cuda_eliminate_memory_transfers)) {
      beforeHost += IR_IfCondition(CUDA_CurrentExecutionMode() Neq CUDA_CPUExecutionMode(),
        ListBuffer[IR_Statement](
          CUDA_DeviceSynchronize(),
          IR_Assignment(CUDA_CurrentExecutionMode(), CUDA_CPUExecutionMode())))
    }

    // device sync stmts

    if (isParallel) {
      if (Knowledge.cuda_syncDeviceAfterKernelCalls)
        afterDevice += CUDA_DeviceSynchronize()

      for (access <- gatherFields.fieldAccesses.toSeq.sortBy(_._1)) {
        // add data sync statements
        if (syncBeforeDevice(access._1, gatherFields.fieldAccesses.keys))
          beforeDevice += CUDA_UpdateDeviceData(Duplicate(access._2)).expand().inner // expand here to avoid global expand afterwards

        // update flags for written fields
        if (syncAfterDevice(access._1, gatherFields.fieldAccesses.keys))
          afterDevice += IR_Assignment(CUDA_DeviceDataUpdated(access._2.field, Duplicate(access._2.slot)), IR_BooleanConstant(true))
      }

      for (access <- gatherBuffers.bufferAccesses.toSeq.sortBy(_._1)) {
        val buffer = access._2

        // add data sync statements
        if (syncBeforeDevice(access._1, gatherBuffers.bufferAccesses.keys))
          beforeDevice += CUDA_UpdateDeviceBufferData(Duplicate(access._2)).expand().inner // expand here to avoid global expand afterwards

        // update flags for written fields
        if (syncAfterDevice(access._1, gatherBuffers.bufferAccesses.keys))
          afterDevice += IR_Assignment(CUDA_DeviceBufferDataUpdated(buffer.field, buffer.direction, Duplicate(buffer.neighIdx)), IR_BooleanConstant(true))
      }

      // trigger synchronize if memory transfers are eliminated and the execution mode is switched
      if (Knowledge.cuda_useZeroCopy || List("both", "host_to_device").contains(Knowledge.cuda_eliminate_memory_transfers)) {
        beforeDevice += IR_IfCondition(CUDA_CurrentExecutionMode() Neq CUDA_GPUExecutionMode(),
          ListBuffer[IR_Statement](
            CUDA_DeviceSynchronize(),
            IR_Assignment(CUDA_CurrentExecutionMode(), CUDA_GPUExecutionMode())))
      }
    }

    (beforeHost, afterHost, beforeDevice, afterDevice)
  }

  def getBranch(condWrapper : NoDuplicateWrapper[IR_Expression], loop : IR_LoopOverDimensions, hostStmts : ListBuffer[IR_Statement], deviceStmts : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = {
    condWrapper.value = Knowledge.cuda_preferredExecution match {
      case "Host"        => IR_BooleanConstant(true) // CPU by default
      case "Device"      => IR_BooleanConstant(false) // GPU by default
      case "Performance" => IR_BooleanConstant(loop.getAnnotation("perf_timeEstimate_host").get.asInstanceOf[Double] <= loop.getAnnotation("perf_timeEstimate_device").get.asInstanceOf[Double]) // decide according to performance estimates
      case "Condition"   => Knowledge.cuda_executionCondition
    }
    // set dummy first to prevent IR_GeneralSimplify from removing the branch statement until the condition is final
    val branch = IR_IfCondition(IR_VariableAccess("replaceIn_CUDA_AnnotateLoops", IR_BooleanDatatype), hostStmts, deviceStmts)
    branch.annotate(CUDA_Util.CUDA_BRANCH_CONDITION, condWrapper)
    ListBuffer[IR_Statement](branch)
  }

  this += new Transformation("Process ContractingLoop and LoopOverDimensions nodes", {
    case cl : IR_ContractingLoop =>
      val hostStmts = new ListBuffer[IR_Statement]()
      val deviceStmts = new ListBuffer[IR_Statement]()
      val fieldOffset = new mutable.HashMap[(String, Int), Int]()
      val fields = new mutable.HashMap[(String, Int), IR_Field]()
      var hostCondStmt : IR_IfCondition = null
      var deviceCondStmt : IR_IfCondition = null

      val containedLoop = cl.body.find(s =>
        s.isInstanceOf[IR_IfCondition] || s.isInstanceOf[IR_LoopOverDimensions]) match {
        case Some(IR_IfCondition(cond, trueBody : ListBuffer[IR_Statement], ListBuffer())) =>
          val bodyWithoutComments = trueBody.filterNot(x => x.isInstanceOf[IR_Comment])
          bodyWithoutComments match {
            case ListBuffer(loop : IR_LoopOverDimensions) => loop
            case _                                        => IR_LoopOverDimensions(0, IR_ExpressionIndexRange(IR_ExpressionIndex(), IR_ExpressionIndex()), ListBuffer[IR_Statement]())
          }
        case Some(loop : IR_LoopOverDimensions)                                            =>
          loop
        case None                                                                          => IR_LoopOverDimensions(0, IR_ExpressionIndexRange(IR_ExpressionIndex(), IR_ExpressionIndex()), ListBuffer[IR_Statement]())
      }

      // every LoopOverDimensions statement is potentially worse to transform in CUDA code
      // Exceptions:
      // 1. this loop is a special one and cannot be optimized in polyhedral model
      // 2. this loop has no parallel potential
      // use the host for dealing with the two exceptional cases
      val isParallel = containedLoop.parallelization.potentiallyParallel // filter some generate loops?

      // calculate memory transfer statements for host and device
      val (beforeHost, afterHost, beforeDevice, afterDevice) = getHostDeviceSyncStmts(containedLoop.body, isParallel)

      hostStmts ++= beforeHost
      deviceStmts ++= beforeDevice

      val condWrapper = NoDuplicateWrapper[IR_Expression](null)
      // resolve contracting loop
      // determine the inital expand value (it must be decreased for every IR_LoopOverDimensions node and it must reach 0 eventually)
      var expand : Int = -1 + cl.number * cl.body.view.flatMap({
        case IR_IfCondition(_, trueBody : ListBuffer[IR_Statement], ListBuffer()) => trueBody
        case x                                                                    => List(x)
      }).count({
        case _ : IR_LoopOverDimensions => true
        case _                         => false
      })
      for (i <- 0 until cl.number)
        for (stmt <- cl.body)
          stmt match {
            case IR_AdvanceSlot(IR_IV_ActiveSlot(field, fragment), step) =>
              val fKey = (field.name, field.level)
              fieldOffset(fKey) = fieldOffset.getOrElse(fKey, 0) + step
              fields(fKey) = field

            case cStmt @ IR_IfCondition(cond, trueBody : ListBuffer[IR_Statement], ListBuffer()) =>
              val bodyWithoutComments = trueBody.filterNot(x => x.isInstanceOf[IR_Comment])
              bodyWithoutComments match {
                case ListBuffer(l : IR_LoopOverDimensions) =>
                  val nju = cl.processLoopOverDimensions(l, expand, fieldOffset)

                  if (hostCondStmt == null || cond != hostCondStmt.condition) {
                    hostCondStmt = Duplicate(cStmt)
                    hostCondStmt.trueBody.clear()
                    deviceCondStmt = Duplicate(hostCondStmt)
                    hostStmts += hostCondStmt
                    deviceStmts += deviceCondStmt
                  }
                  hostCondStmt.trueBody += nju

                  if (isParallel) {
                    val njuCuda = Duplicate(nju)
                    njuCuda.annotate(CUDA_Util.CUDA_LOOP_ANNOTATION, condWrapper)
                    deviceCondStmt.trueBody += njuCuda
                  }
                  expand += 1
                case _                                     =>
              }

            case l : IR_LoopOverDimensions =>
              val loop = cl.processLoopOverDimensions(l, expand, fieldOffset)
              hostStmts += loop

              if (isParallel) {
                val loopCuda = Duplicate(loop)
                loopCuda.annotate(CUDA_Util.CUDA_LOOP_ANNOTATION, condWrapper)
                deviceStmts += loopCuda
              }
              expand += 1
          }

      hostStmts ++= afterHost
      deviceStmts ++= afterDevice
      // lists are already added to branch

      val res = if (isParallel) getBranch(condWrapper, containedLoop, hostStmts, deviceStmts) else hostStmts

      for ((fKey, offset) <- fieldOffset) {
        val field = fields(fKey)
        res += IR_AdvanceSlot(IR_IV_ActiveSlot(field), offset)
      }

      res

    case loop : IR_LoopOverDimensions =>
      val hostStmts = ListBuffer[IR_Statement]()
      val deviceStmts = ListBuffer[IR_Statement]()

      // every LoopOverDimensions statement is potentially worth to transform in CUDA code
      // Exceptions:
      // 1. this loop is a special one and cannot be optimized in polyhedral model
      // 2. this loop has no parallel potential
      // use the host for dealing with the two exceptional cases
      val isParallel = loop.parallelization.potentiallyParallel // filter some generate loops?

      // calculate memory transfer statements for host and device
      val (beforeHost, afterHost, beforeDevice, afterDevice) = getHostDeviceSyncStmts(loop.body, isParallel)

      hostStmts ++= beforeHost
      deviceStmts ++= beforeDevice

      hostStmts += loop
      val condWrapper = NoDuplicateWrapper[IR_Expression](null)
      if (isParallel) {
        val loopCuda = Duplicate(loop)
        loopCuda.annotate(CUDA_Util.CUDA_LOOP_ANNOTATION, condWrapper)
        loopCuda.polyOptLevel = math.min(2, loopCuda.polyOptLevel) // do not perform a tiling!
        //loopCuda.polyOptLevel = 0 // is there a way to create only perfectly nested loops using the isl? (check with correction code) - should be fixed now
        deviceStmts += loopCuda
      }

      hostStmts ++= afterHost
      deviceStmts ++= afterDevice
      // lists are already added to branch

      if (isParallel) getBranch(condWrapper, loop, hostStmts, deviceStmts) else hostStmts
  }, false)
}
