package exastencils.parallelization.api.cuda

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.core.collectors.FctNameCollector
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.polyhedron.PolyhedronAccessible

/// CUDA_PrepareHostCode

/**
  * This transformation annotates LoopOverDimensions and LoopOverDimensions enclosed within ContractingLoops.
  * Additionally required statements for memory transfer are added.
  */
object CUDA_PrepareHostCode extends DefaultStrategy("Prepare CUDA relevant code by adding memory transfer statements " +
  "and annotating for later kernel transformation") {
  val collector = new FctNameCollector
  this.register(collector)

  def getHostDeviceSyncStmts(loop : IR_LoopOverDimensions, isParallel : Boolean) = {
    val (beforeHost, afterHost) = (ListBuffer[IR_Statement](), ListBuffer[IR_Statement]())
    val (beforeDevice, afterDevice) = (ListBuffer[IR_Statement](), ListBuffer[IR_Statement]())
    // don't filter here - memory transfer code is still required
    CUDA_GatherFieldAccess.fieldAccesses.clear
    CUDA_GatherFieldAccess.applyStandalone(IR_Scope(loop.body))

    // host sync stmts

    // add data sync statements
    for (access <- CUDA_GatherFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
      var sync = true
      if (access._1.startsWith("write") && !Knowledge.cuda_syncHostForWrites)
        sync = false // skip write accesses if demanded
      if (access._1.startsWith("write") && CUDA_GatherFieldAccess.fieldAccesses.contains("read" + access._1.substring("write".length)))
        sync = false // skip write access for read/write accesses
      if (sync)
        beforeHost += CUDA_UpdateHostData(Duplicate(access._2)).expand().inner // expand here to avoid global expand afterwards
    }

    // update flags for written fields
    for (access <- CUDA_GatherFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
      val fieldSelection = access._2.fieldSelection
      if (access._1.startsWith("write"))
        afterHost += IR_Assignment(CUDA_HostDataUpdated(fieldSelection.field, fieldSelection.slot), IR_BooleanConstant(true))
    }

    // device sync stmts

    if (isParallel) {
      // before: add data sync statements
      for (access <- CUDA_GatherFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
        var sync = true
        if (access._1.startsWith("write") && !Knowledge.cuda_syncDeviceForWrites)
          sync = false // skip write accesses if demanded
        if (access._1.startsWith("write") && CUDA_GatherFieldAccess.fieldAccesses.contains("read" + access._1.substring("write".length)))
          sync = false // skip write access for read/write accesses
        if (sync)
          beforeDevice += CUDA_UpdateDeviceData(Duplicate(access._2)).expand().inner // expand here to avoid global expand afterwards
      }

      if (Knowledge.cuda_syncDeviceAfterKernelCalls)
        afterDevice += CUDA_DeviceSynchronize()

      // update flags for written fields
      for (access <- CUDA_GatherFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
        val fieldSelection = access._2.fieldSelection
        if (access._1.startsWith("write"))
          afterDevice += IR_Assignment(CUDA_DeviceDataUpdated(fieldSelection.field, fieldSelection.slot), IR_BooleanConstant(true))
      }
    }

    (beforeHost, afterHost, beforeDevice, afterDevice)
  }

  def addHostDeviceBranching(hostStmts : ListBuffer[IR_Statement], deviceStmts : ListBuffer[IR_Statement], loop : IR_LoopOverDimensions, earlyExit : Boolean) : ListBuffer[IR_Statement] = {
    if (earlyExit) {
      hostStmts
    } else {
      /// compile final switch
      val defaultChoice = Knowledge.cuda_preferredExecution match {
        case "Host"        => 1 // CPU by default
        case "Device"      => 0 // GPU by default
        case "Performance" => if (loop.getAnnotation("perf_timeEstimate_host").get.asInstanceOf[Double] > loop.getAnnotation("perf_timeEstimate_device").get.asInstanceOf[Double]) 0 else 1 // decide according to performance estimates
      }

      ListBuffer[IR_Statement](IR_IfCondition(defaultChoice, hostStmts, deviceStmts))
    }
  }

  this += new Transformation("Processing ContractingLoop and LoopOverDimensions nodes", {
    case cl : IR_ContractingLoop      =>
      var hostStmts = new ListBuffer[IR_Statement]()
      var deviceStmts = new ListBuffer[IR_Statement]()
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
      val isParallel = containedLoop.isInstanceOf[PolyhedronAccessible] && containedLoop.parallelization.potentiallyParallel

      // calculate memory transfer statements for host and device
      val (beforeHost, afterHost, beforeDevice, afterDevice) = getHostDeviceSyncStmts(containedLoop, isParallel)

      hostStmts ++= beforeHost
      deviceStmts ++= beforeDevice

      // resolve contracting loop
      for (i <- 1 to cl.number)
        for (stmt <- cl.body)
          stmt match {
            case IR_AdvanceSlot(IR_IV_ActiveSlot(field, fragment)) =>
              val fKey = (field.identifier, field.level)
              fieldOffset(fKey) = fieldOffset.getOrElse(fKey, 0) + 1
              fields(fKey) = field

            case cStmt @ IR_IfCondition(cond, trueBody : ListBuffer[IR_Statement], ListBuffer()) =>
              val bodyWithoutComments = trueBody.filterNot(x => x.isInstanceOf[IR_Comment])
              bodyWithoutComments match {
                case ListBuffer(l : IR_LoopOverDimensions) =>
                  val nju = cl.processLoopOverDimensions(l, cl.number - i, fieldOffset)

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
                    njuCuda.annotate(CUDA_Util.CUDA_LOOP_ANNOTATION, collector.getCurrentName)
                    deviceCondStmt.trueBody += njuCuda
                  }
                case _                                     =>
              }
            case l : IR_LoopOverDimensions                                                       =>
              val loop = cl.processLoopOverDimensions(l, cl.number - i, fieldOffset)
              hostStmts += loop

              if (isParallel) {
                val loopCuda = Duplicate(loop)
                loopCuda.annotate(CUDA_Util.CUDA_LOOP_ANNOTATION, collector.getCurrentName)
                deviceStmts += loopCuda
              }
          }

      hostStmts ++= afterHost
      deviceStmts ++= afterDevice

      val res = addHostDeviceBranching(hostStmts, deviceStmts, containedLoop, !isParallel)

      for ((fKey, offset) <- fieldOffset) {
        val field = fields(fKey)
        res += IR_Assignment(IR_IV_ActiveSlot(field), (IR_IV_ActiveSlot(field) + offset) Mod field.numSlots)
      }

      res
    case loop : IR_LoopOverDimensions =>
      val hostStmts = ListBuffer[IR_Statement]()
      val deviceStmts = ListBuffer[IR_Statement]()

      // every LoopOverDimensions statement is potentially worse to transform in CUDA code
      // Exceptions:
      // 1. this loop is a special one and cannot be optimized in polyhedral model
      // 2. this loop has no parallel potential
      // use the host for dealing with the two exceptional cases
      val isParallel = loop.isInstanceOf[PolyhedronAccessible] && loop.parallelization.potentiallyParallel

      // calculate memory transfer statements for host and device
      val (beforeHost, afterHost, beforeDevice, afterDevice) = getHostDeviceSyncStmts(loop, isParallel)

      hostStmts ++= beforeHost
      deviceStmts ++= beforeDevice

      hostStmts += loop
      if (isParallel) {
        val loopCuda = Duplicate(loop)
        loopCuda.annotate(CUDA_Util.CUDA_LOOP_ANNOTATION, collector.getCurrentName)
        deviceStmts += loopCuda
      }

      hostStmts ++= afterHost
      deviceStmts ++= afterDevice

      addHostDeviceBranching(hostStmts, deviceStmts, loop, !isParallel)
  }, false)
}
