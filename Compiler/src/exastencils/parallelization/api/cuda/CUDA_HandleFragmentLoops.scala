package exastencils.parallelization.api.cuda

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir.IR_IV_CommBuffer
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.OutputType
import exastencils.field.ir.IR_IV_FieldData
import exastencils.parallelization.api.cuda.CUDA_HandleFragmentLoops.getReductionCounter
import exastencils.parallelization.ir.IR_HasParallelizationInfo

/// CUDA_HandleFragmentLoops
// - for multi-fragment kernel calls
// - uses fragment-local copies of the reduction variable's initial value
// - uses CUDA stream for each fragment
// - additional fragment loop added for stream synchronization and reduction result accumulation

object CUDA_HandleFragmentLoops extends DefaultStrategy("Handle synchronization and reductions in CUDA parallel fragment loops") {

  private var reductionCounters : mutable.HashMap[String, Int] = mutable.HashMap()

  def getReductionCounter(targetName : String) = {
    val c = reductionCounters.getOrElseUpdate(targetName, 0)
    reductionCounters(targetName) += 1
    c
  }

  this += Transformation("Expand CUDA_HandleFragmentLoops nodes", {
    case handle : CUDA_HandleFragmentLoops => handle.expandSpecial()
  })
}

case class CUDA_AccessedElementsInFragmentLoop(
    var streams : ListBuffer[CUDA_Stream],
    var fieldAccesses : mutable.HashMap[String, IR_IV_FieldData],
    var bufferAccesses : mutable.HashMap[String, IR_IV_CommBuffer],
    var isLoopParallel : Boolean,
    var fromMPIStatement : Boolean,
    var estimatedHostTime : Double,
    var estimatedDeviceTime : Double
)

case class CUDA_HandleFragmentLoops(
    var fragLoop : IR_ScopedStatement with IR_HasParallelizationInfo,
    var accessedElements : CUDA_AccessedElementsInFragmentLoop
) extends IR_Statement with IR_SpecialExpandable with CUDA_PrepareBufferSync with CUDA_ExecutionBranching {

  // forward members
  def streams = accessedElements.streams
  def fieldAccesses = accessedElements.fieldAccesses
  def bufferAccesses = accessedElements.bufferAccesses
  def isParallel = accessedElements.isLoopParallel
  def fromMPIStatement = accessedElements.fromMPIStatement
  def fasterHostExecEstimation = accessedElements.estimatedHostTime <= accessedElements.estimatedDeviceTime

  val iter = IR_LoopOverFragments.defIt
  def currCopy(copies : IR_VariableAccess) = IR_ArrayAccess(copies, iter)
  def reductionDt(redTarget : IR_Expression) = CUDA_Util.getReductionDatatype(redTarget)

  // tmp buffer for reduction result (host)
  val reductionTmp = if (fragLoop.parallelization.reduction.isDefined) {
    val red = Duplicate(fragLoop.parallelization.reduction.get)
    val redTarget = Duplicate(red.target)

    val ret = reductionDt(redTarget) match {
      case mat : IR_MatrixDatatype    =>
        CUDA_ReductionResultBuffer(s"${ red.targetName }_${ getReductionCounter(red.targetName) }_reductionTmpMatrix", mat.resolveBaseDatatype, mat.sizeN * mat.sizeM)
      case scalar : IR_ScalarDatatype =>
        CUDA_ReductionResultBuffer(s"${ red.targetName }_${ getReductionCounter(red.targetName) }_reductionTmp", scalar.resolveBaseDatatype, 1)
    }

    Some(ret)
  } else {
    None
  }
  fragLoop.annotate(CUDA_Util.CUDA_REDUCTION_RESULT_BUF, reductionTmp)

  // replace occurrences of reduction target with its copy
  private object CUDA_ReplaceReductionAccesses extends QuietDefaultStrategy("Replace accesses to reduction targets") {
    var redTarget : IR_Expression = IR_NullExpression
    var replacement : IR_Expression = IR_NullExpression

    this += new Transformation("Replace", {
      case expr : IR_Expression if expr == redTarget => Duplicate(replacement)
    })
  }

  def matrixAssignment(stdFunc : String, dst : IR_Expression, src : IR_Expression, size : Int) =
    IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference(stdFunc, IR_UnitDatatype),
      ListBuffer[IR_Expression](Duplicate(dst), Duplicate(dst) + IR_IntegerConstant(size), src)))

  def copyReductionTarget(redTarget : IR_Expression, op : String, copies : IR_VariableAccess) = {
    val tpe = redTarget.datatype.resolveBaseDatatype.prettyprint()
    val initVal : IR_Expression = op match {
      case "+" | "-" => 0
      case "*" => 1
      case "max" => IR_FunctionCall(s"std::numeric_limits<$tpe>::min")
      case "min" => IR_FunctionCall(s"std::numeric_limits<$tpe>::max")
    }

    reductionDt(redTarget) match {
      case _ : IR_ScalarDatatype               =>
        IR_Assignment(currCopy(copies), initVal)
      case hodt : IR_HigherDimensionalDatatype =>
        matrixAssignment("std::fill", currCopy(copies), initVal, hodt.getSizeArray.product)
    }
  }

  def initCopies(redTarget : IR_Expression, op : String, copies : IR_VariableAccess) = {
    // TODO: should be handled in prettyprinter
    val declCopies = copies.datatype match {
      case _ @ IR_ArrayDatatype(mat : IR_MatrixDatatype, numElements) =>
        IR_VariableDeclaration(IR_ArrayDatatype(IR_ArrayDatatype(mat.resolveBaseDatatype, mat.sizeN * mat.sizeM), numElements), copies.name)
      case _ : IR_Datatype =>
        IR_VariableDeclaration(copies)
    }
    val initCopies = IR_LoopOverFragments(copyReductionTarget(redTarget, op, copies)).expandSpecial().inner

    ListBuffer(declCopies, initCopies)
  }

  // finalize reduction
  def updateReductionTarget(op : String, redTarget : IR_Expression, reductionTmp : CUDA_ReductionResultBuffer, copies : IR_VariableAccess) : (ListBuffer[IR_Statement], ListBuffer[IR_Statement]) = {

    // update reduction target
    def getAssign(reductionResult : IR_Expression) = reductionDt(redTarget) match {
      case mat : IR_MatrixDatatype => // array returned

        val i = IR_VariableAccess("_i", IR_IntegerDatatype)
        val j = IR_VariableAccess("_j", IR_IntegerDatatype)
        val idx = i * mat.sizeN + j
        val dst = IR_ArrayAccess(redTarget, idx)
        val src = IR_ArrayAccess(reductionResult, idx)
        IR_ForLoop(IR_VariableDeclaration(i, IR_IntegerConstant(0)), IR_Lower(i, mat.sizeM), IR_PreIncrement(i), ListBuffer[IR_Statement](
          IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, mat.sizeN), IR_PreIncrement(j), ListBuffer[IR_Statement](
            IR_Assignment(dst, IR_BinaryOperators.createExpression(op, dst, src))))))

      case _ : IR_ScalarDatatype => // single value returned
        val dst = redTarget
        val src = if (reductionResult.datatype.isInstanceOf[IR_PointerDatatype])
          IR_ArrayAccess(reductionResult, 0)
        else
          reductionResult
        IR_Assignment(redTarget, IR_BinaryOperators.createExpression(op, dst, src))
    }

    // accumulate fragment copies into reduction variable
    val host = ListBuffer(getAssign(currCopy(copies)))
    val device = ListBuffer(getAssign(reductionTmp))

    (host, device)
  }

  def replaceAccesses(redTarget : IR_Expression, copies : IR_VariableAccess, body : ListBuffer[IR_Statement]) : Unit = {
    // replace redTarget accesses with accesses to frag copy
    CUDA_ReplaceReductionAccesses.redTarget = Duplicate(redTarget)
    CUDA_ReplaceReductionAccesses.replacement = Duplicate(currCopy(copies))
    CUDA_ReplaceReductionAccesses.applyStandalone(IR_Scope(body))
  }

  def syncUpdatedBuffers() = {
    val (beforeHost, afterHost) = (ListBuffer[IR_Statement](), ListBuffer[IR_Statement]())
    val (beforeDevice, afterDevice) = (ListBuffer[IR_Statement](), ListBuffer[IR_Statement]())

    // - host sync stmts -

    // sync kernel streams before issuing transfers
    val issuedSyncs = streams.flatMap(stream => CUDA_Stream.genSynchronize(stream, before = true))
    val requiredSyncs = CUDA_Stream.genCompSync() +: CUDA_Stream.genCommSync()
    if (streams.exists(_.useNonDefaultStreams))
      beforeHost ++= requiredSyncs.filterNot(issuedSyncs.contains(_))

    for (access <- fieldAccesses.toSeq.sortBy(_._1)) {
      val fieldData = access._2
      val transferStream = CUDA_TransferStream(fieldData.field, Duplicate(fieldData.fragmentIdx))

      // add data sync statements
      if (syncBeforeHost(access._1, fieldAccesses.keys))
        beforeHost += CUDA_UpdateHostData(Duplicate(fieldData), transferStream).expand().inner // expand here to avoid global expand afterwards

      // update flags for written fields
      if (syncAfterHost(access._1, fieldAccesses.keys))
        afterHost += IR_Assignment(CUDA_HostDataUpdated(fieldData.field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)), IR_BooleanConstant(true))
    }

    for (access <- bufferAccesses.toSeq.sortBy(_._1)) {
      val buffer = access._2
      val transferStream = CUDA_TransferStream(buffer.field, Duplicate(buffer.fragmentIdx))

      // add buffer sync statements
      if (syncBeforeHost(access._1, bufferAccesses.keys))
        beforeHost += CUDA_UpdateHostBufferData(Duplicate(buffer), transferStream).expand().inner // expand here to avoid global expand afterwards

      // update flags for written buffers
      if (syncAfterHost(access._1, bufferAccesses.keys))
        afterHost += IR_Assignment(CUDA_HostBufferDataUpdated(buffer.field, buffer.direction, Duplicate(buffer.neighIdx), Duplicate(buffer.fragmentIdx)), IR_BooleanConstant(true))
    }

    // - device sync stmts -

    if (isParallel) {
      for (access <- fieldAccesses.toSeq.sortBy(_._1)) {
        val fieldData = access._2
        val transferStream = CUDA_TransferStream(fieldData.field, Duplicate(fieldData.fragmentIdx))

        // add data sync statements
        if (syncBeforeDevice(access._1, fieldAccesses.keys))
          beforeDevice += CUDA_UpdateDeviceData(Duplicate(fieldData), transferStream).expand().inner // expand here to avoid global expand afterwards

        // update flags for written fields
        if (syncAfterDevice(access._1, fieldAccesses.keys))
          afterDevice += IR_Assignment(CUDA_DeviceDataUpdated(fieldData.field, Duplicate(fieldData.slot), Duplicate(fieldData.fragmentIdx)), IR_BooleanConstant(true))
      }
      for (access <- bufferAccesses.toSeq.sortBy(_._1)) {
        val buffer = access._2
        val transferStream = CUDA_TransferStream(buffer.field, Duplicate(buffer.fragmentIdx))

        // add data sync statements
        if (syncBeforeDevice(access._1, bufferAccesses.keys))
          beforeDevice += CUDA_UpdateDeviceBufferData(Duplicate(buffer), transferStream).expand().inner // expand here to avoid global expand afterwards

        // update flags for written fields
        if (syncAfterDevice(access._1, bufferAccesses.keys))
          afterDevice += IR_Assignment(CUDA_DeviceBufferDataUpdated(buffer.field, buffer.direction, Duplicate(buffer.neighIdx), Duplicate(buffer.fragmentIdx)), IR_BooleanConstant(true))
      }
    }

    (beforeHost, afterHost, beforeDevice, afterDevice)
  }

  def expandSpecial() : OutputType = {
    var stmts = ListBuffer[IR_Statement]()

    // sync before/after kernel calls in separate frag loop
    val syncBeforeFragLoop = IR_LoopOverFragments(streams.flatMap(stream => CUDA_Stream.genSynchronize(stream, before = true)))
    val syncAfterFragLoop = IR_LoopOverFragments(streams.flatMap(stream => CUDA_Stream.genSynchronize(stream, before = false)))

    val loopTuple = fragLoop match {
      case loop : IR_LoopOverFragments                                                               => Some((loop, loop.body))
      case loop @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == iter.name => Some((loop, loop.body))
      case _                                                                                         => None
    }

    // early exit: no loop to be transformed
    if (loopTuple.isEmpty)
      return ListBuffer(fragLoop)

    // fetch (resolved) frag loop (=> scoped stmt with ParallelizationInfo) and its body
    val loop = loopTuple.get._1
    val body = loopTuple.get._2

    // branching with cond wrapper not required as they are already resolved
    def branchingWrapper(hostStmts : ListBuffer[IR_Statement], deviceStmts : ListBuffer[IR_Statement]) = {
      if (fromMPIStatement)
        getHostDeviceBranchingMPI(hostStmts, deviceStmts)
      else
        getHostDeviceBranching(hostStmts, deviceStmts, fasterHostExecEstimation)
    }

    // handle reductions
    if (loop.parallelization.reduction.isDefined) {
      val red = Duplicate(loop.parallelization.reduction.get)
      val redTarget = Duplicate(red.target)

      // move reduction towards "synchroFragLoop"
      // -> OpenMP/MPI reduction occurs after accumulation in "synchroFragLoop"
      loop.parallelization.reduction = None
      syncAfterFragLoop.parallelization.reduction = Some(red)

      // force comp stream sync if comp kernels are not synced explicitly
      val syncsComp = syncAfterFragLoop.body.exists {
        case streamSync : CUDA_StreamSynchronize if streamSync.stream.isInstanceOf[CUDA_ComputeStream] => true
        case _ => false
      }
      if (!syncsComp)
        syncAfterFragLoop.body += CUDA_Stream.genCompSync()

      val counter = CUDA_HandleFragmentLoops.getReductionCounter(red.targetName)
      val copies = IR_VariableAccess(red.targetName + "_fragCpy" + counter, IR_ArrayDatatype(reductionDt(redTarget), Knowledge.domain_numFragmentsPerBlock))

      stmts ++= initCopies(redTarget, red.op, copies) // init frag copies
      replaceAccesses(redTarget, copies, body) // replace accesses to frag copies

      // assign orig reduction target to result in cpu/gpu fragment copy
      val (assignToHostBuffer, assignToDeviceBuffer) = updateReductionTarget(red.op, redTarget, reductionTmp.get, copies) // accumulate frag copies at end
      branchingWrapper(assignToHostBuffer, assignToDeviceBuffer)
    }

    // get syncs for updated buffers on device/host
    val (beforeHost, afterHost, beforeDevice, afterDevice) = syncUpdatedBuffers()

    stmts += syncBeforeFragLoop // add stream synchro loop before kernel calls
    stmts += IR_LoopOverFragments(branchingWrapper(beforeHost, beforeDevice))
    stmts += Duplicate(loop)
    stmts += IR_LoopOverFragments(branchingWrapper(afterHost, afterDevice))
    stmts += syncAfterFragLoop // add stream synchro loop after kernel calls

    stmts
  }
}
