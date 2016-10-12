package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.boundary.ir._
import exastencils.config._
import exastencils.core._
import exastencils.core.collectors.StackCollector
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir.IR_SlotAccess
import exastencils.knowledge.Fragment
import exastencils.logger._
import exastencils.parallelization.api.mpi.MPI_WaitForRequest
import exastencils.parallelization.api.omp.OMP_WaitForFlag

/// IR_SetupCommunication

// TODO: refactor
object IR_SetupCommunication extends DefaultStrategy("Set up communication") {
  var commFunctions = IR_CommunicationFunctions()
  var addedFunctions : ListBuffer[String] = ListBuffer()

  var firstCall = true
  var condCounter = 0

  var collector = new StackCollector
  this.register(collector)

  override def apply(node : Option[Node] = None) = {
    if (firstCall) {
      commFunctions = StateManager.findFirst[IR_CommunicationFunctions]().get
      addedFunctions.clear

      if (Knowledge.mpi_enabled && Knowledge.domain_canHaveRemoteNeighs)
        commFunctions.functions += MPI_WaitForRequest
      if (Knowledge.omp_enabled && Knowledge.domain_canHaveLocalNeighs)
        commFunctions.functions += OMP_WaitForFlag
    }

    super.apply(node)

    firstCall = false
  }

  this += new Transformation("Adding and linking communication functions", {
    case loop : IR_LoopOverPoints if firstCall => // skip communication statements in loops -> to be handled after resolving loops
      loop

    case communicateStatement : IR_Communicate =>
      val numDims = communicateStatement.field.field.fieldLayout.numDimsData

      var commDup = false
      var dupBegin = IR_ExpressionIndex(Array.fill(numDims)(0))
      var dupEnd = IR_ExpressionIndex(Array.fill(numDims)(0))
      var commGhost = false
      var ghostBegin = IR_ExpressionIndex(Array.fill(numDims)(0))
      var ghostEnd = IR_ExpressionIndex(Array.fill(numDims)(0))

      val cond = communicateStatement.condition

      var insideFragLoop = collector.stack.map(_.isInstanceOf[IR_LoopOverFragments]).reduce(_ || _)
      if (insideFragLoop && !Knowledge.experimental_allowCommInFragLoops) {
        Logger.warn("Found communication inside fragment loop; this is currently unsupported")
        insideFragLoop = false
      }

      // TODO: currently assumes numXXXRight == numXXXLeft
      if (communicateStatement.targets.exists(t => "all" == t.target)) {
        val target = communicateStatement.targets.find(t => "all" == t.target).get
        commDup = true
        dupBegin = target.begin.getOrElse(IR_ExpressionIndex(Array.fill(numDims)(0)))
        dupEnd = target.end.getOrElse(IR_ExpressionIndex((0 until numDims).toArray.map(dim => communicateStatement.field.fieldLayout(dim).numDupLayersLeft)))
        commGhost = true
        ghostBegin = target.begin.getOrElse(IR_ExpressionIndex(Array.fill(numDims)(0)))
        ghostEnd = target.end.getOrElse(IR_ExpressionIndex((0 until numDims).toArray.map(dim => communicateStatement.field.fieldLayout(dim).numGhostLayersLeft)))
      }
      if (communicateStatement.targets.exists(t => "dup" == t.target)) {
        val target = communicateStatement.targets.find(t => "dup" == t.target).get
        commDup = true
        dupBegin = target.begin.getOrElse(IR_ExpressionIndex(Array.fill(numDims)(0)))
        dupEnd = target.end.getOrElse(IR_ExpressionIndex((0 until numDims).toArray.map(dim => communicateStatement.field.fieldLayout(dim).numDupLayersLeft)))
      }
      if (communicateStatement.targets.exists(t => "ghost" == t.target)) {
        val target = communicateStatement.targets.find(t => "ghost" == t.target).get
        commGhost = true
        ghostBegin = target.begin.getOrElse(IR_ExpressionIndex(Array.fill(numDims)(0)))
        ghostEnd = target.end.getOrElse(IR_ExpressionIndex((0 until numDims).toArray.map(dim => communicateStatement.field.fieldLayout(dim).numGhostLayersLeft)))
      }

      var functionName = communicateStatement.op match {
        case "begin"  => s"beginExch${ communicateStatement.field.codeName }"
        case "finish" => s"finishExch${ communicateStatement.field.codeName }"
        case "both"   => s"exch${ communicateStatement.field.codeName }"
      }
      functionName += s"_${ communicateStatement.field.arrayIndex.getOrElse("a") }_" +
        communicateStatement.targets.map(t => s"${ t.target }_${
          val begin : IR_ExpressionIndex = t.begin.getOrElse(IR_ExpressionIndex(Array.fill(numDims)("a" : IR_Expression)))
          (0 until numDims).toArray.map(dim => begin(dim).prettyprint).mkString("_")
        }_${
          val end : IR_ExpressionIndex = t.end.getOrElse(IR_ExpressionIndex(Array.fill(numDims)("a" : IR_Expression)))
          (0 until numDims).toArray.map(dim => end(dim).prettyprint).mkString("_")
        }").mkString("_")
      if (insideFragLoop)
        functionName += "_ifl"
      if (cond.isDefined) {
        // TODO: summarize communicate statements with identical conditions (and targets ofc)
        functionName += s"_c$condCounter"
        condCounter += 1
      }

      if (!addedFunctions.contains(functionName)) {
        addedFunctions += functionName
        val fieldSelection = Duplicate(communicateStatement.field)
        fieldSelection.slot = "slot"
        commFunctions.functions += IR_CommunicateFunction(functionName,
          fieldSelection, Fragment.neighbors,
          "begin" == communicateStatement.op || "both" == communicateStatement.op,
          "finish" == communicateStatement.op || "both" == communicateStatement.op,
          commDup, dupBegin, dupEnd,
          commGhost, ghostBegin, ghostEnd,
          insideFragLoop,
          cond)
      }

      communicateStatement.field.slot match {
        case IR_SlotAccess(slot, _) if IR_StringLiteral(IR_LoopOverFragments.defIt) == slot.fragmentIdx => slot.fragmentIdx = 0
        case _                                                                                          =>
      }

      var fctArgs : ListBuffer[IR_Expression] = ListBuffer()
      fctArgs += communicateStatement.field.slot
      if (insideFragLoop)
        fctArgs += IR_LoopOverFragments.defIt

      IR_FunctionCall(functionName, fctArgs) : IR_Statement

    case applyBCsStatement : IR_ApplyBC =>
      var insideFragLoop = collector.stack.map(_.isInstanceOf[IR_LoopOverFragments]).reduce(_ || _)
      if (insideFragLoop && !Knowledge.experimental_allowCommInFragLoops) {
        Logger.warn("Found apply BCs inside fragment loop; this is currently unsupported")
        insideFragLoop = false
      }

      var functionName = s"applyBCs${ applyBCsStatement.field.codeName }"
      if (insideFragLoop) functionName += "_ifl"

      if (!addedFunctions.contains(functionName)) {
        addedFunctions += functionName
        val fieldSelection = Duplicate(applyBCsStatement.field)
        fieldSelection.slot = "slot"
        commFunctions.functions += IR_ApplyBCFunction(functionName, fieldSelection, Fragment.neighbors, insideFragLoop)
      }

      applyBCsStatement.field.slot match {
        case IR_SlotAccess(slot, _) if IR_StringLiteral(IR_LoopOverFragments.defIt) == slot.fragmentIdx => slot.fragmentIdx = 0
        case _                                                                                          =>
      }

      var fctArgs : ListBuffer[IR_Expression] = ListBuffer()
      fctArgs += applyBCsStatement.field.slot
      if (insideFragLoop)
        fctArgs += IR_LoopOverFragments.defIt

      IR_FunctionCall(functionName, fctArgs) : IR_Statement
  }, false)
}
