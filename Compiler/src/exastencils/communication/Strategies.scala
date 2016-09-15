package exastencils.communication

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core._
import exastencils.core.collectors.StackCollector
import exastencils.data._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.mpi._
import exastencils.omp._

object SetupCommunication extends DefaultStrategy("Setting up communication") {
  var commFunctions : CommunicationFunctions = CommunicationFunctions()
  var addedFunctions : ListBuffer[String] = ListBuffer()

  var firstCall = true
  var condCounter = 0

  var collector = new StackCollector
  this.register(collector)

  override def apply(node : Option[Node] = None) = {
    if (firstCall) {
      commFunctions = StateManager.findFirst[CommunicationFunctions]().get
      addedFunctions.clear

      if (Knowledge.mpi_enabled && Knowledge.domain_canHaveRemoteNeighs)
        commFunctions.functions += new MPI_WaitForRequest
      if (Knowledge.omp_enabled && Knowledge.domain_canHaveLocalNeighs)
        commFunctions.functions += new OMP_WaitForFlag
      if (Knowledge.domain_canHaveLocalNeighs)
        commFunctions.functions += new ConnectLocalElement()
      if (Knowledge.domain_canHaveRemoteNeighs)
        commFunctions.functions += new ConnectRemoteElement()
    }

    super.apply(node)

    firstCall = false
  }

  this += new Transformation("Adding and linking communication functions", {
    case loop : IR_LoopOverPoints if firstCall => // skip communication statements in loops -> to be handled after resolving loops
      loop

    case communicateStatement : CommunicateStatement => {
      val numDims = communicateStatement.field.field.fieldLayout.numDimsData

      var commDup = false;
      var dupBegin = IR_ExpressionIndex(Array.fill(numDims)(0));
      var dupEnd = IR_ExpressionIndex(Array.fill(numDims)(0))
      var commGhost = false
      var ghostBegin = IR_ExpressionIndex(Array.fill(numDims)(0));
      var ghostEnd = IR_ExpressionIndex(Array.fill(numDims)(0))

      var cond = communicateStatement.condition

      var insideFragLoop = collector.stack.map(node => node match { case loop : IR_LoopOverFragments => true; case _ => false }).reduce((left, right) => left || right)
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

      var functionName = (if (Knowledge.experimental_useLevelIndepFcts)
        communicateStatement.op match {
          case "begin"  => s"beginExch${ communicateStatement.field.field.identifier }"
          case "finish" => s"finishExch${ communicateStatement.field.field.identifier }"
          case "both"   => s"exch${ communicateStatement.field.field.identifier }"
        }
      else communicateStatement.op match {
        case "begin"  => s"beginExch${ communicateStatement.field.codeName }"
        case "finish" => s"finishExch${ communicateStatement.field.codeName }"
        case "both"   => s"exch${ communicateStatement.field.codeName }"
      })
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
        var fieldSelection = Duplicate(communicateStatement.field)
        fieldSelection.slot = "slot"
        commFunctions.functions += ExchangeDataFunction(functionName,
          fieldSelection, Fragment.neighbors,
          "begin" == communicateStatement.op || "both" == communicateStatement.op,
          "finish" == communicateStatement.op || "both" == communicateStatement.op,
          commDup, dupBegin, dupEnd,
          commGhost, ghostBegin, ghostEnd,
          insideFragLoop,
          cond)
      }

      communicateStatement.field.slot match {
        case SlotAccess(slot, _) if IR_StringLiteral(IR_LoopOverFragments.defIt) == slot.fragmentIdx => slot.fragmentIdx = 0
        case _                                                                                       =>
      }

      var fctArgs : ListBuffer[IR_Expression] = ListBuffer()
      fctArgs += communicateStatement.field.slot
      if (Knowledge.experimental_useLevelIndepFcts)
        fctArgs += communicateStatement.field.level
      if (insideFragLoop)
        fctArgs += IR_LoopOverFragments.defIt

      IR_FunctionCall(functionName, fctArgs) : IR_Statement
    }

    case applyBCsStatement : ApplyBCsStatement => {
      var insideFragLoop = collector.stack.map(node => node match { case loop : IR_LoopOverFragments => true; case _ => false }).reduce((left, right) => left || right)
      if (insideFragLoop && !Knowledge.experimental_allowCommInFragLoops) {
        Logger.warn("Found apply BCs inside fragment loop; this is currently unsupported")
        insideFragLoop = false
      }

      var functionName = (if (Knowledge.experimental_useLevelIndepFcts)
        s"applyBCs${ applyBCsStatement.field.field.identifier }"
      else
        s"applyBCs${ applyBCsStatement.field.codeName }")
      if (insideFragLoop) functionName += "_ifl"

      if (!addedFunctions.contains(functionName)) {
        addedFunctions += functionName
        var fieldSelection = Duplicate(applyBCsStatement.field)
        fieldSelection.slot = "slot"
        commFunctions.functions += ApplyBCsFunction(functionName, fieldSelection, Fragment.neighbors, insideFragLoop)
      }

      applyBCsStatement.field.slot match {
        case SlotAccess(slot, _) if IR_StringLiteral(IR_LoopOverFragments.defIt) == slot.fragmentIdx => slot.fragmentIdx = 0
        case _                                                                                       =>
      }

      var fctArgs : ListBuffer[IR_Expression] = ListBuffer()
      fctArgs += applyBCsStatement.field.slot
      if (Knowledge.experimental_useLevelIndepFcts)
        fctArgs += applyBCsStatement.field.level
      if (insideFragLoop)
        fctArgs += IR_LoopOverFragments.defIt

      IR_FunctionCall(functionName, fctArgs) : IR_Statement
    }
  }, false)
}

object MergeCommunicatesAndLoops extends DefaultStrategy("Merging communicate statements with loop nodes") {
  def processFctBody(body : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = {
    if (body.length < 2) return body

    var newBody = ListBuffer[IR_Statement]()

    for (i <- 1 until body.length) { // check for pre communications steps
      (body(i - 1), body(i)) match {
        case (cs : CommunicateStatement, loop : IR_LoopOverPoints) if cs.field.field.level == loop.field.level => // skip intergrid ops for now
          loop.preComms += cs // already merged: newBody += cs
        case (first, second)                                                                                   => newBody += first
      }
    }
    newBody += body.last

    if (newBody.length == body.length) { // nothing changed -> look for post communications steps
      newBody.clear
      for (i <- body.length - 1 until 0 by -1) {
        (body(i - 1), body(i)) match {
          case (loop : IR_LoopOverPoints, cs : CommunicateStatement) if cs.field.field.level == loop.field.level => // skip intergrid ops for now
            loop.postComms += cs // already merged: newBody += cs
          case (first, second)                                                                                   => newBody.prepend(second)
        }
      }
      newBody.prepend(body.head)
    }

    if (newBody.length != body.length)
      processFctBody(newBody) // sth changed -> apply recursively to support multiple communicate statements
    else
      newBody // nothing changed -> work is done
  }

  this += new Transformation("Resolving", {
    case fct : IR_Function => {
      fct.body = processFctBody(fct.body)
      fct
    }
  })
}
