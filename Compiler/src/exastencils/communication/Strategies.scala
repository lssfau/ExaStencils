package exastencils.communication

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.StackCollector
import exastencils.data._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.mpi._

object SetupCommunication extends DefaultStrategy("Setting up communication") {
  var commFunctions : CommunicationFunctions = CommunicationFunctions()
  var addedFunctions : ListBuffer[String] = ListBuffer()

  var collector = new StackCollector
  this.register(collector)

  override def apply(node : Option[Node] = None) = {
    commFunctions = StateManager.findFirst[CommunicationFunctions]().get
    addedFunctions.clear

    if (Knowledge.mpi_enabled && Knowledge.domain_canHaveRemoteNeighs)
      commFunctions.functions += new MPI_WaitForRequest
    if (Knowledge.domain_canHaveLocalNeighs)
      commFunctions.functions += new ConnectLocalElement()
    if (Knowledge.domain_canHaveRemoteNeighs)
      commFunctions.functions += new ConnectRemoteElement()

    super.apply(node)
  }

  this += new Transformation("Adding and linking communication functions", {
    case communicateStatement : CommunicateStatement => {
      var commDup = false;
      var dupBegin = new MultiIndex(Array.fill(Knowledge.dimensionality)(0)); var dupEnd = new MultiIndex(Array.fill(Knowledge.dimensionality)(0))
      var commGhost = false
      var ghostBegin = new MultiIndex(Array.fill(Knowledge.dimensionality)(0)); var ghostEnd = new MultiIndex(Array.fill(Knowledge.dimensionality)(0))

      var insideFragLoop = collector.stack.map(node => node match { case loop : LoopOverFragments => true; case _ => false }).reduce((left, right) => left || right)
      if (insideFragLoop && !Knowledge.experimental_allowCommInFragLoops) {
        Logger.warn("Found communication inside fragment loop; this is currently unsupported")
        insideFragLoop = false
      }

      // TODO: currently assumes numXXXRight == numXXXLeft
      if (communicateStatement.targets.exists(t => "all" == t.target)) {
        val target = communicateStatement.targets.find(t => "all" == t.target).get
        commDup = true
        dupBegin = target.begin.getOrElse(new MultiIndex(Array.fill(Knowledge.dimensionality)(0)))
        dupEnd = target.end.getOrElse(new MultiIndex((0 until Knowledge.dimensionality).toArray.map(dim => communicateStatement.field.fieldLayout(dim).numDupLayersLeft)))
        commGhost = true
        ghostBegin = target.begin.getOrElse(new MultiIndex(Array.fill(Knowledge.dimensionality)(0)))
        ghostEnd = target.end.getOrElse(new MultiIndex((0 until Knowledge.dimensionality).toArray.map(dim => communicateStatement.field.fieldLayout(dim).numGhostLayersLeft)))
      }
      if (communicateStatement.targets.exists(t => "dup" == t.target)) {
        val target = communicateStatement.targets.find(t => "dup" == t.target).get
        commDup = true
        dupBegin = target.begin.getOrElse(new MultiIndex(Array.fill(Knowledge.dimensionality)(0)))
        dupEnd = target.end.getOrElse(new MultiIndex((0 until Knowledge.dimensionality).toArray.map(dim => communicateStatement.field.fieldLayout(dim).numDupLayersLeft)))
      }
      if (communicateStatement.targets.exists(t => "ghost" == t.target)) {
        val target = communicateStatement.targets.find(t => "ghost" == t.target).get
        commGhost = true
        ghostBegin = target.begin.getOrElse(new MultiIndex(Array.fill(Knowledge.dimensionality)(0)))
        ghostEnd = target.end.getOrElse(new MultiIndex((0 until Knowledge.dimensionality).toArray.map(dim => communicateStatement.field.fieldLayout(dim).numGhostLayersLeft)))
      }

      var functionName = (if (Knowledge.experimental_useLevelIndepFcts)
        communicateStatement.op match {
        case "begin"  => s"beginExch${communicateStatement.field.field.identifier}"
        case "finish" => s"finishExch${communicateStatement.field.field.identifier}"
        case "both"   => s"exch${communicateStatement.field.field.identifier}"
      }
      else communicateStatement.op match {
        case "begin"  => s"beginExch${communicateStatement.field.codeName}"
        case "finish" => s"finishExch${communicateStatement.field.codeName}"
        case "both"   => s"exch${communicateStatement.field.codeName}"
      })
      functionName += s"_${communicateStatement.field.arrayIndex.getOrElse("a")}_" +
        communicateStatement.targets.map(t => s"${t.target}_${
          val begin : MultiIndex = t.begin.getOrElse(new MultiIndex(Array.fill(Knowledge.dimensionality)("a" : Expression)))
          (0 until Knowledge.dimensionality).toArray.map(dim => begin(dim).prettyprint).mkString("_")
        }_${
          val end : MultiIndex = t.end.getOrElse(new MultiIndex(Array.fill(Knowledge.dimensionality)("a" : Expression)))
          (0 until Knowledge.dimensionality).toArray.map(dim => end(dim).prettyprint).mkString("_")
        }").mkString("_")
      if (insideFragLoop)
        functionName += "_ifl"

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
          insideFragLoop)
      }

      communicateStatement.field.slot match {
        case SlotAccess(slot, _) if StringConstant(LoopOverFragments.defIt) == slot.fragmentIdx => slot.fragmentIdx = 0
        case _ =>
      }

      var fctArgs : ListBuffer[Expression] = ListBuffer()
      fctArgs += communicateStatement.field.slot
      if (Knowledge.experimental_useLevelIndepFcts)
        fctArgs += communicateStatement.field.level
      if (insideFragLoop)
        fctArgs += LoopOverFragments.defIt

      FunctionCallExpression(functionName, fctArgs) : Statement
    }
    case applyBCsStatement : ApplyBCsStatement => {
      var insideFragLoop = collector.stack.map(node => node match { case loop : LoopOverFragments => true; case _ => false }).reduce((left, right) => left || right)
      if (insideFragLoop && !Knowledge.experimental_allowCommInFragLoops) {
        Logger.warn("Found apply BCs inside fragment loop; this is currently unsupported")
        insideFragLoop = false
      }

      var functionName = if (Knowledge.experimental_useLevelIndepFcts) s"applyBCs${applyBCsStatement.field.field.identifier}" else s"applyBCs${applyBCsStatement.field.codeName}"
      if (insideFragLoop) functionName += "_ifl"

      if (!addedFunctions.contains(functionName)) {
        addedFunctions += functionName
        var fieldSelection = Duplicate(applyBCsStatement.field)
        fieldSelection.slot = "slot"
        commFunctions.functions += ApplyBCsFunction(functionName, fieldSelection, Fragment.neighbors, insideFragLoop)
      }

      applyBCsStatement.field.slot match {
        case SlotAccess(slot, _) if StringConstant(LoopOverFragments.defIt) == slot.fragmentIdx => slot.fragmentIdx = 0
        case _ =>
      }

      var fctArgs : ListBuffer[Expression] = ListBuffer()
      fctArgs += applyBCsStatement.field.slot
      if (Knowledge.experimental_useLevelIndepFcts)
        fctArgs += applyBCsStatement.field.level
      if (insideFragLoop)
        fctArgs += LoopOverFragments.defIt

      FunctionCallExpression(functionName, fctArgs) : Statement
    }
  })
}
