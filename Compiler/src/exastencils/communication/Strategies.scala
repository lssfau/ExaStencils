package exastencils.communication

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.data._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.mpi._

object SetupCommunication extends DefaultStrategy("Setting up communication") {
  var commFunctions : CommunicationFunctions = CommunicationFunctions()
  var addedFunctions : ListBuffer[String] = ListBuffer()

  override def apply(node : Option[Node] = None) = {
    commFunctions = StateManager.findFirst[CommunicationFunctions]().get
    addedFunctions.clear

    if (Knowledge.useMPI && Knowledge.domain_canHaveRemoteNeighs)
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

      // TODO: currently assumes numXXXRight == numXXXLeft
      if (communicateStatement.targets.exists(t => "all" == t.target)) {
        val target = communicateStatement.targets.find(t => "all" == t.target).get
        commDup = true
        dupBegin = target.begin.getOrElse(new MultiIndex(Array.fill(Knowledge.dimensionality)(0)))
        dupEnd = target.end.getOrElse(new MultiIndex((0 until Knowledge.dimensionality).toArray.map(dim => communicateStatement.field.layout(dim).numDupLayersLeft)))
        commGhost = true
        ghostBegin = target.begin.getOrElse(new MultiIndex(Array.fill(Knowledge.dimensionality)(0)))
        ghostEnd = target.end.getOrElse(new MultiIndex((0 until Knowledge.dimensionality).toArray.map(dim => communicateStatement.field.layout(dim).numGhostLayersLeft)))
      }
      if (communicateStatement.targets.exists(t => "dup" == t.target)) {
        val target = communicateStatement.targets.find(t => "dup" == t.target).get
        commDup = true
        dupBegin = target.begin.getOrElse(new MultiIndex(Array.fill(Knowledge.dimensionality)(0)))
        dupEnd = target.end.getOrElse(new MultiIndex((0 until Knowledge.dimensionality).toArray.map(dim => communicateStatement.field.layout(dim).numDupLayersLeft)))
      }
      if (communicateStatement.targets.exists(t => "ghost" == t.target)) {
        val target = communicateStatement.targets.find(t => "ghost" == t.target).get
        commGhost = true
        ghostBegin = target.begin.getOrElse(new MultiIndex(Array.fill(Knowledge.dimensionality)(0)))
        ghostEnd = target.end.getOrElse(new MultiIndex((0 until Knowledge.dimensionality).toArray.map(dim => communicateStatement.field.layout(dim).numGhostLayersLeft)))
      }

      val functionName = ((if (Knowledge.comm_useLevelIndependentFcts)
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
        + s"_${if (communicateStatement.field.arrayIndex >= 0) communicateStatement.field.arrayIndex else "a"}_"
        + communicateStatement.targets.map(t => s"${t.target}_${
          val begin : MultiIndex = t.begin.getOrElse(new MultiIndex(Array.fill(Knowledge.dimensionality)("a" : Expression)))
          (0 until Knowledge.dimensionality).toArray.map(dim => begin(dim).cpp).mkString("_")
        }_${
          val end : MultiIndex = t.end.getOrElse(new MultiIndex(Array.fill(Knowledge.dimensionality)("a" : Expression)))
          (0 until Knowledge.dimensionality).toArray.map(dim => end(dim).cpp).mkString("_")
        }").mkString("_"))

      if (!addedFunctions.contains(functionName)) {
        addedFunctions += functionName
        var fieldSelection = Duplicate(communicateStatement.field)
        fieldSelection.slot = "slot"
        commFunctions.functions += ExchangeDataFunction(functionName,
          fieldSelection, Fragment.neighbors,
          "begin" == communicateStatement.op || "both" == communicateStatement.op,
          "finish" == communicateStatement.op || "both" == communicateStatement.op,
          commDup, dupBegin, dupEnd,
          commGhost, ghostBegin, ghostEnd)
      }

      communicateStatement.field.slot match {
        case SlotAccess(slot, _) if StringConstant(LoopOverFragments.defIt) == slot.fragmentIdx => slot.fragmentIdx = 0
        case _ =>
      }

      if (Knowledge.comm_useLevelIndependentFcts)
        (new FunctionCallExpression(functionName, ListBuffer[Expression](communicateStatement.field.slot, communicateStatement.field.level))) : Statement
      else
        (new FunctionCallExpression(functionName, communicateStatement.field.slot)) : Statement
    }
    case applyBCsStatement : ApplyBCsStatement => {
      val functionName = if (Knowledge.comm_useLevelIndependentFcts) s"applyBCs${applyBCsStatement.field.field.identifier}" else s"applyBCs${applyBCsStatement.field.codeName}"

      if (!addedFunctions.contains(functionName)) {
        addedFunctions += functionName
        var fieldSelection = Duplicate(applyBCsStatement.field)
        fieldSelection.slot = "slot"
        commFunctions.functions += ApplyBCsFunction(functionName, fieldSelection, Fragment.neighbors)
      }

      applyBCsStatement.field.slot match {
        case SlotAccess(slot, _) if StringConstant(LoopOverFragments.defIt) == slot.fragmentIdx => slot.fragmentIdx = 0
        case _ =>
      }

      if (Knowledge.comm_useLevelIndependentFcts)
        (new FunctionCallExpression(functionName, ListBuffer[Expression](applyBCsStatement.field.slot, applyBCsStatement.field.level))) : Statement
      else
        (new FunctionCallExpression(functionName, applyBCsStatement.field.slot)) : Statement
    }
  })
}
