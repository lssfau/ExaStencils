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
    case commStatement : CommunicateStatement => {
      val functionName = (if (Knowledge.comm_useLevelIndependentFcts)
        commStatement.op match {
        case "begin"  => s"beginExch${commStatement.field.field.identifier}"
        case "finish" => s"finishExch${commStatement.field.field.identifier}"
        case "both"   => s"exch${commStatement.field.field.identifier}"
      }
      else commStatement.op match {
        case "begin"  => s"beginExch${commStatement.field.codeName}"
        case "finish" => s"finishExch${commStatement.field.codeName}"
        case "both"   => s"exch${commStatement.field.codeName}"
      })

      if (!addedFunctions.contains(functionName)) {
        addedFunctions += functionName
        var fieldSelection = Duplicate(commStatement.field)
        fieldSelection.slot = "slot"
        commFunctions.functions += ExchangeDataFunction(fieldSelection, Fragment.neighbors,
          "begin" == commStatement.op || "both" == commStatement.op,
          "finish" == commStatement.op || "both" == commStatement.op)
      }

      commStatement.field.slot match {
        case SlotAccess(slot, _) if StringConstant(LoopOverFragments.defIt) == slot.fragmentIdx => slot.fragmentIdx = 0
        case _ =>
      }

      if (Knowledge.comm_useLevelIndependentFcts)
        (new FunctionCallExpression(functionName, ListBuffer[Expression](commStatement.field.slot, commStatement.field.level))) : Statement
      else
        (new FunctionCallExpression(functionName, commStatement.field.slot)) : Statement
    }
  })
}
