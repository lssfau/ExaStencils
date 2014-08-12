package exastencils.communication

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.primitives._

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
      val functionName = (commStatement.op match {
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

      (new FunctionCallExpression(functionName, commStatement.field.slot)) : Statement
    }
  })
}
