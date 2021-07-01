package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.boundary.ir.IR_ApplyBCFunction
import exastencils.communication.ir.IR_CommunicationFunctions
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation

object IR_WaLBerlaSetupCommunication extends DefaultStrategy("Communication handling for waLBerla fields") {
  var funcsToMove : ListBuffer[IR_ApplyBCFunction] = ListBuffer()

  override def apply(applyAtNode : Option[Node]) : Unit = {
    funcsToMove.clear()
    super.apply(applyAtNode)
  }

  override def applyStandalone(node : Node) : Unit = {
    funcsToMove.clear()
    super.applyStandalone(node)
  }

  this += Transformation("Determine apply bc funcs on waLBerla fields", {
    case commFuncs : IR_CommunicationFunctions =>
      commFuncs.functions foreach {
        case applyBC : IR_ApplyBCFunction if IR_WaLBerlaFieldCollection.exists(applyBC.field.name, applyBC.field.level) =>
          funcsToMove += applyBC
        case _                                                                                                          =>
      }

      commFuncs
  })

  this += Transformation("Convert to waLBerla func", {
    case applyBC : IR_ApplyBCFunction if funcsToMove.contains(applyBC) =>
      val genFct = applyBC.generateFct()
      IR_WaLBerlaCollection.get.functions += IR_WaLBerlaLeveledFunction(applyBC.name, applyBC.level, genFct.datatype, genFct.parameters, genFct.body)
      None
  })
}
