package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_FunctionLike
import exastencils.boundary.ir.IR_ApplyBCFunction
import exastencils.communication.ir.IR_CommunicationFunctions
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation

object IR_WaLBerlaSetupCommunication extends DefaultStrategy("Communication handling for waLBerla fields") {
  var funcsToMove : ListBuffer[IR_FunctionLike] = ListBuffer()

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
      funcsToMove ++= Duplicate(commFuncs.functions).filter {
        case applyBC : IR_ApplyBCFunction => IR_WaLBerlaFieldCollection.exists(applyBC.field.name, applyBC.field.level)
        case _ => false
      }

      commFuncs
  })

  this += Transformation("Convert to waLBerla func", {
    case applyBC : IR_ApplyBCFunction if funcsToMove.contains(applyBC) =>
      val genFct = applyBC.generateFct()
      IR_WaLBerlaLeveledFunction(applyBC.name, applyBC.level, genFct.datatype, genFct.parameters, genFct.body)
  })

  // TODO remove converted funcs from Communication collection
}
