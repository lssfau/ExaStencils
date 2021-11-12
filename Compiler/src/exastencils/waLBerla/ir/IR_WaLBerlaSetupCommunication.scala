package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_FutureFunction
import exastencils.boundary.ir.IR_ApplyBCFunction
import exastencils.communication.ir.IR_CommunicateFunction
import exastencils.communication.ir.IR_CommunicationFunctions
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation

object IR_WaLBerlaSetupCommunication extends DefaultStrategy("Communication handling for waLBerla fields") {
  var funcsToMove : ListBuffer[IR_FutureFunction] = ListBuffer()

  override def apply(applyAtNode : Option[Node]) : Unit = {
    funcsToMove.clear()
    super.apply(applyAtNode)
  }

  override def applyStandalone(node : Node) : Unit = {
    funcsToMove.clear()
    super.applyStandalone(node)
  }

  this += Transformation("Determine apply bc funcs for waLBerla fields", {
    case commFuncs : IR_CommunicationFunctions =>
      commFuncs.functions foreach {
        case applyBC : IR_ApplyBCFunction if IR_WaLBerlaFieldCollection.exists(applyBC.field.name, applyBC.field.level) =>
          funcsToMove += applyBC
        case comm : IR_CommunicateFunction if IR_WaLBerlaFieldCollection.exists(comm.field.name, comm.field.level)      =>
          funcsToMove += comm
        case _                                                                                                          =>
      }

      commFuncs
  })

  this += Transformation("Convert to waLBerla func", {
    case applyBC : IR_ApplyBCFunction if funcsToMove.contains(applyBC) =>
      val genFct = applyBC.generateFct()
      IR_WaLBerlaCollection.get.functions += IR_WaLBerlaLeveledFunction(applyBC.name, applyBC.level, genFct.datatype, genFct.parameters, genFct.body)
      None // consume

    case comm : IR_CommunicateFunction if funcsToMove.contains(comm) =>
      // replace body of exa's communicate function with 'communicate()' member fct of waLBerla's comm scheme and inline
      val genFct = comm.generateFct()
      val field = comm.field
      val body = IR_WaLBerlaCommScheme(IR_WaLBerlaFieldCollection.getByIdentifier(field.name, field.level).get, comm.slot).communicate

      IR_WaLBerlaCollection.get.functions += IR_WaLBerlaLeveledFunction(comm.name, comm.level, genFct.datatype, genFct.parameters, ListBuffer(body))
      None // consume
  })
}
