package exastencils.waLBerla.ir.communication

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.communication.ir.IR_CommunicateFunction
import exastencils.communication.ir.IR_CommunicationFunctions
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.timing.ir.IR_StartTimer
import exastencils.timing.ir.IR_StopTimer
import exastencils.waLBerla.ir.cuda.CUDA_WaLBerlaGPUCommScheme
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaCollection
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaLeveledFunction

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

  this += Transformation("Determine communicate funcs for waLBerla fields", {
    case commFuncs : IR_CommunicationFunctions =>
      commFuncs.functions foreach {
        case comm : IR_CommunicateFunction if IR_WaLBerlaFieldCollection.exists(comm.field.name, comm.field.level)      =>
          funcsToMove += comm
        case _                                                                                                          =>
      }

      commFuncs
  })

  this += Transformation("Convert to waLBerla func", {
    case comm : IR_CommunicateFunction if funcsToMove.contains(comm) =>
      // replace body of exa's communicate function with 'communicate()' member fct of waLBerla's comm scheme and inline
      val genFct = comm.generateFct()
      val field = comm.field
      val commSchemeGPU = CUDA_WaLBerlaGPUCommScheme(IR_WaLBerlaFieldCollection.getByIdentifier(field.name, field.level).get, comm.slot)
      val commSchemeCPU = IR_WaLBerlaCPUCommScheme(IR_WaLBerlaFieldCollection.getByIdentifier(field.name, field.level).get, comm.slot)

      val cond : IR_Expression = Knowledge.cuda_preferredExecution match {
        case "Host"        => IR_BooleanConstant(true) // CPU by default
        case "Device"      => IR_BooleanConstant(false) // GPU by default
        case "Performance" => IR_BooleanConstant(true) // no estimates available -> CPU default
        case "Condition"   => Knowledge.cuda_executionCondition
      }
      val body = ListBuffer[IR_Statement](IR_IfCondition(cond,
        commSchemeCPU.communicate(),
        commSchemeGPU.communicate()))

      if (comm.timer.isDefined) {
        body.prepend(IR_FunctionCall(IR_StartTimer().name, comm.timer.get))
        body.append(IR_FunctionCall(IR_StopTimer().name, comm.timer.get))
      }

      IR_WaLBerlaCollection.get.functions += IR_WaLBerlaLeveledFunction(comm.name, comm.level, genFct.datatype, genFct.parameters, body)
      None // consume
  })
}
