package exastencils.visualization.ir.interactive.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.OutputType
import exastencils.field.ir.IR_FieldCollection
import exastencils.visualization.ir.interactive.visit.IR_VisItGlobals._
import exastencils.visualization.ir.interactive.visit.IR_VisItUtil._

case class IR_VisItCommandHandling(cmd : IR_Expression) extends IR_Statement with IR_Expandable {

  def step() = {
    IR_IfCondition(
      stringEquals(cmd, "step"),
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_LeveledInternalFunctionReference("simulate_timestep", Knowledge.maxLevel, IR_UnitDatatype)),
        IR_IfCondition(
          callExtFunction("VisItIsConnected"),
          ListBuffer[IR_Statement](
            IR_IfCondition(
              updatePlots,
              ListBuffer[IR_Statement](
                callExtFunction("VisItTimeStepChanged"),
                callExtFunction("VisItUpdatePlots"))))))
    )
  }

  def stop() = {
    IR_IfCondition(stringEquals(cmd, "stop"),
      IR_Assignment(runMode, IR_BooleanConstant(false)))
  }

  def run() = {
    IR_IfCondition(stringEquals(cmd, "run"),
      IR_Assignment(runMode, IR_BooleanConstant(true)))
  }

  def toggleUpdates() = {
    IR_IfCondition(stringEquals(cmd, "toggle updates"),
      IR_Assignment(updatePlots, IR_Negation(updatePlots)))
  }

  def toggleLevel() = {
    IR_IfCondition(
      stringEquals(cmd, "toggle level"),
      ListBuffer[IR_Statement](
        IR_Assignment(curLevel, modulo(curLevel - Knowledge.minLevel - 1, Knowledge.numLevels) + Knowledge.minLevel),
        IR_IfCondition(
          callExtFunction("VisItIsConnected"),
          ListBuffer[IR_Statement](
            callExtFunction("VisItTimeStepChanged"),
            callExtFunction("VisItUpdatePlots")))))
  }

  def toggleSlot() = {
    IR_IfCondition(
      stringEquals(cmd, "toggle slot"),
      ListBuffer[IR_Statement](
        IR_Assignment(curSlot, (curSlot + 1) Mod IR_FieldCollection.objects.map(_.numSlots).max),
        IR_IfCondition(
          callExtFunction("VisItIsConnected"),
          ListBuffer[IR_Statement](
            callExtFunction("VisItTimeStepChanged"),
            callExtFunction("VisItUpdatePlots")))))
  }

  override def expand() : OutputType = {
    var stmts = ListBuffer[IR_Statement](step(), stop(), run(), toggleUpdates())

    // only register level switches when necessary
    if (isMultiLeveled)
      stmts += toggleLevel()

    // only register slot switch when necessary
    if (isMultiSlotted)
      stmts += toggleSlot()

    stmts
  }
}
