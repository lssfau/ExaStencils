package exastencils.visualization.ir.interactive.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.config.Knowledge
import exastencils.field.ir.IR_FieldCollection

object IR_VisItGlobals {
  val simDoneDecl = IR_VariableDeclaration(IR_BooleanDatatype, "sim_done", IR_BooleanConstant(false))
  val simDone = IR_VariableAccess(simDoneDecl)

  val curSlotDecl = IR_VariableDeclaration(IR_IntegerDatatype, "cur_slot", 0)
  val curSlot = IR_VariableAccess(curSlotDecl)

  val curLevelDecl = IR_VariableDeclaration(IR_IntegerDatatype, "cur_level", Knowledge.maxLevel)
  val curLevel = IR_VariableAccess(curLevelDecl)

  val simTimeDecl = IR_VariableDeclaration(IR_DoubleDatatype, "sim_time", IR_DoubleConstant(0.0))
  val simTime = IR_VariableAccess(simTimeDecl)

  val simCycleDecl = IR_VariableDeclaration(IR_IntegerDatatype, "sim_cycle", IR_IntegerConstant(0))
  val simCycle = IR_VariableAccess(simCycleDecl)

  val runModeDecl = IR_VariableDeclaration(IR_BooleanDatatype, "visit_runMode", IR_BooleanConstant(false))
  val runMode = IR_VariableAccess(runModeDecl)

  val updatePlotsDecl = IR_VariableDeclaration(IR_BooleanDatatype, "visit_updatePlots", IR_BooleanConstant(true))
  val updatePlots = IR_VariableAccess(updatePlotsDecl)

  val scaleCurvemeshDecl = IR_VariableDeclaration(IR_RealDatatype, "scale", IR_RealConstant(1.0))
  val scaleCurvemesh = IR_VariableAccess(scaleCurvemeshDecl)

  val isMultiLeveled = Knowledge.numLevels > 1 && IR_FieldCollection.objects.map(_.level).distinct.size > 1
  val isMultiSlotted = IR_FieldCollection.objects.exists(_.numSlots > 1)

  val nullptr = IR_VariableAccess("nullptr", IR_UnknownDatatype)

  val visitOkay = IR_VariableAccess("VISIT_OKAY", IR_UnknownDatatype)

  val visitHandle = IR_SpecialDatatype("visit_handle")
  val visitInvalidHandle = IR_VariableAccess("VISIT_INVALID_HANDLE", visitHandle)

  var visitCommands = ListBuffer("step", "stop", "run", "toggle updates")
  if (isMultiLeveled)
    visitCommands += "toggle level"
  if (isMultiSlotted)
    visitCommands += "toggle slot"

  val commandNames = IR_VariableAccess("commandNames", IR_ArrayDatatype(IR_PointerDatatype(IR_CharDatatype), visitCommands.length))
  val commandNamesDecl = IR_VariableDeclaration(commandNames, new IR_InitializerList(visitCommands.map(s => IR_StringConstant(s) : IR_Expression)))
}
