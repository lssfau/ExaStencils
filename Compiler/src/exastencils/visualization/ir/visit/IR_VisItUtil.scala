package exastencils.visualization.ir.visit

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.field.ir.IR_FieldCollection

object IR_VisItUtil {

  // global declarations to make variable access easier
  val simDoneDecl = IR_VariableDeclaration(IR_BooleanDatatype, "sim_done", IR_BooleanConstant(false))
  val curLevelDecl = IR_VariableDeclaration(IR_IntegerDatatype, "cur_level", Knowledge.maxLevel)

  val simTimeDecl = IR_VariableDeclaration(IR_DoubleDatatype, "sim_time", IR_DoubleConstant(0.0))
  val simCycleDecl = IR_VariableDeclaration(IR_IntegerDatatype, "sim_cycle", IR_IntegerConstant(0))

  val runModeDecl = IR_VariableDeclaration(IR_BooleanDatatype, "visit_runMode", IR_BooleanConstant(false))
  val updatePlotsDecl = IR_VariableDeclaration(IR_BooleanDatatype, "visit_updatePlots", IR_BooleanConstant(true))

  val scaleCurvemesh = IR_VariableDeclaration(IR_RealDatatype, "scale", IR_RealConstant(1.0))

  val isMultiLeveled = IR_FieldCollection.objects.map(_.level).distinct.size > 1
  val nullptr = IR_VariableAccess("nullptr", IR_UnknownDatatype)

  val visitOkay = IR_VariableAccess("VISIT_OKAY", IR_UnknownDatatype)

  val visitHandle = IR_SpecialDatatype("visit_handle")
  val visitInvalidHandle = IR_VariableAccess("VISIT_INVALID_HANDLE", visitHandle)

  var visitCommands = ListBuffer("step", "stop", "run", "switchUpdates")
  if (isMultiLeveled) {
    visitCommands ++= ListBuffer("level down", "level up")
  }

  val commandNames = IR_VariableAccess("commandNames", IR_ArrayDatatype(IR_PointerDatatype(IR_CharDatatype), visitCommands.length))
  val commandNamesDecl = IR_VariableDeclaration(commandNames, new IR_InitializerList(visitCommands.map(s => IR_StringConstant(s) : IR_Expression)))

  var innerDatatype : IR_Datatype = IR_PointerDatatype(IR_RealDatatype)
  if (Knowledge.dimensionality > 1) innerDatatype = IR_ArrayDatatype(innerDatatype, Knowledge.dimensionality)
  if (Knowledge.numLevels > 1) innerDatatype = IR_ArrayDatatype(innerDatatype, Knowledge.numLevels)

  // coordinate arrays to create the rectilinear mesh(coordsNode for node/cell variables, coordsFace for face centered variables)
  val coordsNodeDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsNode")
  val coordsZoneDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsZone")
  val coordsFaceXDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsFace_x")
  val coordsFaceYDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsFace_y")
  val coordsFaceZDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsFace_z")
  val coordsFaceAsVec : Array[IR_VariableDeclaration] = Array[IR_VariableDeclaration](coordsFaceXDecl, coordsFaceYDecl, coordsFaceZDecl)

  // coordinate arrays to create the 3d curvilinear mesh for 2d variables and 2d curvilinear variable consisting of 1d variables
  // requires special handling for cell-based variables since they are not placed on a mesh's zone anymore
  val curveCoordsNodeDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsNode")
  val curveCoordsZoneDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsZone")
  val curveCoordsFaceXDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsFace_x")
  val curveCoordsFaceYDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsFace_y")
  val curveCoordsFaceZDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsFace_z")
  val curveCoordsFaceAsVec : Array[IR_VariableDeclaration] = Array[IR_VariableDeclaration](curveCoordsFaceXDecl, curveCoordsFaceYDecl, curveCoordsFaceZDecl)

  val coordsArrays : ArrayBuffer[IR_VariableDeclaration] = ArrayBuffer[IR_VariableDeclaration]()
  val isNodalInDim : ArrayBuffer[Array[Int]] = ArrayBuffer[Array[Int]]()

  val curveCoordsArrays : ArrayBuffer[IR_VariableDeclaration] = ArrayBuffer[IR_VariableDeclaration]()
  val isNodalInDimCurve : ArrayBuffer[Array[Int]] = ArrayBuffer[Array[Int]]()
}
