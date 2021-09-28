package exastencils.visualization.ir.visit

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.field.ir.IR_FieldCollection
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_AtFaceCenter
import exastencils.grid.ir.IR_AtNode

object IR_VisItUtil {

  // global declarations to make variable access easier
  val simDoneDecl = IR_VariableDeclaration(IR_BooleanDatatype, "sim_done", IR_BooleanConstant(false))
  val simDone = IR_VariableAccess(simDoneDecl)

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

  def localizationFromCoords(coords : String) = coords.drop(6) // drop "coords" at start

  // coordinate arrays to create the rectilinear mesh(coordsNode for node/cell variables, coordsFace for face centered variables)
  val coordsNodeDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsNode")
  val coordsZoneDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsCell")
  val coordsFaceXDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsFace_x")
  val coordsFaceYDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsFace_y")
  val coordsFaceZDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsFace_z")
  val coordsFaceAsVec : Array[IR_VariableDeclaration] = Array[IR_VariableDeclaration](coordsFaceXDecl, coordsFaceYDecl, coordsFaceZDecl)

  // coordinate arrays to create the 3d curvilinear mesh for 2d variables and 2d curvilinear variable consisting of 1d variables
  // requires special handling for cell-based variables since they are not placed on a mesh's zone anymore
  val curveCoordsNodeDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsNode")
  val curveCoordsZoneDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsCell")
  val curveCoordsFaceXDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsFace_x")
  val curveCoordsFaceYDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsFace_y")
  val curveCoordsFaceZDecl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsFace_z")
  val curveCoordsFaceAsVec : Array[IR_VariableDeclaration] = Array[IR_VariableDeclaration](curveCoordsFaceXDecl, curveCoordsFaceYDecl, curveCoordsFaceZDecl)

  val coordsArrays : ArrayBuffer[IR_VariableDeclaration] = ArrayBuffer[IR_VariableDeclaration]()
  val isNodalInDim : ArrayBuffer[Array[Int]] = ArrayBuffer[Array[Int]]()

  val curveCoordsArrays : ArrayBuffer[IR_VariableDeclaration] = ArrayBuffer[IR_VariableDeclaration]()
  val isNodalInDimCurve : ArrayBuffer[Array[Int]] = ArrayBuffer[Array[Int]]()

  // get variable localizations for rectilinear and curvilinear meshes
  for (field <- IR_FieldCollection.objects) {
    field.layout.localization match {
      case IR_AtNode              =>
        if (!coordsArrays.contains(coordsNodeDecl)) {
          coordsArrays += coordsNodeDecl
          isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(1)

          curveCoordsArrays += curveCoordsNodeDecl
          isNodalInDimCurve += Array.fill[Int](Knowledge.dimensionality)(1)
        }
      case IR_AtCellCenter        =>
        if (!coordsArrays.contains(coordsZoneDecl)) {
          coordsArrays += coordsZoneDecl
          isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(1)

          curveCoordsArrays += curveCoordsZoneDecl
          isNodalInDimCurve += Array.fill[Int](Knowledge.dimensionality)(0)
        }
      case face : IR_AtFaceCenter =>
        if (!coordsArrays.contains(coordsFaceAsVec(face.dim))) {
          coordsArrays += coordsFaceAsVec(face.dim)
          isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(0).updated(face.dim, 1)

          curveCoordsArrays += curveCoordsFaceAsVec(face.dim)
          isNodalInDimCurve += Array.fill[Int](Knowledge.dimensionality)(0).updated(face.dim, 1)
        }
    }
  }

  def stringEquals(value : IR_Expression, str : String) =
    IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), value, IR_StringConstant(str)) EqEq IR_IntegerConstant(0)
}

trait IR_FuturePlainVisItFunction extends IR_FuturePlainFunction {

  allowInlining = false

  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint()
}
