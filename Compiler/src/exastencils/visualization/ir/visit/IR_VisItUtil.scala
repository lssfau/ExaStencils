package exastencils.visualization.ir.visit

import scala.collection.mutable.ArrayBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldCollection
import exastencils.grid.ir._
import exastencils.visualization.ir.visit.IR_VisItGlobals.curSlot

object IR_VisItUtil {

  /* helper functions */

  def useCurveMesh() = Knowledge.dimensionality < 3

  def useRectMesh() = Knowledge.dimensionality > 1

  def stringEquals(value : IR_Expression, str : String) =
    IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), value, IR_StringConstant(str)) EqEq IR_IntegerConstant(0)

  def modulo(x : IR_Expression, N : IR_Expression) = {
    val remainder = x Mod N
    IR_TernaryCondition(remainder < 0, remainder + N, remainder)
  }

  def meshname(coords : IR_VariableAccess) = s"rect${ Knowledge.dimensionality }d_${ coords.name }"
  def meshname(field : IR_Field) = s"rect${ Knowledge.dimensionality }d_${ meshCoordsForLocalization(field.localization).name }"

  def curvname(numDims : Int, fieldname : String) = "curv" + (numDims + 1) + "d_" + fieldname

  def meshCoordsForLocalization(loc : IR_Localization) = loc match {
    // TODO: adapt if multiple mesh coords exist
    case _ => meshCoords
  }

  def callExtFunction(name : String, args : IR_Expression*) =
    IR_FunctionCall(IR_ExternalFunctionReference(name), args : _*)

  // determine whether simulation or VisIt is responsible for freeing
  def ownership(dataIsCopied : Boolean) = IR_VariableAccess("VISIT_OWNER_" + (if (dataIsCopied) "VISIT" else "SIM"), IR_UnknownDatatype)

  // determine whether doubles or floats are sent
  def setVariableDataFunc = IR_ExternalFunctionReference("VisIt_VariableData_setData" + (if (Knowledge.useDblPrecision) "D" else "F"))

  // cap curSlot to max available slot
  def getAndCapSlot(field : IR_Field) = IR_Maximum(curSlot, field.numSlots - 1)

  /* mesh coords */

  // determine data type of mesh coords array
  var innerDatatype : IR_Datatype = IR_PointerDatatype(IR_RealDatatype)
  if (Knowledge.dimensionality > 1) innerDatatype = IR_ArrayDatatype(innerDatatype, Knowledge.dimensionality)
  if (Knowledge.numLevels > 1) innerDatatype = IR_ArrayDatatype(innerDatatype, Knowledge.numLevels)

  // coordinate array to create the rectilinear mesh
  val meshCoords = IR_VariableAccess("meshCoords", innerDatatype)

  // coordinate arrays to create the 3d curvilinear mesh for 2d variables and 2d curvilinear variable consisting of 1d variables
  // requires special handling for cell-based variables since they are not placed on a mesh's zone anymore
  val curveCoordsNode = IR_VariableAccess("curve_coordsNode", innerDatatype)
  val curveCoordsZone = IR_VariableAccess("curve_coordsCell", innerDatatype)
  val curveCoordsFaceX = IR_VariableAccess("curve_coordsFace_x", innerDatatype)
  val curveCoordsFaceY = IR_VariableAccess("curve_coordsFace_y", innerDatatype)
  val curveCoordsFaceZ = IR_VariableAccess("curve_coordsFace_z", innerDatatype)
  val curveCoordsFaceAsVec = Array[IR_VariableAccess](curveCoordsFaceX, curveCoordsFaceY, curveCoordsFaceZ)

  val coordsArrays : ArrayBuffer[IR_VariableAccess] = ArrayBuffer[IR_VariableAccess]()
  val isNodalInDim : ArrayBuffer[Array[Int]] = ArrayBuffer[Array[Int]]()

  val curveCoordsArrays : ArrayBuffer[IR_VariableAccess] = ArrayBuffer[IR_VariableAccess]()
  val isNodalInDimCurve : ArrayBuffer[Array[Int]] = ArrayBuffer[Array[Int]]()

  // get variable localizations for rectilinear and curvilinear meshes
  for (field <- IR_FieldCollection.objects) {
    field.layout.localization match {
      case IR_AtNode              =>
        if (!coordsArrays.contains(meshCoords)) {
          coordsArrays += meshCoords
          isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(1)
        }

        if (!curveCoordsArrays.contains(curveCoordsNode)) {
          curveCoordsArrays += curveCoordsNode
          isNodalInDimCurve += Array.fill[Int](Knowledge.dimensionality)(1)
        }
      case IR_AtCellCenter        =>
        if (!coordsArrays.contains(meshCoords)) {
          coordsArrays += meshCoords
          isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(1)
        }

        if (!curveCoordsArrays.contains(curveCoordsZone)) {
          curveCoordsArrays += curveCoordsZone
          isNodalInDimCurve += Array.fill[Int](Knowledge.dimensionality)(0)
        }
      case face : IR_AtFaceCenter =>
        if (!coordsArrays.contains(meshCoords)) { // face centered vars are interpolated to cell centered vars
          coordsArrays += meshCoords
          isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(1)
        }

        if (!curveCoordsArrays.contains(curveCoordsFaceAsVec(face.dim))) {
          curveCoordsArrays += curveCoordsFaceAsVec(face.dim)
          isNodalInDimCurve += Array.fill[Int](Knowledge.dimensionality)(0).updated(face.dim, 1)
        }
    }
  }
}
