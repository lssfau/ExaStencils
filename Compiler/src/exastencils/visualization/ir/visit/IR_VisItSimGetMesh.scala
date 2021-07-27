package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_FragmentIndex
import exastencils.field.ir._
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_AtFaceCenter
import exastencils.grid.ir.IR_AtNode

/// IR_VisItSimGetMesh
// provide mesh for VisIt

case class IR_VisItSimGetMesh() extends IR_FuturePlainFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  // get variable localizations for rectilinear and curvilinear meshes
  for (field <- IR_FieldCollection.objects) {
    field.layout.localization match {
      case IR_AtNode if !coords_arrays.contains(coordsNode_decl)                        =>
        coords_arrays += coordsNode_decl
        isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(1)

        curveCoords_arrays += curveCoordsNode_decl
        isNodalInDim_curve += Array.fill[Int](Knowledge.dimensionality)(1)
      case IR_AtCellCenter if !coords_arrays.contains(coordsZone_decl)                  =>
        coords_arrays += coordsZone_decl
        isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(1)

        curveCoords_arrays += curveCoordsZone_decl
        isNodalInDim_curve += Array.fill[Int](Knowledge.dimensionality)(0)
      case face : IR_AtFaceCenter if !coords_arrays.contains(coordsFaceAsVec(face.dim)) =>
        coords_arrays += coordsFaceAsVec(face.dim)
        isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(0).updated(face.dim, 1)

        curveCoords_arrays += curveCoordsFaceAsVec(face.dim)
        isNodalInDim_curve += Array.fill[Int](Knowledge.dimensionality)(0).updated(face.dim, 1)
    }
  }

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()
    val h_decl = IR_VariableDeclaration(visitHandle, "h", visitInvalidHandle)
    val handles_decl = IR_VariableDeclaration(IR_ArrayDatatype(visitHandle, Knowledge.dimensionality), "handles")

    fctBody += h_decl

    // 1d case only produce a curvilinear mesh
    if (Knowledge.dimensionality > 1) {
      for (coords_decl <- coords_arrays) {
        val curCoords = coords_arrays.indexOf(coords_decl)
        for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
          val numPointsDim = (0 until Knowledge.dimensionality).map(d => Knowledge.domain_fragmentLengthAsVec(d) * Knowledge.domain_rect_numFragsPerBlockAsVec(d) * (1 << level) + isNodalInDim(curCoords)(d))

          // coordinate setter function depending on dimensionality
          val handles_accessDim = (0 until Knowledge.dimensionality).map(d => IR_ArrayAccess(IR_VariableAccess(handles_decl), d)).toArray
          val funcCall = if (Knowledge.dimensionality == 2) {
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_RectilinearMesh_setCoordsXY"), IR_VariableAccess(h_decl), handles_accessDim(0), handles_accessDim(1))
          } else {
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_RectilinearMesh_setCoordsXYZ"), IR_VariableAccess(h_decl), handles_accessDim(0), handles_accessDim(1), handles_accessDim(2))
          }

          val ifBody = ListBuffer[IR_Statement]()
          // allocate handles
          ifBody += handles_decl
          for (dim <- 0 until Knowledge.dimensionality) {
            ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableData_alloc"), IR_AddressOf(IR_ArrayAccess(IR_VariableAccess(handles_decl), dim)))
          }

          // determine whether doubles or floats are sent
          val funcRef = if (Knowledge.useDblPrecision) IR_ExternalFunctionReference("VisIt_VariableData_setDataD") else IR_ExternalFunctionReference("VisIt_VariableData_setDataF")

          // pass pointers of coordinate arrays to handles
          for (dim <- 0 until Knowledge.dimensionality) {
            // array access depending on number of levels
            val coords_access = if (Knowledge.numLevels > 1) {
              IR_MultiDimArrayAccess(IR_VariableAccess(coords_decl), IR_ExpressionIndex(Array[IR_Expression](dim, level - Knowledge.minLevel)))
            } else {
              IR_ArrayAccess(IR_VariableAccess(coords_decl), dim)
            }

            ifBody += IR_FunctionCall(funcRef,
              IR_ArrayAccess(IR_VariableAccess(handles_decl), dim),
              IR_Native("VISIT_OWNER_SIM"),
              IR_IntegerConstant(1),
              numPointsDim(dim),
              coords_access
            )
          }
          ifBody += funcCall

          fctBody += IR_IfCondition(
            IR_AndAnd(
              IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("name", IR_StringDatatype), IR_StringConstant("rect" + Knowledge.dimensionality + "d_" + coords_decl.name.drop(6))) EqEq IR_IntegerConstant(0),
              IR_VariableAccess(cur_level_decl) EqEq level
            ),
            ListBuffer[IR_Statement](
              IR_IfCondition(
                IR_FunctionCall(IR_ExternalFunctionReference("VisIt_RectilinearMesh_alloc"), IR_AddressOf(IR_VariableAccess(h_decl))) EqEq visitOkay,
                ifBody
              )
            )
          )
        }
      }
    }

    // curvilinear mesh construction for 1d and 2d problems
    if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) {
      for (field <- IR_FieldCollection.sortedObjects) {
        val numDims = field.layout.numDimsGrid
        val handlesCurve_decl = IR_VariableDeclaration(IR_ArrayDatatype(visitHandle, numDims + 1), "handles")
        val numPointsDim_tmp = (0 until numDims).map(d => field.layout.defIdxDupRightEnd(d) - field.layout.defIdxDupLeftBegin(d)).toArray
        val numPointsDim_field = (0 until numDims).map(d => field.layout.defIdxPadRightEnd(0) - field.layout.defIdxPadLeftBegin(0))
        val numOuterLayersLeft = (0 until numDims).map(d => field.layout.defIdxDupLeftBegin(d) - field.layout.defIdxPadLeftBegin(d)).toArray
        val numOuterLayersRight = (0 until numDims).map(d => field.layout.defIdxPadRightEnd(d) - field.layout.defIdxDupRightEnd(d)).toArray
        val isNodalDim = (0 until numDims).map(d => numPointsDim_tmp(d) % 2)
        val fragOffset = (0 until numDims).map(d => if (Knowledge.domain_rect_numFragsPerBlockAsVec(d) <= 1) IR_IntegerConstant(0)
        else (numPointsDim_tmp(d) - isNodalDim(d)) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(d)))
        val numPointsTotal_tmp = (0 until numDims).map(d => Knowledge.domain_rect_numFragsPerBlockAsVec(d) * (numPointsDim_tmp(d) - isNodalDim(d)) + isNodalDim(d))

        // determine if data must be copied or not
        val dataIsCopied = if (numOuterLayersLeft.sum != 0 || numOuterLayersRight.sum != 0 || Knowledge.domain_numFragmentsPerBlock > 1) true else false

        val ifBody = ListBuffer[IR_Statement]()
        // dimensionality that must be passed -> for last dimension its 1 because we send 1 variable value per mesh point
        val dims = IR_VariableDeclaration(IR_ArrayDatatype(IR_IntegerDatatype, numDims + 1), "dims")
        ifBody += dims
        for (dim <- 0 until numDims) {
          ifBody += IR_Assignment(IR_ArrayAccess(IR_VariableAccess(dims), dim), numPointsTotal_tmp(dim))
        }
        ifBody += IR_Assignment(IR_ArrayAccess(IR_VariableAccess(dims), numDims), 1)

        val funcCall = if (numDims == 1) {
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CurvilinearMesh_setCoordsXY"), IR_VariableAccess(h_decl), IR_VariableAccess(dims), IR_ArrayAccess(IR_VariableAccess(handlesCurve_decl), 0), IR_ArrayAccess(IR_VariableAccess(handlesCurve_decl), 1))
        } else {
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CurvilinearMesh_setCoordsXYZ"), IR_VariableAccess(h_decl), IR_VariableAccess(dims), IR_ArrayAccess(IR_VariableAccess(handlesCurve_decl), 0), IR_ArrayAccess(IR_VariableAccess(handlesCurve_decl), 1), IR_ArrayAccess(IR_VariableAccess(handlesCurve_decl), 2))
        }

        // allocate handles
        ifBody += handlesCurve_decl
        for (dim <- 0 to numDims) {
          ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableData_alloc"), IR_AddressOf(IR_ArrayAccess(IR_VariableAccess(handles_decl), dim)))
        }

        // pass pointers of coordinate arrays to handles
        val loc_name = field.layout.localization.name
        val curveCoords_decl = if (loc_name == "Node") curveCoordsNode_decl else if (loc_name == "Cell") curveCoordsZone_decl else curveCoordsFaceAsVec(loc_name.charAt(loc_name.length - 1).toInt - 'x'.toInt)

        val idx_tmp = if (numDims == 1) {
          IR_FieldIteratorAccess(0) + fragOffset(0)
        } else {
          numPointsTotal_tmp(0) * (IR_FieldIteratorAccess(1) + fragOffset(1)) + IR_FieldIteratorAccess(0) + fragOffset(0)
        }

        val idx_field = if (numDims == 1) {
          IR_FieldIteratorAccess(0)
        } else {
          numPointsDim_field(0) * IR_FieldIteratorAccess(1) + IR_FieldIteratorAccess(0)
        }

        val offsetToInnerPoints = if (numDims == 1) {
          numOuterLayersLeft(0)
        } else {
          numPointsDim_field(0) * numOuterLayersLeft(1) + numOuterLayersLeft(0)
        }

        // copy values to temporary memory (only when necessary)
        val tmp_decl = IR_VariableDeclaration(IR_PointerDatatype(IR_RealDatatype), "tmp")
        if (dataIsCopied) {
          ifBody += tmp_decl
          ifBody += IR_Assignment(
            IR_VariableAccess(tmp_decl),
            IR_Cast(IR_PointerDatatype(IR_RealDatatype), IR_FunctionCall(IR_ExternalFunctionReference("malloc"), numPointsTotal_tmp.product * IR_SizeOf(IR_RealDatatype)))
          )

          ifBody += IR_LoopOverFragments(
            IR_LoopOverDimensions(numDims, IR_ExpressionIndexRange(IR_ExpressionIndex(Array.fill[Int](numDims)(0)), IR_ExpressionIndex(numPointsDim_tmp)),
              IR_Assignment(
                IR_ArrayAccess(IR_VariableAccess(tmp_decl), idx_tmp),
                // TODO: assumes slot = 0
                IR_VariableAccess(visit_scaleCurvemesh) * IR_LinearizedFieldAccess(field, slot = 0, IR_LoopOverFragments.defIt, idx_field + offsetToInnerPoints)
              )
            )
          )
        }

        val variable_access = if (dataIsCopied) {
          IR_VariableAccess(tmp_decl)
        }
        else {
          // pass pointer of field if nothing was copied
          if (Knowledge.numLevels > 1) {
            IR_ArrayAccess(IR_VariableAccess("fieldData_" + field.name, field.layout.datatype), field.level - Knowledge.minLevel)
          } else {
            IR_VariableAccess("fieldData_" + field.name, field.layout.datatype)
          }
        }

        // determine whether doubles or floats are sent
        val funcRef = if (Knowledge.useDblPrecision) IR_ExternalFunctionReference("VisIt_VariableData_setDataD") else IR_ExternalFunctionReference("VisIt_VariableData_setDataF")

        for (dim <- 0 until numDims) {
          val curveCoords_access = if (numDims == 1) {
            if (Knowledge.numLevels > 1) IR_ArrayAccess(IR_VariableAccess(curveCoords_decl), field.level - Knowledge.minLevel) else IR_VariableAccess(curveCoords_decl)
          } else {
            if (Knowledge.numLevels > 1) {
              IR_MultiDimArrayAccess(IR_VariableAccess(curveCoords_decl), IR_ExpressionIndex(Array[IR_Expression](dim, field.level - Knowledge.minLevel)))
            } else {
              IR_ArrayAccess(IR_VariableAccess(curveCoords_decl), dim)
            }
          }

          // pass coordinate array, simulation responsible for freeing memory
          ifBody += IR_FunctionCall(funcRef,
            IR_ArrayAccess(IR_VariableAccess(handles_decl), dim),
            IR_Native("VISIT_OWNER_SIM"),
            IR_IntegerConstant(1),
            numPointsTotal_tmp.product,
            curveCoords_access
          )
        }

        // determines whether simulation or VisIt is responsible for freeing
        val ownership = if (dataIsCopied) IR_Native("VISIT_OWNER_VISIT") else IR_Native("VISIT_OWNER_SIM")

        // pass tmp array or field
        ifBody += IR_FunctionCall(funcRef,
          IR_ArrayAccess(IR_VariableAccess(handles_decl), numDims),
          ownership,
          IR_IntegerConstant(1),
          numPointsTotal_tmp.product,
          variable_access
        )
        ifBody += funcCall

        fctBody += IR_IfCondition(
          IR_AndAnd(
            IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("name", IR_StringDatatype), IR_StringConstant("curv" + (numDims + 1) + "d_" + field.name)) EqEq IR_IntegerConstant(0),
            IR_VariableAccess(cur_level_decl) EqEq field.level
          ),
          ListBuffer[IR_Statement](
            IR_IfCondition(
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CurvilinearMesh_alloc"), IR_AddressOf(IR_VariableAccess(h_decl))) EqEq visitOkay,
              ifBody
            )
          )
        )
      }
    }

    fctBody += IR_Return(IR_VariableAccess(h_decl))

    IR_PlainFunction(
      name,
      visitHandle,
      ListBuffer(IR_FunctionArgument("domain", IR_IntegerDatatype), IR_FunctionArgument("name", IR_SpecialDatatype("const char*")), IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype))),
      fctBody
    )
  }

  override def name : String = "SimGetMesh"
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint()
}
