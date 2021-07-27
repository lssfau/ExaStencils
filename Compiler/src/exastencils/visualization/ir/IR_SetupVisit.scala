package exastencils.visualization.ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.datastructures._
import exastencils.domain.ir._
import exastencils.field.ir.IR_FieldCollection
import exastencils.field.ir._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.parallelization.api.mpi._

/// IR_SetupVisit

object IR_SetupVisit extends DefaultStrategy("Setup Visit functions") {

  // global declarations to make variable access easier
  val sim_done_decl = IR_VariableDeclaration(IR_BooleanDatatype, "sim_done", IR_BooleanConstant(false))
  val cur_level_decl = IR_VariableDeclaration(IR_IntegerDatatype, "cur_level", NumberToIntegerConstant(Knowledge.maxLevel))

  val sim_time_decl = IR_VariableDeclaration(IR_DoubleDatatype, "sim_time", IR_DoubleConstant(0.0))
  val sim_cycle_decl = IR_VariableDeclaration(IR_IntegerDatatype, "sim_cycle", IR_IntegerConstant(0))

  val visit_runMode_decl = IR_VariableDeclaration(IR_BooleanDatatype, "visit_runMode", IR_BooleanConstant(false))
  val visit_updatePlots_decl = IR_VariableDeclaration(IR_BooleanDatatype, "visit_updatePlots", IR_BooleanConstant(true))

  val visit_scaleCurvemesh = IR_VariableDeclaration(IR_RealDatatype, "scale", IR_RealConstant(1.0))

  var innerDatatype : IR_Datatype = IR_PointerDatatype(IR_RealDatatype)
  if (Knowledge.dimensionality > 1) innerDatatype = IR_ArrayDatatype(innerDatatype, Knowledge.dimensionality)
  if (Knowledge.numLevels > 1) innerDatatype = IR_ArrayDatatype(innerDatatype, Knowledge.numLevels)

  // coordinate arrays to create the rectilinear mesh(coordsNode for node/cell variables, coordsFace for face centered variables)
  val coordsNode_decl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsNode")
  val coordsZone_decl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsZone")
  val coordsFaceX_decl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsFace_x")
  val coordsFaceY_decl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsFace_y")
  val coordsFaceZ_decl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "coordsFace_z")
  val coordsFaceAsVec : Array[IR_VariableDeclaration] = Array[IR_VariableDeclaration](coordsFaceX_decl, coordsFaceY_decl, coordsFaceZ_decl)

  // coordinate arrays to create the 3d curvilinear mesh for 2d variables and 2d curvilinear variable consisting of 1d variables
  // requires special handling for cell-based variables since they are not placed on a mesh's zone anymore
  val curveCoordsNode_decl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsNode")
  val curveCoordsZone_decl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsZone")
  val curveCoordsFaceX_decl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsFace_x")
  val curveCoordsFaceY_decl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsFace_y")
  val curveCoordsFaceZ_decl : IR_VariableDeclaration = IR_VariableDeclaration(innerDatatype, "curve_coordsFace_z")
  val curveCoordsFaceAsVec : Array[IR_VariableDeclaration] = Array[IR_VariableDeclaration](curveCoordsFaceX_decl, curveCoordsFaceY_decl, curveCoordsFaceZ_decl)

  val coords_arrays : ArrayBuffer[IR_VariableDeclaration] = ArrayBuffer[IR_VariableDeclaration]()
  val isNodalInDim : ArrayBuffer[Array[Int]] = ArrayBuffer[Array[Int]]()

  val curveCoords_arrays : ArrayBuffer[IR_VariableDeclaration] = ArrayBuffer[IR_VariableDeclaration]()
  val isNodalInDim_curve : ArrayBuffer[Array[Int]] = ArrayBuffer[Array[Int]]()

  // get variable localizations for rectilinear and curvilinear meshes
  for (field <- IR_FieldCollection.objects) {
    val loc_name = field.layout.localization.name
    val faceCenter_dir = loc_name.charAt(loc_name.length - 1).toInt - 'x'.toInt
    if (loc_name == "Node" && !coords_arrays.contains(coordsNode_decl)) {
      coords_arrays += coordsNode_decl
      isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(1)

      curveCoords_arrays += curveCoordsNode_decl
      isNodalInDim_curve += Array.fill[Int](Knowledge.dimensionality)(1)
    }
    if (loc_name == "Cell" && !coords_arrays.contains(coordsZone_decl)) {
      coords_arrays += coordsZone_decl
      isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(1)

      curveCoords_arrays += curveCoordsZone_decl
      isNodalInDim_curve += Array.fill[Int](Knowledge.dimensionality)(0)
    }
    if (loc_name.contains("Face") && !coords_arrays.contains(coordsFaceAsVec(faceCenter_dir))) {
      coords_arrays += coordsFaceAsVec(faceCenter_dir)
      isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(0)
      isNodalInDim.last(faceCenter_dir) = 1

      curveCoords_arrays += curveCoordsFaceAsVec(faceCenter_dir)
      isNodalInDim_curve += Array.fill[Int](Knowledge.dimensionality)(0)
      isNodalInDim_curve.last(faceCenter_dir) = 1
    }
  }

  val commandNames_decl : IR_VariableDeclaration = if (Knowledge.numLevels > 1) {
    IR_VariableDeclaration(IR_ArrayDatatype(IR_PointerDatatype(IR_CharDatatype), 6), "commandNames", Option[IR_Expression](IR_Native("{\"step\", \"stop\", \"run\", \"switchUpdates\", \"level down\", \"level up\" }")), isConst = true)
  } else {
    IR_VariableDeclaration(IR_ArrayDatatype(IR_PointerDatatype(IR_CharDatatype), 4), "commandNames", Option[IR_Expression](IR_Native("{\"step\", \"stop\", \"run\", \"switchUpdates\"}")), isConst = true)
  }

  // only gets generated when dimensionality is either 2 or 3
  def setupFct_SimGetVariable() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()
    val h_decl = IR_VariableDeclaration(IR_SpecialDatatype("visit_handle"), "h", IR_Native("VISIT_INVALID_HANDLE"))

    fctBody += h_decl

    for (field <- IR_FieldCollection.sortedObjects) {
      val numDims = field.layout.numDimsGrid
      val numPointsDim_tmp = (0 until numDims).map(d => field.layout.defIdxDupRightEnd(d) - field.layout.defIdxDupLeftBegin(d)).toArray
      val numPointsDim_field = (0 until numDims).map(d => field.layout.defIdxPadRightEnd(d) - field.layout.defIdxPadLeftBegin(d)).toArray
      val numOuterLayersLeft = (0 until numDims).map(d => field.layout.defIdxDupLeftBegin(d) - field.layout.defIdxPadLeftBegin(d)).toArray
      val numOuterLayersRight = (0 until numDims).map(d => field.layout.defIdxPadRightEnd(d) - field.layout.defIdxDupRightEnd(d)).toArray
      val isNodalDim = (0 until numDims).map(d => numPointsDim_tmp(d) % 2)
      val numPointsTotal_tmp = (0 until numDims).map(d => (Knowledge.domain_rect_numFragsPerBlockAsVec(d) * (numPointsDim_tmp(d) - isNodalDim(d))) + isNodalDim(d))

      // determine if data must be copied or not
      val dataIsCopied = if (numOuterLayersLeft.sum != 0 || numOuterLayersRight.sum != 0 || Knowledge.domain_numFragmentsPerBlock > 1) true else false

      val tmp_decl = IR_VariableDeclaration(IR_PointerDatatype(IR_RealDatatype), "tmp")
      // offset depending on number of ghost/pad layers
      val offsetToInnerPoints = if (numDims == 2) {
        numOuterLayersLeft(1) * numPointsDim_field(0) + numOuterLayersLeft(0)
      } else {
        numOuterLayersLeft(2) * numPointsDim_field(0) * numPointsDim_field(1) + numOuterLayersLeft(1) * numPointsDim_field(0) + numOuterLayersLeft(0)
      }

      // offset to the current fragment
      val fragOffset = (0 until numDims).map(d => if (Knowledge.domain_rect_numFragsPerBlockAsVec(d) <= 1) IR_IntegerConstant(0)
      else (numPointsDim_tmp(d) - isNodalDim(d)) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(d))).toArray

      // indices for array access
      val idxTmp = if (numDims == 2) {
        numPointsTotal_tmp(0) * (IR_FieldIteratorAccess(1) + fragOffset(1)) + (IR_FieldIteratorAccess(0) + fragOffset(0))
      } else {
        numPointsTotal_tmp(0) * numPointsTotal_tmp(1) * (IR_FieldIteratorAccess(2) + fragOffset(2)) + // linearized z
          numPointsTotal_tmp(0) * (IR_FieldIteratorAccess(1) + fragOffset(1)) + // linearized y
          (IR_FieldIteratorAccess(0) + fragOffset(0)) // linearized x
      }

      val idxField = if (numDims == 2) {
        numPointsDim_field(0) * IR_FieldIteratorAccess(1) + IR_FieldIteratorAccess(0)
      } else {
        numPointsDim_field(0) * numPointsDim_field(1) * IR_FieldIteratorAccess(2) +
          numPointsDim_field(0) * IR_FieldIteratorAccess(1) + IR_FieldIteratorAccess(0)
      }

      // array accesses depending on number of levels
      val arrayAccess = IR_ArrayAccess(IR_VariableAccess(tmp_decl), IR_ExpressionIndex(idxTmp))

      // direct access to field if data is not copied
      val arrayAccessArg = if (!dataIsCopied) {
        if (Knowledge.numLevels > 1) {
          IR_ArrayAccess(IR_VariableAccess("fieldData_" + field.name, field.layout.datatype), field.level - Knowledge.minLevel)
        } else {
          IR_VariableAccess("fieldData_" + field.name, field.layout.datatype)
        }
      }
      else IR_VariableAccess(tmp_decl)

      // determine whether simulation or VisIt is responsible for freeing
      val ownership = if (!dataIsCopied) {
        IR_Native("VISIT_OWNER_SIM")
      } else {
        IR_Native("VISIT_OWNER_VISIT")
      }

      // determine whether doubles or floats are sent
      val funcRef = if (Knowledge.useDblPrecision) IR_ExternalFunctionReference("VisIt_VariableData_setDataD") else IR_ExternalFunctionReference("VisIt_VariableData_setDataF")

      val sendData = IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableData_alloc"), IR_AddressOf(IR_VariableAccess(h_decl))) EqEq IR_Native("VISIT_OKAY"),
        IR_FunctionCall(funcRef, IR_VariableAccess(h_decl),
          ownership, IR_IntegerConstant(1), numPointsTotal_tmp.product, arrayAccessArg
        )
      )

      // no copy needed if offset == 0 and no multiple fragments
      val loop_statement = ListBuffer[IR_Statement]()

      if (!dataIsCopied) {
        loop_statement += sendData
      } else {
        loop_statement += tmp_decl
        loop_statement += IR_Assignment(
          IR_VariableAccess(tmp_decl),
          IR_Cast(IR_PointerDatatype(IR_RealDatatype), IR_FunctionCall(IR_ExternalFunctionReference("malloc"), numPointsTotal_tmp.product * IR_SizeOf(IR_RealDatatype)))
        )
        loop_statement += IR_LoopOverFragments(
          IR_LoopOverDimensions(numDims, IR_ExpressionIndexRange(IR_ExpressionIndex(Array.fill[Int](numDims)(0)), IR_ExpressionIndex(numPointsDim_tmp)),
            IR_Assignment( //copy values from field to tmp
              arrayAccess,
              // TODO: assumes slot = 0
              IR_LinearizedFieldAccess(field, slot = 0, IR_LoopOverFragments.defIt, IR_ExpressionIndex(idxField + offsetToInnerPoints))
            )
          )
        )
        loop_statement += sendData
      }

      fctBody += IR_IfCondition(
        IR_AndAnd(
          IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("name", IR_StringDatatype), IR_StringConstant(field.name)) EqEq IR_IntegerConstant(0),
          field.level EqEq IR_VariableAccess(cur_level_decl)
        ),
        loop_statement
      )
    }

    fctBody += IR_Return(IR_VariableAccess(h_decl))

    IR_PlainFunction(
      "SimGetVariable",
      IR_SpecialDatatype("visit_handle"),
      ListBuffer(IR_FunctionArgument("domain", IR_IntegerDatatype), IR_FunctionArgument("name", IR_SpecialDatatype("const char*")), IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype))),
      fctBody
    )
  }

  def setupFct_SimGetMesh() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()
    val h_decl = IR_VariableDeclaration(IR_SpecialDatatype("visit_handle"), "h", IR_Native("VISIT_INVALID_HANDLE"))
    val handles_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_SpecialDatatype("visit_handle"), Knowledge.dimensionality), "handles")

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
                IR_FunctionCall(IR_ExternalFunctionReference("VisIt_RectilinearMesh_alloc"), IR_AddressOf(IR_VariableAccess(h_decl))) EqEq IR_Native("VISIT_OKAY"),
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
        val handlesCurve_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_SpecialDatatype("visit_handle"), numDims + 1), "handles")
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
        val ownership = if (dataIsCopied) {
          IR_Native("VISIT_OWNER_VISIT")
        } else {
          IR_Native("VISIT_OWNER_SIM")
        }

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
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CurvilinearMesh_alloc"), IR_AddressOf(IR_VariableAccess(h_decl))) EqEq IR_Native("VISIT_OKAY"),
              ifBody
            )
          )
        )
      }
    }

    fctBody += IR_Return(IR_VariableAccess(h_decl))

    IR_PlainFunction(
      "SimGetMesh",
      IR_SpecialDatatype("visit_handle"),
      ListBuffer(IR_FunctionArgument("domain", IR_IntegerDatatype), IR_FunctionArgument("name", IR_SpecialDatatype("const char*")), IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype))),
      fctBody
    )
  }

  def setupFct_SimGetMetaData() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()
    val ifBody = ListBuffer[IR_Statement]()

    val md_decl = IR_VariableDeclaration(IR_SpecialDatatype("visit_handle"), "metadata", IR_Native("VISIT_INVALID_HANDLE"))

    var new_name_identifier = "" // detects new field name

    // simulation metadata(mode, time, cycle)
    val mode_decl = IR_VariableDeclaration(IR_IntegerDatatype, "mode", IR_TernaryCondition(IR_VariableAccess(visit_runMode_decl), IR_Native("VISIT_SIMMODE_RUNNING"), IR_Native("VISIT_SIMMODE_STOPPED")))
    ifBody += mode_decl
    ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_setMode"), IR_VariableAccess(md_decl), IR_VariableAccess(mode_decl))
    ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_setCycleTime"), IR_VariableAccess(md_decl), IR_VariableAccess(sim_cycle_decl), IR_VariableAccess(sim_time_decl))

    if (Knowledge.dimensionality > 1) {
      for (coords_decl <- coords_arrays) {
        val mmd_decl = IR_VariableDeclaration(IR_SpecialDatatype("visit_handle"), "meshMetadata_" + coords_decl.name.drop(6), IR_Native("VISIT_INVALID_HANDLE"))

        ifBody += mmd_decl
        ifBody += IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_alloc"), IR_AddressOf(IR_VariableAccess(mmd_decl))) EqEq IR_Native("VISIT_OKAY"),
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setName"), IR_VariableAccess(mmd_decl), IR_StringConstant("rect" + Knowledge.dimensionality + "d_" + coords_decl.name.drop(6))),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setMeshType"), IR_VariableAccess(mmd_decl), IR_Native("VISIT_MESHTYPE_RECTILINEAR")),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setTopologicalDimension"), IR_VariableAccess(mmd_decl), Knowledge.dimensionality),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setSpatialDimension"), IR_VariableAccess(mmd_decl), Knowledge.dimensionality),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setNumDomains"), IR_VariableAccess(mmd_decl), Knowledge.domain_numBlocks),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addMesh"), IR_VariableAccess(md_decl), IR_VariableAccess(mmd_decl))
          )
        )
      }
    }
    if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) {
      // display one dimensional variables as a 2d curvilinear mesh consisting of the created coordinate array and the variable values
      // display two dimensional variables as a 3d curvilinear
      for (field <- IR_FieldCollection.sortedObjects) {
        if (field.name != new_name_identifier) {
          val mmd_decl = IR_VariableDeclaration(IR_SpecialDatatype("visit_handle"), "meshMetadata_" + field.name, IR_Native("VISIT_INVALID_HANDLE"))

          ifBody += mmd_decl
          ifBody += IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_alloc"), IR_AddressOf(IR_VariableAccess(mmd_decl))) EqEq IR_Native("VISIT_OKAY"),
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setName"), IR_VariableAccess(mmd_decl), IR_StringConstant("curv" + (Knowledge.dimensionality + 1) + "d_" + field.name)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setMeshType"), IR_VariableAccess(mmd_decl), IR_Native("VISIT_MESHTYPE_CURVILINEAR")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setTopologicalDimension"), IR_VariableAccess(mmd_decl), Knowledge.dimensionality + 1),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setSpatialDimension"), IR_VariableAccess(mmd_decl), Knowledge.dimensionality + 1),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setNumDomains"), IR_VariableAccess(mmd_decl), Knowledge.domain_numBlocks),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addMesh"), IR_VariableAccess(md_decl), IR_VariableAccess(mmd_decl))
            )
          )
          new_name_identifier = field.name
        }
      }
    }

    new_name_identifier = "" // reset

    if (Knowledge.dimensionality > 1) {
      // variable metadata
      for (field <- IR_FieldCollection.sortedObjects) {
        if (field.name != new_name_identifier) {
          val vmd_decl = IR_VariableDeclaration(IR_SpecialDatatype("visit_handle"), "varMetadata_" + field.name, IR_Native("VISIT_INVALID_HANDLE"))
          val loc_name = field.layout.localization.name
          val varCentering = if (loc_name == "Cell") {
            IR_Native("VISIT_VARCENTERING_ZONE")
          } else {
            IR_Native("VISIT_VARCENTERING_NODE")
          }
          val meshname = if (loc_name == "Node") "Node" else if (loc_name == "Cell") "Zone" else "Face_" + loc_name.charAt(loc_name.length() - 1)
          ifBody += vmd_decl
          ifBody += IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_alloc"), IR_AddressOf(IR_VariableAccess(vmd_decl))) EqEq IR_Native("VISIT_OKAY"),
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setName"), IR_VariableAccess(vmd_decl), IR_StringConstant(field.name)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setMeshName"), IR_VariableAccess(vmd_decl), IR_StringConstant("rect" + Knowledge.dimensionality + "d_" + meshname)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setType"), IR_VariableAccess(vmd_decl), IR_Native("VISIT_VARTYPE_SCALAR")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setCentering"), IR_VariableAccess(vmd_decl), varCentering),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addVariable"), IR_VariableAccess(md_decl), IR_VariableAccess(vmd_decl))
            )
          )
          new_name_identifier = field.name
        }
      }
    }

    // command metadata
    val cmd_decl = IR_VariableDeclaration(IR_SpecialDatatype("visit_handle"), "commandMetadata", IR_Native("VISIT_INVALID_HANDLE"))
    ifBody += IR_ForLoop(
      IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < commandNames_decl.datatype.resolveFlattendSize, IR_PostIncrement(IR_FieldIteratorAccess(0)),
      ListBuffer[IR_Statement](
        cmd_decl,
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CommandMetaData_alloc"), IR_AddressOf(IR_VariableAccess(cmd_decl))) EqEq IR_Native("VISIT_OKAY"),
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CommandMetaData_setName"), IR_VariableAccess(cmd_decl), IR_ArrayAccess(IR_VariableAccess(commandNames_decl), IR_FieldIteratorAccess(0))),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addGenericCommand"), IR_VariableAccess(md_decl), IR_VariableAccess(cmd_decl))
          )
        )
      )
    )

    fctBody += md_decl
    fctBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_alloc"), IR_AddressOf(IR_VariableAccess(md_decl))) EqEq IR_Native("VISIT_OKAY"),
      ifBody
    )

    fctBody += IR_Return(IR_VariableAccess(md_decl))

    IR_PlainFunction(
      "SimGetMetaData",
      IR_SpecialDatatype("visit_handle"),
      IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype)),
      fctBody
    )
  }

  // implement functionality for the control buttons on the GUI
  def setupFct_ControlCommandCallback() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()

    fctBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("cmd", IR_ConstPointerDatatype(IR_CharDatatype)), IR_StringConstant("step")) EqEq IR_IntegerConstant(0),
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_LeveledInternalFunctionReference("simulate_timestep", Knowledge.maxLevel, IR_UnitDatatype)),
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
          ListBuffer[IR_Statement](
            IR_IfCondition(
              IR_VariableAccess(visit_updatePlots_decl),
              ListBuffer[IR_Statement](
                IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
                IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))
              )
            )
          )
        )
      )
    )
    fctBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("cmd", IR_ConstPointerDatatype(IR_CharDatatype)), IR_StringConstant("stop")) EqEq IR_IntegerConstant(0),
      IR_Assignment(IR_VariableAccess(visit_runMode_decl), IR_BooleanConstant(false))
    )
    fctBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("cmd", IR_ConstPointerDatatype(IR_CharDatatype)), IR_StringConstant("run")) EqEq IR_IntegerConstant(0),
      IR_Assignment(IR_VariableAccess(visit_runMode_decl), IR_BooleanConstant(true))
    )
    fctBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("cmd", IR_ConstPointerDatatype(IR_CharDatatype)), IR_StringConstant("switchUpdates")) EqEq IR_IntegerConstant(0),
      IR_Assignment(IR_VariableAccess(visit_updatePlots_decl), IR_Negation(IR_VariableAccess(visit_updatePlots_decl)))
    )
    // only register level switches when necessary
    if (Knowledge.numLevels > 1) {
      fctBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("cmd", IR_ConstPointerDatatype(IR_CharDatatype)), IR_StringConstant("level down")) EqEq IR_IntegerConstant(0),
        ListBuffer[IR_Statement](
          IR_Assignment(IR_VariableAccess(cur_level_decl), IR_Maximum(IR_VariableAccess(cur_level_decl) - IR_IntegerConstant(1), Knowledge.minLevel)),
          IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))
            )
          )
        )
      )
      fctBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("cmd", IR_ConstPointerDatatype(IR_CharDatatype)), IR_StringConstant("level up")) EqEq IR_IntegerConstant(0),
        ListBuffer[IR_Statement](
          IR_Assignment(IR_VariableAccess(cur_level_decl), IR_Minimum(IR_VariableAccess(cur_level_decl) + IR_IntegerConstant(1), Knowledge.maxLevel)),
          IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))
            )
          )
        )
      )
    }

    IR_PlainFunction(
      "ControlCommandCallback",
      IR_UnitDatatype,
      ListBuffer(IR_FunctionArgument("cmd", IR_SpecialDatatype("const char*")), IR_FunctionArgument("args", IR_SpecialDatatype("const char*")), IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype))),
      fctBody
    )
  }

  def setupFct_visit_init() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()
    val cwd_decl = IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "cwd", IR_Native("NULL"))

    val getCWD = if (Platform.targetCompiler == "MSVC") {
      IR_ExternalFunctionReference("_getcwd")
    } else {
      IR_ExternalFunctionReference("getcwd")
    }

    // allocate coordinate arrays
    for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
      for (coords_decl <- coords_arrays.distinct) {
        for (dim <- 0 until Knowledge.dimensionality) {
          val array_indices = ArrayBuffer[Int]()
          if (Knowledge.dimensionality > 1) array_indices += dim
          if (Knowledge.numLevels > 1) array_indices += level - Knowledge.minLevel

          val curCoords = coords_arrays.indexOf(coords_decl)
          val coords_access = if (array_indices.isEmpty) {
            IR_VariableAccess(coords_decl)
          }
          else if (array_indices.length == 1) {
            IR_ArrayAccess(IR_VariableAccess(coords_decl), array_indices.head)
          }
          else IR_MultiDimArrayAccess(IR_VariableAccess(coords_decl), IR_ExpressionIndex(array_indices.toArray))

          // regular coordinates for rect mesh, not used in 1d case
          if (Knowledge.dimensionality > 1) {
            fctBody += IR_ArrayAllocation(coords_access, IR_RealDatatype,
              (Knowledge.domain_fragmentLengthAsVec(dim) * Knowledge.domain_rect_numFragsPerBlockAsVec(dim) * (1 << level)) + isNodalInDim(curCoords)(dim))
          }

          // curve coordinates
          if (Knowledge.dimensionality < 3) {
            val curveCoords_decl = IR_VariableDeclaration(coords_decl.datatype, "curve_" + coords_decl.name)
            val curCoords_curve = curveCoords_arrays.indexOf(curveCoords_decl)
            val curveCoords_access = if (array_indices.isEmpty) {
              IR_VariableAccess(curveCoords_decl)
            }
            else if (array_indices.length == 1) {
              IR_ArrayAccess(IR_VariableAccess(curveCoords_decl), array_indices.head)
            }
            else IR_MultiDimArrayAccess(IR_VariableAccess(curveCoords_decl), IR_ExpressionIndex(array_indices.toArray))

            if (Knowledge.dimensionality == 1) {
              fctBody += IR_ArrayAllocation(
                curveCoords_access, IR_RealDatatype,
                (Knowledge.domain_fragmentLength_x * Knowledge.domain_rect_numFragsPerBlock_x * (1 << level)) + isNodalInDim_curve(curCoords_curve)(dim)
              )
            }
            // x and y coordinates for every point within the 2d domain
            if (Knowledge.dimensionality == 2) {
              fctBody += IR_ArrayAllocation(
                curveCoords_access, IR_RealDatatype,
                ((Knowledge.domain_fragmentLength_x * Knowledge.domain_rect_numFragsPerBlock_x * (1 << level)) + isNodalInDim_curve(curCoords_curve)(0)) *
                  ((Knowledge.domain_fragmentLength_y * Knowledge.domain_rect_numFragsPerBlock_y * (1 << level)) + isNodalInDim_curve(curCoords_curve)(1))
              )
            }
          }
        }
      }
    }

    // put Knowledge discr_h* (* = x/y/z) into one array
    val discr_perDim = Array.ofDim[Double](Knowledge.dimensionality, Knowledge.numLevels)
    for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
      discr_perDim(0)(level - Knowledge.minLevel) = Knowledge.discr_hx(level - Knowledge.minLevel)
      if (Knowledge.dimensionality > 1) discr_perDim(1)(level - Knowledge.minLevel) = Knowledge.discr_hy(level - Knowledge.minLevel)
      if (Knowledge.dimensionality > 2) discr_perDim(2)(level - Knowledge.minLevel) = Knowledge.discr_hz(level - Knowledge.minLevel)
    }

    for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
      for (coords_decl <- coords_arrays.distinct) {
        val curveCoords_decl = IR_VariableDeclaration(coords_decl.datatype, "curve_" + coords_decl.name)
        val curCoords = coords_arrays.indexOf(coords_decl)
        val curCoords_curve = curveCoords_arrays.indexOf(curveCoords_decl)

        // get number of points per dimension
        val numPointsDim = (0 until Knowledge.dimensionality).map(d => Knowledge.domain_fragmentLengthAsVec(d) * (1 << level) + isNodalInDim(curCoords)(d))
        val numPointsDim_curve = (0 until Knowledge.dimensionality).map(d => Knowledge.domain_fragmentLengthAsVec(d) * (1 << level) + isNodalInDim_curve(curCoords_curve)(d))

        // get current position of point "i" in the current block/fragment
        val stepSizeDim = Array.ofDim[IR_Expression](Knowledge.dimensionality)
        val stepSizeDim_curve = Array.ofDim[IR_Expression](Knowledge.dimensionality) // adapt step size for initialization of curve coordinates

        for (dim <- 0 until Knowledge.dimensionality) {
          // compute offset towards cell-center
          val iterator = if (isNodalInDim(curCoords)(dim) != 1) IR_FieldIteratorAccess(0) + 0.5 else IR_FieldIteratorAccess(0)
          val iterator_curve = if (isNodalInDim_curve(curCoords_curve)(dim) != 1) IR_FieldIteratorAccess(dim) + 0.5 else IR_FieldIteratorAccess(dim)

          // compute offset towards point "i" on current block/fragment
          if (Knowledge.mpi_enabled || Knowledge.domain_numFragmentsPerBlock > 1) {
            stepSizeDim(dim) = IR_IV_FragmentPositionBegin(dim) + iterator * discr_perDim(dim)(level - Knowledge.minLevel)
            stepSizeDim_curve(dim) = IR_IV_FragmentPositionBegin(dim) + iterator_curve * discr_perDim(dim)(level - Knowledge.minLevel)
          } else {
            stepSizeDim(dim) = iterator * discr_perDim(dim)(level - Knowledge.minLevel)
            stepSizeDim_curve(dim) = iterator_curve * discr_perDim(dim)(level - Knowledge.minLevel)
          }
        }

        val forBody = ListBuffer[IR_Statement]()
        for (dim <- 0 until Knowledge.dimensionality) {
          // assign coordinate values, 1d cases only need curve coords
          if (Knowledge.dimensionality > 1) {
            val array_indices = ArrayBuffer[IR_Expression]()
            if (Knowledge.domain_numFragmentsPerBlock > 1) {
              array_indices += IR_FieldIteratorAccess(0) +
                (numPointsDim(dim) - isNodalInDim(curCoords)(dim)) * (IR_IV_FragmentIndex(dim) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim))
            } else {
              array_indices += IR_FieldIteratorAccess(0)
            }
            if (Knowledge.dimensionality > 1) array_indices += dim
            if (Knowledge.numLevels > 1) array_indices += level - Knowledge.minLevel

            val coords_access = if (array_indices.length == 1) {
              IR_ArrayAccess(IR_VariableAccess(coords_decl), array_indices.head)
            } else {
              IR_MultiDimArrayAccess(IR_VariableAccess(coords_decl), IR_ExpressionIndex(array_indices.toArray))
            }

            forBody += IR_ForLoop(
              IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < numPointsDim(dim), IR_PreIncrement(IR_FieldIteratorAccess(0)),
              IR_Assignment(coords_access, stepSizeDim(dim))
            )
          }

          // curve coords for 1d and 2d problems
          if (Knowledge.dimensionality < 3) {
            // offset to the current fragment
            val fragOffset = (0 until Knowledge.dimensionality).map(d => if (Knowledge.domain_rect_numFragsPerBlockAsVec(d) <= 1) IR_IntegerConstant(0)
            else (numPointsDim_curve(d) - isNodalInDim_curve(curCoords_curve)(d)) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(d))).toArray

            val arrayIndices_curve = ArrayBuffer[IR_Expression]()
            if (Knowledge.domain_numFragmentsPerBlock > 1) {
              if (Knowledge.dimensionality == 1) {
                arrayIndices_curve += IR_FieldIteratorAccess(0) + fragOffset(0)
              } else {
                arrayIndices_curve +=
                  (Knowledge.domain_rect_numFragsPerBlock_x * (numPointsDim_curve(0) - isNodalInDim_curve(curCoords_curve)(dim)) + isNodalInDim_curve(curCoords_curve)(dim)) * (IR_FieldIteratorAccess(1) + fragOffset(1)) +
                    (IR_FieldIteratorAccess(0) + fragOffset(0))
              }
            } else {
              if (Knowledge.dimensionality == 1) {
                arrayIndices_curve += IR_FieldIteratorAccess(0)
              } else {
                arrayIndices_curve += numPointsDim_curve(0) * IR_FieldIteratorAccess(1) + IR_FieldIteratorAccess(0)
              }
            }
            if (Knowledge.dimensionality > 1) arrayIndices_curve += dim
            if (Knowledge.numLevels > 1) arrayIndices_curve += level - Knowledge.minLevel

            val curveCoords_access = if (arrayIndices_curve.length == 1) {
              IR_ArrayAccess(IR_VariableAccess(curveCoords_decl), arrayIndices_curve.head)
            } else {
              IR_MultiDimArrayAccess(IR_VariableAccess(curveCoords_decl), IR_ExpressionIndex(arrayIndices_curve.toArray))
            }

            // assign curve coordinate values
            if (Knowledge.dimensionality == 1) {
              forBody += IR_ForLoop(
                IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < numPointsDim_curve(0), IR_PreIncrement(IR_FieldIteratorAccess(0)),
                IR_Assignment(curveCoords_access, stepSizeDim_curve(dim))
              )
            } else {
              forBody += IR_ForLoop(
                IR_VariableDeclaration(IR_FieldIteratorAccess(1), IR_IntegerConstant(0)), IR_FieldIteratorAccess(1) < numPointsDim_curve(1), IR_PreIncrement(IR_FieldIteratorAccess(1)),
                IR_ForLoop(
                  IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < numPointsDim_curve(0), IR_PreIncrement(IR_FieldIteratorAccess(0)),
                  IR_Assignment(curveCoords_access, stepSizeDim_curve(dim))
                )
              )
            }
          }
        }

        fctBody += IR_LoopOverFragments(
          forBody
        )
      }
    }

    // set path(top level directory where visit is installed) to select a certain visit version
    val str_decl = IR_VariableDeclaration(IR_StringDatatype, "str", IR_FunctionCall(IR_ExternalFunctionReference("std::getenv"), IR_StringConstant("VISIT_HOME")))
    val path_decl = IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "path")
    fctBody += str_decl
    fctBody += IR_IfCondition(
      IR_MemberFunctionCall(IR_VariableAccess(str_decl), "empty") Neq IR_BooleanConstant(true),
      ListBuffer[IR_Statement](
        path_decl,
        IR_ArrayAllocation(IR_VariableAccess(path_decl), IR_CharDatatype, IR_MemberFunctionCall(IR_VariableAccess(str_decl), "size") + IR_IntegerConstant(1)),
        IR_FunctionCall(
          IR_ExternalFunctionReference("std::copy"),
          IR_MemberFunctionCall(IR_VariableAccess(str_decl), "begin"),
          IR_MemberFunctionCall(IR_VariableAccess(str_decl), "end"),
          IR_VariableAccess(path_decl)
        ),
        IR_Assignment(IR_ArrayAccess(IR_VariableAccess(path_decl), IR_MemberFunctionCall(IR_VariableAccess(str_decl), "size")), IR_Native("\'\\0\'")),
        IR_FunctionCall(IR_ExternalFunctionReference("VisItSetDirectory"), IR_VariableAccess(path_decl)),
        IR_ArrayFree(IR_VariableAccess(path_decl))
      )
    )

    // name of the sim2 file
    val sim_name = ListBuffer[String](Knowledge.dimensionality + "d_" + Knowledge.discr_type)
    if (Knowledge.grid_isStaggered) sim_name += "staggered"
    sim_name += Knowledge.domain_numBlocks + "Blocks"
    sim_name += Knowledge.domain_numFragmentsPerBlock + "Frags"

    // mandatory functions for VisIt (setup environment variables, parallel initializations)
    if (Knowledge.mpi_enabled) {
      fctBody += IR_VariableDeclaration(IR_ArrayDatatype(IR_CharDatatype, 1000), "fn")
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("sprintf"), IR_VariableAccess("fn", IR_ArrayDatatype(IR_CharDatatype, 1000)), IR_StringConstant("trace_%d.txt"), MPI_IV_MpiRank)
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItOpenTraceFile"), IR_VariableAccess("fn", IR_ArrayDatatype(IR_CharDatatype, 1000)))

      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetBroadcastIntFunction"), IR_Native("visit_broadcast_int_callback"))
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetBroadcastStringFunction"), IR_Native("visit_broadcast_string_callback"))

      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetParallel"), IR_BooleanConstant(true))
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetParallelRank"), MPI_IV_MpiRank)

      fctBody += IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "env", IR_Native("NULL"))
      fctBody += IR_IfCondition(
        MPI_IsRootProc(),
        IR_Assignment(IR_VariableAccess(IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "env")), IR_FunctionCall(IR_ExternalFunctionReference("VisItGetEnvironment")))
      )
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetupEnvironment2"), IR_VariableAccess(IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "env")))
      fctBody += IR_IfCondition(
        IR_VariableAccess(IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "env")) Neq IR_Native("NULL"),
        IR_FunctionCall(IR_ExternalFunctionReference("free"), IR_VariableAccess(IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "env")))
      )

      fctBody += IR_IfCondition(
        MPI_IsRootProc.apply(),
        ListBuffer[IR_Statement](
          cwd_decl,
          IR_ArrayAllocation(IR_VariableAccess(cwd_decl), IR_CharDatatype, IR_IntegerConstant(1000)),
          IR_Assignment(IR_VariableAccess(cwd_decl), IR_FunctionCall(getCWD, IR_VariableAccess(cwd_decl), IR_IntegerConstant(1000))),
          IR_FunctionCall(IR_ExternalFunctionReference("VisItInitializeSocketAndDumpSimFile"), IR_StringConstant(sim_name.head),
            IR_StringConstant(sim_name.tail.mkString("_")), IR_VariableAccess(cwd_decl), IR_Native("NULL"), IR_Native("NULL"), IR_Native("NULL")),
          IR_ArrayFree(IR_VariableAccess(cwd_decl))
        )
      )
    } else {
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItOpenTraceFile"), IR_StringConstant("trace.txt"))
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetupEnvironment"))
      fctBody += cwd_decl
      fctBody += IR_ArrayAllocation(IR_VariableAccess(cwd_decl), IR_CharDatatype, IR_IntegerConstant(1000))
      fctBody += IR_Assignment(IR_VariableAccess(cwd_decl), IR_FunctionCall(getCWD, IR_VariableAccess(cwd_decl), IR_IntegerConstant(1000)))
      fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItInitializeSocketAndDumpSimFile"), IR_StringConstant(sim_name.head), IR_StringConstant(sim_name.tail.mkString("_")), IR_VariableAccess(cwd_decl), IR_Native("NULL"), IR_Native("NULL"), IR_Native("NULL"))
      fctBody += IR_ArrayFree(IR_VariableAccess(cwd_decl))
    }

    IR_PlainFunction(
      "visit_init",
      IR_UnitDatatype,
      fctBody
    )
  }

  def setupFct_visit_destroy() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()

    fctBody += IR_FunctionCall(IR_ExternalFunctionReference("VisItCloseTraceFile"))

    // free coords array
    for (coords_decl <- coords_arrays.distinct) {
      for (dim <- 0 until Knowledge.dimensionality) {
        val array_indices = ArrayBuffer[IR_Expression]()
        if (Knowledge.dimensionality > 1) array_indices += IR_IntegerConstant(dim)
        if (Knowledge.numLevels > 1) array_indices += IR_LoopOverLevels.defIt - Knowledge.minLevel

        if (Knowledge.dimensionality > 1) {
          val coords_access = if (array_indices.isEmpty) {
            IR_VariableAccess(coords_decl)
          } else if (array_indices.length == 1) {
            IR_ArrayAccess(IR_VariableAccess(coords_decl), array_indices.head)
          } else {
            IR_MultiDimArrayAccess(IR_VariableAccess(coords_decl), IR_ExpressionIndex(array_indices.toArray))
          }

          fctBody += IR_LoopOverLevels(
            IR_IfCondition(
              coords_access,
              ListBuffer[IR_Statement](
                IR_ArrayFree(coords_access),
                IR_Assignment(coords_access, IR_IntegerConstant(0))
              )
            )
          )
        }

        // free curve coords
        if (Knowledge.dimensionality < 3) {
          val curveCoords_decl = IR_VariableDeclaration(coords_decl.datatype, "curve_" + coords_decl.name)
          val curveCoords_access = if (array_indices.isEmpty) {
            IR_VariableAccess(curveCoords_decl)
          } else if (array_indices.length == 1) {
            IR_ArrayAccess(IR_VariableAccess(curveCoords_decl), array_indices.head)
          } else {
            IR_MultiDimArrayAccess(IR_VariableAccess(curveCoords_decl), IR_ExpressionIndex(array_indices.toArray))
          }

          fctBody += IR_LoopOverLevels(
            IR_IfCondition(
              curveCoords_access,
              ListBuffer[IR_Statement](
                IR_ArrayFree(curveCoords_access),
                IR_Assignment(curveCoords_access, IR_IntegerConstant(0))
              )
            )
          )
        }
      }
    }

    IR_PlainFunction(
      "visit_destroy",
      IR_UnitDatatype,
      fctBody
    )
  }

  def setupFct_visit_mainloop() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()
    val whileBody = ListBuffer[IR_Statement]()
    val consoleInputBody = ListBuffer[IR_Statement]()

    val blocking_decl = IR_VariableDeclaration(IR_BooleanDatatype, "blocking", IR_TernaryCondition(IR_VariableAccess(visit_runMode_decl), IR_BooleanConstant(false), IR_BooleanConstant(true)))
    val visit_input_decl = IR_VariableDeclaration(IR_IntegerDatatype, "visit_input")
    val command_decl = IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), "command", IR_Native("NULL"))

    val registerCallbackFcts = ListBuffer[IR_Statement]()
    val procEngineCommandFct = if (Knowledge.mpi_enabled) {
      IR_FunctionCall(IR_ExternalFunctionReference("ProcessVisItCommand"))
    } else {
      IR_FunctionCall(IR_ExternalFunctionReference("VisItProcessEngineCommand"))
    }

    // callback functions to register
    if (Knowledge.mpi_enabled) {
      registerCallbackFcts += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetSlaveProcessCallback"), IR_Native("slave_process_callback"))
      registerCallbackFcts += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetGetDomainList"), IR_Native("SimGetDomainList"), IR_Native("nullptr"))
    }
    registerCallbackFcts += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetGetMetaData"), IR_Native("SimGetMetaData"), IR_Native("nullptr"))
    registerCallbackFcts += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetGetMesh"), IR_Native("SimGetMesh"), IR_Native("nullptr"))
    if (Knowledge.dimensionality > 1) { // 1d variables are pictured as meshes
      registerCallbackFcts += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetGetVariable"), IR_Native("SimGetVariable"), IR_Native("nullptr"))
    }

    registerCallbackFcts += IR_FunctionCall(IR_ExternalFunctionReference("VisItSetCommandCallback"), IR_Native("ControlCommandCallback"), IR_Native("nullptr"))

    // body of the while loop containing the switch statement
    whileBody += IR_IfCondition(
      IR_VariableAccess(sim_done_decl),
      IR_Break()
    )
    whileBody += blocking_decl

    val funcRef = if (Platform.targetCompiler == "MSVC") {
      IR_ExternalFunctionReference("_fileno")
    } else {
      IR_ExternalFunctionReference("fileno")
    }

    if (Knowledge.mpi_enabled) {
      whileBody += visit_input_decl
      whileBody += IR_IfCondition(
        MPI_IsRootProc.apply(),
        IR_Assignment(IR_VariableAccess(visit_input_decl), IR_FunctionCall(IR_ExternalFunctionReference("VisItDetectInput"), IR_VariableAccess("blocking", IR_IntegerDatatype), IR_FunctionCall(funcRef, IR_Native("stdin"))))
      )
      whileBody += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_AddressOf(IR_VariableAccess(visit_input_decl)), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator)
    } else {
      whileBody += IR_VariableDeclaration(IR_IntegerDatatype, "visit_input", IR_FunctionCall(IR_ExternalFunctionReference("VisItDetectInput"), IR_VariableAccess("blocking", IR_IntegerDatatype), IR_FunctionCall(funcRef, IR_Native("stdin"))))
    }

    // body of the third case of the switch statement
    consoleInputBody += command_decl
    consoleInputBody += IR_ArrayAllocation(IR_VariableAccess(command_decl), IR_CharDatatype, IR_IntegerConstant(1000))

    if (Knowledge.mpi_enabled) {
      consoleInputBody += IR_IfCondition(
        MPI_IsRootProc.apply(),
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisItReadConsole"), IR_IntegerConstant(1000), IR_VariableAccess(command_decl)) Neq IR_Native("VISIT_OKAY"),
          IR_Break()
        )
      )
      consoleInputBody += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_VariableAccess(command_decl), IR_IntegerConstant(1000), IR_Native("MPI_CHAR"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator)
    } else {
      consoleInputBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("VisItReadConsole"), IR_IntegerConstant(1000), IR_VariableAccess(command_decl)) Neq IR_Native("VISIT_OKAY"),
        IR_Break()
      )
    }

    // process console inputs
    consoleInputBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess(command_decl), IR_StringConstant("step")) EqEq IR_IntegerConstant(0),
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_LeveledInternalFunctionReference("simulate_timestep", Knowledge.maxLevel, IR_UnitDatatype)),
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
          ListBuffer[IR_Statement](
            IR_IfCondition(
              IR_VariableAccess(visit_updatePlots_decl),
              ListBuffer[IR_Statement](
                IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
                IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))
              )
            )
          )
        )
      )
    )
    consoleInputBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess(command_decl), IR_StringConstant("stop")) EqEq IR_IntegerConstant(0),
      IR_Assignment(IR_VariableAccess(visit_runMode_decl), IR_BooleanConstant(false))
    )
    consoleInputBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess(command_decl), IR_StringConstant("run")) EqEq IR_IntegerConstant(0),
      IR_Assignment(IR_VariableAccess(visit_runMode_decl), IR_BooleanConstant(true))
    )
    consoleInputBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess(command_decl), IR_StringConstant("switchUpdates")) EqEq IR_IntegerConstant(0),
      IR_Assignment(IR_VariableAccess(visit_updatePlots_decl), IR_Negation(IR_VariableAccess(visit_updatePlots_decl)))
    )
    // scaling: only used for curvilinear meshes
    if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) {
      val funcRef_scale = if (Knowledge.useDblPrecision) IR_ExternalFunctionReference("std::stod") else IR_ExternalFunctionReference("std::stof")
      consoleInputBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("strstr"), IR_VariableAccess(command_decl), IR_StringConstant("scale=")) Neq IR_Native("NULL"),
        IR_Native(
          """|try{
             |scale = std::stod(command+6);
             |} catch (const std::invalid_argument&) {
             |
             |} catch (const std::out_of_range&) {
             |
             |}
          """.stripMargin
        )
      )
    }
    if (Knowledge.numLevels > 1) {
      consoleInputBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess(command_decl), IR_StringConstant("level down")) EqEq IR_IntegerConstant(0),
        ListBuffer[IR_Statement](
          IR_Assignment(IR_VariableAccess(cur_level_decl), IR_Maximum(IR_VariableAccess(cur_level_decl) - IR_IntegerConstant(1), Knowledge.minLevel)),
          IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))
            )
          )
        )
      )
      consoleInputBody += IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess(command_decl), IR_StringConstant("level up")) EqEq IR_IntegerConstant(0),
        ListBuffer[IR_Statement](
          IR_Assignment(IR_VariableAccess(cur_level_decl), IR_Minimum(IR_VariableAccess(cur_level_decl) + IR_IntegerConstant(1), Knowledge.maxLevel)),
          IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))
            )
          )
        )
      )
    }
    consoleInputBody += IR_ArrayFree(IR_VariableAccess(command_decl))

    whileBody += IR_IfCondition(
      IR_VariableAccess(visit_input_decl) < IR_IntegerConstant(0), // error, stop calling VisItDetectInput
      IR_Return()
    )
    whileBody += IR_IfCondition(
      IR_VariableAccess(visit_input_decl) EqEq IR_IntegerConstant(0), // VisItDetectInput timed out
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_LeveledInternalFunctionReference("simulate_timestep", Knowledge.maxLevel, IR_UnitDatatype)),
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisItIsConnected")),
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("VisItTimeStepChanged")),
            IR_IfCondition(
              IR_VariableAccess(visit_updatePlots_decl),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItUpdatePlots"))
            )
          )
        )
      )
    )
    whileBody += IR_IfCondition(IR_VariableAccess(visit_input_decl) EqEq IR_IntegerConstant(1), // inbound connection is being made
      IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("VisItAttemptToCompleteConnection")),
        registerCallbackFcts,
        IR_Native("std::cout << \"Visit connection failed. Error message: \" << VisItGetLastError() << std::endl")
      )
    )
    whileBody += IR_IfCondition(
      IR_VariableAccess(visit_input_decl) EqEq IR_IntegerConstant(2), // viewer sent instructions
      IR_IfCondition(
        procEngineCommandFct Neq IR_BooleanConstant(true),
        ListBuffer[IR_Statement](
          IR_FunctionCall(IR_ExternalFunctionReference("VisItDisconnect")),
          IR_Assignment(IR_VariableAccess(visit_runMode_decl), IR_BooleanConstant(true)) // run after VisIt closes connection
        )
      )
    )
    whileBody += IR_IfCondition(
      IR_VariableAccess(visit_input_decl) EqEq IR_IntegerConstant(3), // console input detected
      consoleInputBody
    )

    fctBody += IR_WhileLoop(
      IR_IntegerConstant(1),
      whileBody
    )

    IR_PlainFunction(
      "visit_mainloop",
      IR_UnitDatatype,
      fctBody
    )
  }

  // additional handling for parallel simulations because only root communicates with VisIt
  def setupFct_ProcessVisItCommand() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()
    val command_decl = IR_VariableDeclaration(IR_IntegerDatatype, "command")

    fctBody += command_decl
    fctBody += IR_IfCondition(
      MPI_IsRootProc.apply(),
      ListBuffer[IR_Statement](
        IR_IfCondition(
          IR_EqEq(IR_IntegerConstant(1), IR_FunctionCall(IR_ExternalFunctionReference("VisItProcessEngineCommand"))),
          ListBuffer[IR_Statement](
            IR_Assignment(IR_VariableAccess(command_decl), IR_IntegerConstant(1)),
            IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_AddressOf(IR_VariableAccess(command_decl)), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator),
            IR_Return(IR_IntegerConstant(1))
          ),
          ListBuffer[IR_Statement](
            IR_Assignment(IR_VariableAccess(command_decl), IR_IntegerConstant(0)),
            IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_AddressOf(IR_VariableAccess(command_decl)), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator),
            IR_Return(IR_IntegerConstant(0))
          )
        )
      ),
      ListBuffer[IR_Statement](
        IR_WhileLoop(
          IR_IntegerConstant(1),
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_AddressOf(IR_VariableAccess(command_decl)), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator),
            IR_IfCondition(
              IR_EqEq(IR_VariableAccess(command_decl), IR_IntegerConstant(0)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItProcessEngineCommand"))
            ),
            IR_IfCondition(
              IR_EqEq(IR_VariableAccess(command_decl), IR_IntegerConstant(1)),
              IR_Return(IR_IntegerConstant(1))
            ),
            IR_IfCondition(
              IR_EqEq(IR_VariableAccess(command_decl), IR_IntegerConstant(2)),
              IR_Return(IR_IntegerConstant(0))
            )
          )
        ),
        IR_Return(IR_IntegerConstant(1))
      )
    )

    IR_PlainFunction(
      "ProcessVisItCommand",
      IR_IntegerDatatype,
      fctBody
    )
  }

  // set number of domains per processor, mandatory
  // with 1 block per MPI Rank, a 1:1 ratio is used
  def setupFct_SimGetDomainList() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()
    val h_decl = IR_VariableDeclaration(IR_SpecialDatatype("visit_handle"), "h", IR_Native("VISIT_INVALID_HANDLE"))
    val dl_decl = IR_VariableDeclaration(IR_SpecialDatatype("visit_handle"), "domain_list", IR_Native("VISIT_INVALID_HANDLE"))

    fctBody += h_decl
    fctBody += dl_decl

    fctBody += IR_IfCondition(
      IR_AndAnd(
        IR_EqEq(IR_FunctionCall(IR_ExternalFunctionReference("VisIt_DomainList_alloc"), IR_AddressOf(IR_VariableAccess(h_decl))), IR_Native("VISIT_OKAY")),
        IR_EqEq(IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableData_alloc"), IR_AddressOf(IR_VariableAccess(dl_decl))), IR_Native("VISIT_OKAY"))
      ),
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableData_setDataI"), IR_VariableAccess(dl_decl), IR_Native("VISIT_OWNER_COPY"), IR_IntegerConstant(1), IR_IntegerConstant(1), IR_AddressOf(MPI_IV_MpiRank)),
        IR_FunctionCall(IR_ExternalFunctionReference("VisIt_DomainList_setDomains"), IR_VariableAccess(h_decl), Knowledge.mpi_numThreads, IR_VariableAccess(dl_decl))
      )
    )

    fctBody += IR_Return(IR_VariableAccess(h_decl))

    IR_PlainFunction(
      "SimGetDomainList",
      IR_SpecialDatatype("visit_handle"),
      ListBuffer(IR_FunctionArgument("name", IR_SpecialDatatype("const char*")), IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype))),
      fctBody)
  }

  def setupFct_visit_broadcast_int_callback() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()

    fctBody += IR_Return(IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_VariableAccess("value", IR_PointerDatatype(IR_IntegerDatatype)), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_VariableAccess("sender", IR_IntegerDatatype), Knowledge.mpi_defaultCommunicator))

    IR_PlainFunction(
      "visit_broadcast_int_callback",
      IR_IntegerDatatype,
      ListBuffer(IR_FunctionArgument("value", IR_PointerDatatype(IR_IntegerDatatype)), IR_FunctionArgument("sender", IR_IntegerDatatype)),
      fctBody
    )
  }

  def setupFct_visit_broadcast_string_callback() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()

    fctBody += IR_Return(IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_VariableAccess("str", IR_PointerDatatype(IR_CharDatatype)), IR_VariableAccess("len", IR_IntegerDatatype), IR_Native("MPI_CHAR"), IR_VariableAccess("sender", IR_IntegerDatatype), Knowledge.mpi_defaultCommunicator))

    IR_PlainFunction(
      "visit_broadcast_string_callback",
      IR_IntegerDatatype,
      ListBuffer(IR_FunctionArgument("str", IR_PointerDatatype(IR_CharDatatype)), IR_FunctionArgument("len", IR_IntegerDatatype), IR_FunctionArgument("sender", IR_IntegerDatatype)),
      fctBody
    )
  }

  // callback required to tell slave processes to execute command
  def setupFct_slave_process_callback() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()
    val cmd_decl = IR_VariableDeclaration(IR_IntegerDatatype, "command", IR_IntegerConstant(0))

    fctBody += cmd_decl
    fctBody += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_AddressOf(IR_VariableAccess(cmd_decl)), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator)

    IR_PlainFunction(
      "slave_process_callback",
      IR_UnitDatatype,
      fctBody
    )
  }

  this += Transformation("..", {
    case fctCollection : IR_UserFunctions =>
      if (Knowledge.dimensionality > 1) {
        fctCollection += setupFct_SimGetVariable()
      }
      fctCollection += setupFct_SimGetMesh()
      fctCollection += setupFct_SimGetMetaData()
      fctCollection += setupFct_ControlCommandCallback()
      fctCollection += setupFct_visit_init()
      fctCollection += setupFct_visit_destroy()
      fctCollection += setupFct_visit_mainloop()

      if (Knowledge.mpi_enabled) {
        fctCollection += setupFct_ProcessVisItCommand()
        fctCollection += setupFct_SimGetDomainList()
        fctCollection += setupFct_visit_broadcast_int_callback()
        fctCollection += setupFct_visit_broadcast_string_callback()
        fctCollection += setupFct_slave_process_callback()
      }

      Settings.pathsInc = (Settings.pathsInc :+ "$(SIMV2DIR)/include").distinct
      Settings.pathsLib = (Settings.pathsLib :+ "$(SIMV2DIR)/lib").distinct
      Settings.additionalLibs += "simV2"
      Settings.additionalLibs += "dl"
      fctCollection.externalDependencies += "string.h"
      fctCollection.externalDependencies += "stdlib.h"
      if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) fctCollection.externalDependencies += "stdexcept"
      if (Knowledge.mpi_enabled) {
        fctCollection.externalDependencies += "stdio.h"
      }
      fctCollection.externalDependencies += "VisItControlInterface_V2.h"
      fctCollection.externalDependencies += "VisItDataInterface_V2.h"
      if (Platform.targetCompiler == "MSVC") {
        fctCollection.externalDependencies += "direct.h"
      } else {
        fctCollection.externalDependencies += "unistd.h"
      }

      fctCollection

    case globalCollection : IR_GlobalCollection =>
      globalCollection.variables += cur_level_decl
      globalCollection.variables += visit_runMode_decl
      globalCollection.variables += visit_updatePlots_decl
      if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) globalCollection.variables += visit_scaleCurvemesh

      // coordinate arrays for 2 and 3 dim. rectilinear meshes
      if (Knowledge.dimensionality > 1) {
        for (coords_decl <- coords_arrays.distinct) globalCollection.variables += coords_decl
      }
      // coordinate arrays for 2 and 3 dim. curvilinear meshes(partially consisting of 1d or 2d variables)
      if (Knowledge.dimensionality < 3) {
        for (curveCoords_decl <- curveCoords_arrays.distinct) globalCollection.variables += curveCoords_decl
      }

      globalCollection.variables += commandNames_decl

      globalCollection
  })
}
