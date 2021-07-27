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
      case IR_AtNode if !coordsArrays.contains(coordsNodeDecl)                         =>
        coordsArrays += coordsNodeDecl
        isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(1)

        curveCoordsArrays += curveCoordsNodeDecl
        isNodalInDimCurve += Array.fill[Int](Knowledge.dimensionality)(1)
      case IR_AtCellCenter if !coordsArrays.contains(coordsZoneDecl)                   =>
        coordsArrays += coordsZoneDecl
        isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(1)

        curveCoordsArrays += curveCoordsZoneDecl
        isNodalInDimCurve += Array.fill[Int](Knowledge.dimensionality)(0)
      case face : IR_AtFaceCenter if !coordsArrays.contains(coordsFaceAsVec(face.dim)) =>
        coordsArrays += coordsFaceAsVec(face.dim)
        isNodalInDim += Array.fill[Int](Knowledge.dimensionality)(0).updated(face.dim, 1)

        curveCoordsArrays += curveCoordsFaceAsVec(face.dim)
        isNodalInDimCurve += Array.fill[Int](Knowledge.dimensionality)(0).updated(face.dim, 1)
    }
  }

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()
    val h = IR_VariableAccess("h", visitHandle)
    val handles = IR_VariableAccess("handles", IR_ArrayDatatype(visitHandle, Knowledge.dimensionality))

    fctBody += IR_VariableDeclaration(h, visitInvalidHandle)

    // 1d case only produce a curvilinear mesh
    if (Knowledge.dimensionality > 1) {
      for (coordsDecl <- coordsArrays) {
        val curCoords = coordsArrays.indexOf(coordsDecl)
        for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
          val numPointsDim = (0 until Knowledge.dimensionality).map(d => Knowledge.domain_fragmentLengthAsVec(d) * Knowledge.domain_rect_numFragsPerBlockAsVec(d) * (1 << level) + isNodalInDim(curCoords)(d))

          // coordinate setter function depending on dimensionality
          val handlesAccessDim = (0 until Knowledge.dimensionality).map(d => IR_ArrayAccess(handles, d)).toArray
          val funcCall = if (Knowledge.dimensionality == 2) {
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_RectilinearMesh_setCoordsXY"), h, handlesAccessDim(0), handlesAccessDim(1))
          } else {
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_RectilinearMesh_setCoordsXYZ"), h, handlesAccessDim(0), handlesAccessDim(1), handlesAccessDim(2))
          }

          val ifBody = ListBuffer[IR_Statement]()
          // allocate handles
          ifBody += IR_VariableDeclaration(handles)
          for (dim <- 0 until Knowledge.dimensionality) {
            ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableData_alloc"), IR_AddressOf(IR_ArrayAccess(handles, dim)))
          }

          // determine whether doubles or floats are sent
          val funcRef = if (Knowledge.useDblPrecision) IR_ExternalFunctionReference("VisIt_VariableData_setDataD") else IR_ExternalFunctionReference("VisIt_VariableData_setDataF")

          // pass pointers of coordinate arrays to handles
          for (dim <- 0 until Knowledge.dimensionality) {
            // array access depending on number of levels
            val coordsAccess = if (Knowledge.numLevels > 1) {
              IR_MultiDimArrayAccess(IR_VariableAccess(coordsDecl), IR_ExpressionIndex(Array[IR_Expression](dim, level - Knowledge.minLevel)))
            } else {
              IR_ArrayAccess(IR_VariableAccess(coordsDecl), dim)
            }

            ifBody += IR_FunctionCall(funcRef,
              IR_ArrayAccess(handles, dim),
              IR_Native("VISIT_OWNER_SIM"),
              IR_IntegerConstant(1),
              numPointsDim(dim),
              coordsAccess
            )
          }
          ifBody += funcCall

          fctBody += IR_IfCondition(
            IR_AndAnd(
              IR_FunctionCall(IR_ExternalFunctionReference("strcmp"), IR_VariableAccess("name", IR_StringDatatype), IR_StringConstant("rect" + Knowledge.dimensionality + "d_" + coordsDecl.name.drop(6))) EqEq IR_IntegerConstant(0),
              curLevel EqEq level
            ),
            ListBuffer[IR_Statement](
              IR_IfCondition(
                IR_FunctionCall(IR_ExternalFunctionReference("VisIt_RectilinearMesh_alloc"), IR_AddressOf(h)) EqEq visitOkay,
                ifBody))
          )
        }
      }
    }

    // curvilinear mesh construction for 1d and 2d problems
    if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) {
      for (field <- IR_FieldCollection.sortedObjects) {
        val numDims = field.layout.numDimsGrid
        val handlesCurve = IR_VariableAccess("handles", IR_ArrayDatatype(visitHandle, numDims + 1))
        val numPointsDimTmp = (0 until numDims).map(d => field.layout.defIdxDupRightEnd(d) - field.layout.defIdxDupLeftBegin(d)).toArray
        val numPointsDimField = (0 until numDims).map(d => field.layout.defIdxPadRightEnd(0) - field.layout.defIdxPadLeftBegin(0))
        val numOuterLayersLeft = (0 until numDims).map(d => field.layout.defIdxDupLeftBegin(d) - field.layout.defIdxPadLeftBegin(d)).toArray
        val numOuterLayersRight = (0 until numDims).map(d => field.layout.defIdxPadRightEnd(d) - field.layout.defIdxDupRightEnd(d)).toArray
        val isNodalDim = (0 until numDims).map(d => numPointsDimTmp(d) % 2)
        val fragOffset = (0 until numDims).map(d => if (Knowledge.domain_rect_numFragsPerBlockAsVec(d) <= 1) IR_IntegerConstant(0)
        else (numPointsDimTmp(d) - isNodalDim(d)) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(d)))
        val numPointsTotalTmp = (0 until numDims).map(d => Knowledge.domain_rect_numFragsPerBlockAsVec(d) * (numPointsDimTmp(d) - isNodalDim(d)) + isNodalDim(d))

        // determine if data must be copied or not
        val dataIsCopied = if (numOuterLayersLeft.sum != 0 || numOuterLayersRight.sum != 0 || Knowledge.domain_numFragmentsPerBlock > 1) true else false

        val ifBody = ListBuffer[IR_Statement]()
        // dimensionality that must be passed -> for last dimension its 1 because we send 1 variable value per mesh point
        val dims = IR_VariableAccess("dims", IR_ArrayDatatype(IR_IntegerDatatype, numDims + 1))
        ifBody += IR_VariableDeclaration(dims)
        for (dim <- 0 until numDims) {
          ifBody += IR_Assignment(IR_ArrayAccess(dims, dim), numPointsTotalTmp(dim))
        }
        ifBody += IR_Assignment(IR_ArrayAccess(dims, numDims), 1)

        val funcCall = if (numDims == 1) {
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CurvilinearMesh_setCoordsXY"), h, dims, IR_ArrayAccess(handlesCurve, 0), IR_ArrayAccess(handlesCurve, 1))
        } else {
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CurvilinearMesh_setCoordsXYZ"), h, dims, IR_ArrayAccess(handlesCurve, 0), IR_ArrayAccess(handlesCurve, 1), IR_ArrayAccess(handlesCurve, 2))
        }

        // allocate handles
        ifBody += IR_VariableDeclaration(handlesCurve)
        for (dim <- 0 to numDims) {
          ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableData_alloc"), IR_AddressOf(IR_ArrayAccess(handles, dim)))
        }

        // pass pointers of coordinate arrays to handles
        val curveCoordsDecl = field.layout.localization match {
          case IR_AtNode              => curveCoordsNodeDecl
          case IR_AtCellCenter        => curveCoordsZoneDecl
          case face : IR_AtFaceCenter => curveCoordsFaceAsVec(face.dim)
        }
        val curveCoords = IR_VariableAccess(curveCoordsDecl)

        val idxTmp = if (numDims == 1) {
          IR_FieldIteratorAccess(0) + fragOffset(0)
        } else {
          numPointsTotalTmp(0) * (IR_FieldIteratorAccess(1) + fragOffset(1)) + IR_FieldIteratorAccess(0) + fragOffset(0)
        }

        val idxField = if (numDims == 1) {
          IR_FieldIteratorAccess(0)
        } else {
          numPointsDimField(0) * IR_FieldIteratorAccess(1) + IR_FieldIteratorAccess(0)
        }

        val offsetToInnerPoints = if (numDims == 1) {
          numOuterLayersLeft(0)
        } else {
          numPointsDimField(0) * numOuterLayersLeft(1) + numOuterLayersLeft(0)
        }

        // copy values to temporary memory (only when necessary)
        val tmpDecl = IR_VariableDeclaration(IR_PointerDatatype(IR_RealDatatype), "tmp")
        if (dataIsCopied) {
          ifBody += tmpDecl
          ifBody += IR_Assignment(
            IR_VariableAccess(tmpDecl),
            IR_Cast(IR_PointerDatatype(IR_RealDatatype), IR_FunctionCall(IR_ExternalFunctionReference("malloc"), numPointsTotalTmp.product * IR_SizeOf(IR_RealDatatype)))
          )

          ifBody += IR_LoopOverFragments(
            IR_LoopOverDimensions(numDims, IR_ExpressionIndexRange(IR_ExpressionIndex(Array.fill[Int](numDims)(0)), IR_ExpressionIndex(numPointsDimTmp)),
              IR_Assignment(
                IR_ArrayAccess(IR_VariableAccess(tmpDecl), idxTmp),
                // TODO: assumes slot = 0
                scaleCurvemesh * IR_LinearizedFieldAccess(field, slot = 0, IR_LoopOverFragments.defIt, idxField + offsetToInnerPoints)
              )
            )
          )
        }

        val variableAccess = if (dataIsCopied) {
          IR_VariableAccess(tmpDecl)
        } else {
          // pass pointer of field if nothing was copied
          // TODO: assumes slot = 0
          IR_IV_FieldData(field, slot = 0)
        }

        // determine whether doubles or floats are sent
        val funcRef = if (Knowledge.useDblPrecision) IR_ExternalFunctionReference("VisIt_VariableData_setDataD") else IR_ExternalFunctionReference("VisIt_VariableData_setDataF")

        for (dim <- 0 until numDims) {
          val curveCoordsAccess = if (numDims == 1) {
            if (Knowledge.numLevels > 1) IR_ArrayAccess(curveCoords, field.level - Knowledge.minLevel) else curveCoords
          } else {
            if (Knowledge.numLevels > 1) {
              IR_MultiDimArrayAccess(curveCoords, IR_ExpressionIndex(Array[IR_Expression](dim, field.level - Knowledge.minLevel)))
            } else {
              IR_ArrayAccess(curveCoords, dim)
            }
          }

          // pass coordinate array, simulation responsible for freeing memory
          ifBody += IR_FunctionCall(funcRef,
            IR_ArrayAccess(handles, dim),
            IR_Native("VISIT_OWNER_SIM"),
            IR_IntegerConstant(1),
            numPointsTotalTmp.product,
            curveCoordsAccess
          )
        }

        // determines whether simulation or VisIt is responsible for freeing
        val ownership = if (dataIsCopied) IR_Native("VISIT_OWNER_VISIT") else IR_Native("VISIT_OWNER_SIM")

        // pass tmp array or field
        ifBody += IR_FunctionCall(funcRef,
          IR_ArrayAccess(handles, numDims),
          ownership,
          IR_IntegerConstant(1),
          numPointsTotalTmp.product,
          variableAccess
        )
        ifBody += funcCall

        fctBody += IR_IfCondition(
          IR_AndAnd(
            stringEquals(IR_VariableAccess("name", IR_StringDatatype), "curv" + (numDims + 1) + "d_" + field.name),
            curLevel EqEq field.level
          ),
          ListBuffer[IR_Statement](
            IR_IfCondition(
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CurvilinearMesh_alloc"), IR_AddressOf(h)) EqEq visitOkay,
              ifBody
            )
          )
        )
      }
    }

    fctBody += IR_Return(h)

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
