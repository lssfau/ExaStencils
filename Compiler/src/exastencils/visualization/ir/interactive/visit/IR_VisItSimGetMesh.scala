package exastencils.visualization.ir.interactive.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_FragmentIndex
import exastencils.field.ir._
import exastencils.grid.ir._
import exastencils.visualization.ir.interactive.visit.IR_VisItGlobals._

/// IR_VisItSimGetMesh
// provide mesh for VisIt

case class IR_VisItSimGetMesh() extends IR_VisItFuturePlainFunction {

  import exastencils.visualization.ir.interactive.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()
    val h = IR_VariableAccess("h", visitHandle)
    val handles = IR_VariableAccess("handles", IR_ArrayDatatype(visitHandle, Knowledge.dimensionality))

    fctBody += IR_VariableDeclaration(h, visitInvalidHandle)

    // 1d case only produce a curvilinear mesh
    if (useRectMesh()) {
      for (coords <- coordsArrays) {
        val curCoords = coordsArrays.indexOf(coords)
        for (level <- Knowledge.minLevel to Knowledge.maxLevel) {
          val numPointsDim = (0 until Knowledge.dimensionality).map(d =>
            Knowledge.domain_fragmentLengthAsVec(d) * Knowledge.domain_rect_numFragsPerBlockAsVec(d) * (1 << level) + isNodalInDim(curCoords)(d))

          // coordinate setter function depending on dimensionality
          val handlesAccessDim = (0 until Knowledge.dimensionality).map(d => IR_ArrayAccess(handles, d)).toArray
          val funcCall = if (Knowledge.dimensionality == 2) {
            callExtFunction("VisIt_RectilinearMesh_setCoordsXY", h, handlesAccessDim(0), handlesAccessDim(1))
          } else {
            callExtFunction("VisIt_RectilinearMesh_setCoordsXYZ", h, handlesAccessDim(0), handlesAccessDim(1), handlesAccessDim(2))
          }

          val ifBody = ListBuffer[IR_Statement]()
          // allocate handles
          ifBody += IR_VariableDeclaration(handles)
          for (dim <- 0 until Knowledge.dimensionality) {
            ifBody += callExtFunction("VisIt_VariableData_alloc", IR_AddressOf(IR_ArrayAccess(handles, dim)))
          }

          // pass pointers of coordinate arrays to handles
          for (dim <- 0 until Knowledge.dimensionality) {
            // array access depending on number of levels
            val coordsAccess = if (Knowledge.numLevels > 1) {
              IR_MultiDimArrayAccess(coords, IR_ExpressionIndex(Array[IR_Expression](dim, level - Knowledge.minLevel)))
            } else {
              IR_ArrayAccess(coords, dim)
            }

            ifBody += IR_FunctionCall(setVariableDataFunc,
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
              stringEquals(IR_VariableAccess("name", IR_StringDatatype), meshname(coords)),
              curLevel EqEq level
            ),
            ListBuffer[IR_Statement](
              IR_IfCondition(
                callExtFunction("VisIt_RectilinearMesh_alloc", IR_AddressOf(h)) EqEq visitOkay,
                ifBody))
          )
        }
      }
    }

    // curvilinear mesh construction for 1d and 2d problems
    if (useCurveMesh()) {
      for (field <- IR_FieldCollection.sortedObjects) {
        val numDims = field.layout.numDimsGrid
        val handlesCurve = IR_VariableAccess("handles", IR_ArrayDatatype(visitHandle, numDims + 1))
        val numPointsDimTmp = (0 until numDims).map(d => field.layout.defIdxDupRightEnd(d) - field.layout.defIdxDupLeftBegin(d)).toArray
        val numOuterLayersLeft = (0 until numDims).map(d => field.layout.defIdxDupLeftBegin(d) - field.layout.defIdxPadLeftBegin(d)).toArray
        val numOuterLayersRight = (0 until numDims).map(d => field.layout.defIdxPadRightEnd(d) - field.layout.defIdxDupRightEnd(d)).toArray
        val isNodalDim = (0 until numDims).map(d => numPointsDimTmp(d) % 2)
        val fragOffset = (0 until numDims).map(d => if (Knowledge.domain_rect_numFragsPerBlockAsVec(d) <= 1) IR_IntegerConstant(0)
        else (numPointsDimTmp(d) - isNodalDim(d)) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(d)))
        val numPointsTotalTmp = (0 until numDims).map(d => Knowledge.domain_rect_numFragsPerBlockAsVec(d) * (numPointsDimTmp(d) - isNodalDim(d)) + isNodalDim(d))

        // determine if data must be copied or not
        val dataIsCopied = numOuterLayersLeft.sum != 0 || numOuterLayersRight.sum != 0 || Knowledge.domain_numFragmentsPerBlock > 1

        val ifBody = ListBuffer[IR_Statement]()
        // dimensionality that must be passed -> for last dimension its 1 because we send 1 variable value per mesh point
        val dims = IR_VariableAccess("dims", IR_ArrayDatatype(IR_IntegerDatatype, numDims + 1))
        ifBody += IR_VariableDeclaration(dims)
        for (dim <- 0 until numDims) {
          ifBody += IR_Assignment(IR_ArrayAccess(dims, dim), numPointsTotalTmp(dim))
        }
        ifBody += IR_Assignment(IR_ArrayAccess(dims, numDims), 1)

        val funcCall = if (numDims == 1) {
          callExtFunction("VisIt_CurvilinearMesh_setCoordsXY", h, dims, IR_ArrayAccess(handlesCurve, 0), IR_ArrayAccess(handlesCurve, 1))
        } else {
          callExtFunction("VisIt_CurvilinearMesh_setCoordsXYZ", h, dims, IR_ArrayAccess(handlesCurve, 0), IR_ArrayAccess(handlesCurve, 1), IR_ArrayAccess(handlesCurve, 2))
        }

        // allocate handles
        ifBody += IR_VariableDeclaration(handlesCurve)
        for (dim <- 0 to numDims) {
          ifBody += callExtFunction("VisIt_VariableData_alloc", IR_AddressOf(IR_ArrayAccess(handles, dim)))
        }

        // pass pointers of coordinate arrays to handles
        val curveCoords = field.layout.localization match {
          case IR_AtNode              => curveCoordsNode
          case IR_AtCellCenter        => curveCoordsZone
          case face : IR_AtFaceCenter => curveCoordsFaceAsVec(face.dim)
        }

        val idxTmp = (0 until numDims).map(d =>
          (0 until d).map(numPointsTotalTmp).product * (IR_FieldIteratorAccess(d) + fragOffset(d)) : IR_Expression).reduce(_ + _)

        // copy values to temporary memory (only when necessary)
        val tmp = IR_VariableAccess("tmp", IR_PointerDatatype(IR_RealDatatype))
        if (dataIsCopied) {
          ifBody += IR_VariableDeclaration(tmp)
          ifBody += IR_ArrayAllocation(tmp, IR_RealDatatype, numPointsTotalTmp.product)

          ifBody += IR_LoopOverFragments(
            IR_LoopOverDimensions(numDims, IR_ExpressionIndexRange(IR_ExpressionIndex(Array.fill[Int](numDims)(0)), IR_ExpressionIndex(numPointsDimTmp)),
              IR_Assignment(
                IR_ArrayAccess(tmp, idxTmp),
                scaleCurvemesh * IR_FieldAccess(field, getAndCapSlot(field), IR_LoopOverFragments.defIt, IR_LoopOverDimensions.defIt(numDims)))))
        }

        val variableAccess = if (dataIsCopied) {
          tmp
        } else {
          // pass pointer of field if nothing is copied
          IR_IV_FieldData(field, getAndCapSlot(field))
        }

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
          ifBody += IR_FunctionCall(setVariableDataFunc,
            IR_ArrayAccess(handles, dim),
            IR_Native("VISIT_OWNER_SIM"),
            IR_IntegerConstant(1),
            numPointsTotalTmp.product,
            curveCoordsAccess
          )
        }

        // pass tmp array or field
        ifBody += IR_FunctionCall(setVariableDataFunc,
          IR_ArrayAccess(handles, numDims),
          ownership(dataIsCopied),
          IR_IntegerConstant(1),
          numPointsTotalTmp.product,
          variableAccess
        )
        ifBody += funcCall

        fctBody += IR_IfCondition(
          IR_AndAnd(
            stringEquals(IR_VariableAccess("name", IR_StringDatatype), curvname(numDims, field.name)),
            curLevel EqEq field.level
          ),
          ListBuffer[IR_Statement](
            IR_IfCondition(
              callExtFunction("VisIt_CurvilinearMesh_alloc", IR_AddressOf(h)) EqEq visitOkay,
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
}
