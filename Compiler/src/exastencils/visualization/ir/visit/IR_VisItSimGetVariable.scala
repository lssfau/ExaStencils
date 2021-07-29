package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_FragmentIndex
import exastencils.field.ir._

/// IR_VisItSimGetVariable
// register variable for VisIt coupling
// only gets generated when dimensionality is either 2 or 3

case class IR_VisItSimGetVariable() extends IR_FuturePlainVisItFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()
    val h = IR_VariableAccess("h", visitHandle)

    fctBody += IR_VariableDeclaration(h, visitInvalidHandle)

    for (field <- IR_FieldCollection.sortedObjects) {
      val numDims = field.layout.numDimsGrid
      val numPointsDimTmp = (0 until numDims).map(d => field.layout.defIdxDupRightEnd(d) - field.layout.defIdxDupLeftBegin(d)).toArray
      val numPointsDimField = (0 until numDims).map(d => field.layout.defIdxPadRightEnd(d) - field.layout.defIdxPadLeftBegin(d)).toArray
      val numOuterLayersLeft = (0 until numDims).map(d => field.layout.defIdxDupLeftBegin(d) - field.layout.defIdxPadLeftBegin(d)).toArray
      val numOuterLayersRight = (0 until numDims).map(d => field.layout.defIdxPadRightEnd(d) - field.layout.defIdxDupRightEnd(d)).toArray
      val isNodalDim = (0 until numDims).map(d => numPointsDimTmp(d) % 2)
      val numPointsTotalTmp = (0 until numDims).map(d => (Knowledge.domain_rect_numFragsPerBlockAsVec(d) * (numPointsDimTmp(d) - isNodalDim(d))) + isNodalDim(d))

      // determine if data must be copied or not
      val dataIsCopied = if (numOuterLayersLeft.sum != 0 || numOuterLayersRight.sum != 0 || Knowledge.domain_numFragmentsPerBlock > 1) true else false

      val tmpDecl = IR_VariableDeclaration(IR_PointerDatatype(IR_RealDatatype), "tmp")
      // offset depending on number of ghost/pad layers
      val offsetToInnerPoints = if (numDims == 2) {
        numOuterLayersLeft(1) * numPointsDimField(0) + numOuterLayersLeft(0)
      } else {
        numOuterLayersLeft(2) * numPointsDimField(0) * numPointsDimField(1) + numOuterLayersLeft(1) * numPointsDimField(0) + numOuterLayersLeft(0)
      }

      // offset to the current fragment
      val fragOffset = (0 until numDims).map(d => if (Knowledge.domain_rect_numFragsPerBlockAsVec(d) <= 1) IR_IntegerConstant(0)
      else (numPointsDimTmp(d) - isNodalDim(d)) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(d))).toArray

      // indices for array access
      val idxTmp = if (numDims == 2) {
        numPointsTotalTmp(0) * (IR_FieldIteratorAccess(1) + fragOffset(1)) + (IR_FieldIteratorAccess(0) + fragOffset(0))
      } else {
        numPointsTotalTmp(0) * numPointsTotalTmp(1) * (IR_FieldIteratorAccess(2) + fragOffset(2)) + // linearized z
          numPointsTotalTmp(0) * (IR_FieldIteratorAccess(1) + fragOffset(1)) + // linearized y
          (IR_FieldIteratorAccess(0) + fragOffset(0)) // linearized x
      }

      val idxField = if (numDims == 2) {
        numPointsDimField(0) * IR_FieldIteratorAccess(1) + IR_FieldIteratorAccess(0)
      } else {
        numPointsDimField(0) * numPointsDimField(1) * IR_FieldIteratorAccess(2) +
          numPointsDimField(0) * IR_FieldIteratorAccess(1) + IR_FieldIteratorAccess(0)
      }

      // array accesses depending on number of levels
      val arrayAccess = IR_ArrayAccess(IR_VariableAccess(tmpDecl), idxTmp)

      // direct access to field if data is not copied
      val arrayAccessArg = if (!dataIsCopied) {
        // TODO: assumes slot = 0
        IR_IV_FieldData(field, slot = 0)
      } else {
        IR_VariableAccess(tmpDecl)
      }

      // determine whether simulation or VisIt is responsible for freeing
      val ownership = if (!dataIsCopied) IR_Native("VISIT_OWNER_SIM") else IR_Native("VISIT_OWNER_VISIT")

      // determine whether doubles or floats are sent
      val funcRef = if (Knowledge.useDblPrecision) IR_ExternalFunctionReference("VisIt_VariableData_setDataD") else IR_ExternalFunctionReference("VisIt_VariableData_setDataF")

      val sendData = IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableData_alloc"), IR_AddressOf(h)) EqEq visitOkay,
        IR_FunctionCall(funcRef, h,
          ownership, IR_IntegerConstant(1), numPointsTotalTmp.product, arrayAccessArg)
      )

      // no copy needed if offset == 0 and no multiple fragments
      val loopStatement = ListBuffer[IR_Statement]()

      if (!dataIsCopied) {
        loopStatement += sendData
      } else {
        loopStatement += tmpDecl
        loopStatement += IR_Assignment(
          IR_VariableAccess(tmpDecl),
          IR_Cast(IR_PointerDatatype(IR_RealDatatype), IR_FunctionCall(IR_ExternalFunctionReference("malloc"), numPointsTotalTmp.product * IR_SizeOf(IR_RealDatatype)))
        )
        loopStatement += IR_LoopOverFragments(
          IR_LoopOverDimensions(numDims, IR_ExpressionIndexRange(IR_ExpressionIndex(Array.fill[Int](numDims)(0)), IR_ExpressionIndex(numPointsDimTmp)),
            IR_Assignment( //copy values from field to tmp
              arrayAccess,
              // TODO: assumes slot = 0
              IR_LinearizedFieldAccess(field, slot = 0, IR_LoopOverFragments.defIt, idxField + offsetToInnerPoints))))
        loopStatement += sendData
      }

      fctBody += IR_IfCondition(
        IR_AndAnd(
          stringEquals(IR_VariableAccess("name", IR_StringDatatype), field.name),
          field.level EqEq curLevel),
        loopStatement
      )
    }

    fctBody += IR_Return(h)

    IR_PlainFunction(
      name,
      visitHandle,
      ListBuffer(IR_FunctionArgument("domain", IR_IntegerDatatype), IR_FunctionArgument("name", IR_SpecialDatatype("const char*")), IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype))),
      fctBody
    )
  }

  override def name : String = "SimGetVariable"
}
