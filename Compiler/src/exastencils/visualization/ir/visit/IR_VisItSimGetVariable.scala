package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_FragmentIndex
import exastencils.field.ir._
import exastencils.grid.ir._
import exastencils.visualization.ir.visit.IR_VisItGlobals._


/// IR_VisItSimGetVariable
// register variable for VisIt coupling
// only gets generated when dimensionality is either 2 or 3

case class IR_VisItSimGetVariable() extends IR_VisItFuturePlainFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()
    val h = IR_VariableAccess("h", visitHandle)

    fctBody += IR_VariableDeclaration(h, visitInvalidHandle)

    for (field <- IR_FieldCollection.sortedObjects) {
      val numDims = field.layout.numDimsGrid
      val numPointsDimTmp = (0 until numDims).map(d => field.localization match {
        case IR_AtFaceCenter(`d`) => field.layout.defIdxDupRightEnd(d) - field.layout.defIdxDupLeftBegin(d) - 1 // interpolated
        case _                    => field.layout.defIdxDupRightEnd(d) - field.layout.defIdxDupLeftBegin(d)
      }).toArray
      val numOuterLayersLeft = (0 until numDims).map(d => field.layout.defIdxDupLeftBegin(d) - field.layout.defIdxPadLeftBegin(d)).toArray
      val numOuterLayersRight = (0 until numDims).map(d => field.layout.defIdxPadRightEnd(d) - field.layout.defIdxDupRightEnd(d)).toArray
      val isNodalDim = (0 until numDims).map(d => numPointsDimTmp(d) % 2)
      val numPointsTotalTmp = (0 until numDims).map(d => (Knowledge.domain_rect_numFragsPerBlockAsVec(d) * (numPointsDimTmp(d) - isNodalDim(d))) + isNodalDim(d))

      // determine if data must be copied or not
      val dataIsCopied = if (numOuterLayersLeft.sum != 0 || numOuterLayersRight.sum != 0 || Knowledge.domain_numFragmentsPerBlock > 1) true else false

      val tmp = IR_VariableAccess("tmp", IR_PointerDatatype(IR_RealDatatype))

      // offset to the current fragment
      val fragOffset = (0 until numDims).map(d =>
        if (Knowledge.domain_rect_numFragsPerBlockAsVec(d) <= 1)
          IR_IntegerConstant(0)
        else
          (numPointsDimTmp(d) - isNodalDim(d)) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(d))).toArray

      // indices for array access
      val idxTmp = (0 until numDims).map(dim => {
        (IR_FieldIteratorAccess(dim) + fragOffset(dim)) * (0 until dim).map(numPointsTotalTmp).product : IR_Expression
      }).reduce(_ + _)

      // array accesses depending on number of levels
      val arrayAccess = IR_ArrayAccess(tmp, idxTmp)

      // direct access to field if data is not copied
      val arrayAccessArg = if (!dataIsCopied) {
        // TODO: assumes slot = 0
        IR_IV_FieldData(field, slot = 0)
      } else {
        tmp
      }

      // determine whether simulation or VisIt is responsible for freeing
      val ownership = if (!dataIsCopied) IR_Native("VISIT_OWNER_SIM") else IR_Native("VISIT_OWNER_VISIT")

      // determine whether doubles or floats are sent
      val setData = if (Knowledge.useDblPrecision) IR_ExternalFunctionReference("VisIt_VariableData_setDataD") else IR_ExternalFunctionReference("VisIt_VariableData_setDataF")

      val sendData = IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableData_alloc"), IR_AddressOf(h)) EqEq visitOkay,
        IR_FunctionCall(setData, h,
          ownership, IR_IntegerConstant(1), numPointsTotalTmp.product, arrayAccessArg)
      )

      // no copy needed if offset == 0 and no multiple fragments
      val loopStatement = ListBuffer[IR_Statement]()

      if (!dataIsCopied) {
        loopStatement += sendData
      } else {
        loopStatement += IR_VariableDeclaration(tmp)
        loopStatement += IR_ArrayAllocation(tmp, IR_RealDatatype, numPointsTotalTmp.product)
        loopStatement += IR_LoopOverFragments(
          IR_LoopOverDimensions(numDims, IR_ExpressionIndexRange(IR_ExpressionIndex(Array.fill[Int](numDims)(0)), IR_ExpressionIndex(numPointsDimTmp)),
            IR_Assignment( //copy values from field to tmp
              arrayAccess,
              // TODO: assumes slot = 0
              field.localization match {
                case IR_AtFaceCenter(dim) => // interpolate to cell centered var
                  0.5 * (IR_FieldAccess(field, 0, IR_LoopOverDimensions.defIt(numDims))
                    + IR_FieldAccess(field, 0, IR_LoopOverDimensions.defIt(numDims) + IR_ConstIndex(Array.fill(3)(0).updated(dim, 1))))
                case _ =>
                  IR_FieldAccess(field, slot = 0, IR_LoopOverFragments.defIt, IR_LoopOverDimensions.defIt(numDims))
              })))
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
