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

case class IR_VisItSimGetVariable() extends IR_FuturePlainFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()
    val h_decl = IR_VariableDeclaration(visitHandle, "h", visitInvalidHandle)

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
      else
        IR_VariableAccess(tmp_decl)

      // determine whether simulation or VisIt is responsible for freeing
      val ownership = if (!dataIsCopied) IR_Native("VISIT_OWNER_SIM") else IR_Native("VISIT_OWNER_VISIT")

      // determine whether doubles or floats are sent
      val funcRef = if (Knowledge.useDblPrecision) IR_ExternalFunctionReference("VisIt_VariableData_setDataD") else IR_ExternalFunctionReference("VisIt_VariableData_setDataF")

      val sendData = IR_IfCondition(
        IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableData_alloc"), IR_AddressOf(IR_VariableAccess(h_decl))) EqEq visitOkay,
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
      name,
      visitHandle,
      ListBuffer(IR_FunctionArgument("domain", IR_IntegerDatatype), IR_FunctionArgument("name", IR_SpecialDatatype("const char*")), IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype))),
      fctBody
    )
  }


  override def name : String = "SimGetVariable"
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint()
}
