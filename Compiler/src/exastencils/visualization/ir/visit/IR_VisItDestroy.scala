package exastencils.visualization.ir.visit

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._

case class IR_VisItDestroy() extends IR_FuturePlainFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction = {

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
      name,
      IR_UnitDatatype,
      fctBody
    )
  }

  override def name : String = "visit_destroy"
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint()
}
