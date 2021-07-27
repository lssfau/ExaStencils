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
    for (coordsDecl <- coordsArrays.distinct) {
      for (dim <- 0 until Knowledge.dimensionality) {
        val arrayIndices = ArrayBuffer[IR_Expression]()
        if (Knowledge.dimensionality > 1) arrayIndices += IR_IntegerConstant(dim)
        if (Knowledge.numLevels > 1) arrayIndices += IR_LoopOverLevels.defIt - Knowledge.minLevel

        if (Knowledge.dimensionality > 1) {
          val coordsAccess = if (arrayIndices.isEmpty) {
            IR_VariableAccess(coordsDecl)
          } else if (arrayIndices.length == 1) {
            IR_ArrayAccess(IR_VariableAccess(coordsDecl), arrayIndices.head)
          } else {
            IR_MultiDimArrayAccess(IR_VariableAccess(coordsDecl), IR_ExpressionIndex(arrayIndices.toArray))
          }

          fctBody += IR_LoopOverLevels(
            IR_IfCondition(
              coordsAccess,
              ListBuffer[IR_Statement](
                IR_ArrayFree(coordsAccess),
                IR_Assignment(coordsAccess, IR_IntegerConstant(0))))
          )
        }

        // free curve coords
        if (Knowledge.dimensionality < 3) {
          val curveCoordsDecl = IR_VariableDeclaration(coordsDecl.datatype, "curve_" + coordsDecl.name)
          val curveCoordsAccess = if (arrayIndices.isEmpty) {
            IR_VariableAccess(curveCoordsDecl)
          } else if (arrayIndices.length == 1) {
            IR_ArrayAccess(IR_VariableAccess(curveCoordsDecl), arrayIndices.head)
          } else {
            IR_MultiDimArrayAccess(IR_VariableAccess(curveCoordsDecl), IR_ExpressionIndex(arrayIndices.toArray))
          }

          fctBody += IR_LoopOverLevels(
            IR_IfCondition(
              curveCoordsAccess,
              ListBuffer[IR_Statement](
                IR_ArrayFree(curveCoordsAccess),
                IR_Assignment(curveCoordsAccess, IR_IntegerConstant(0))))
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
