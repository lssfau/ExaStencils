package exastencils.waLBerla.ir.util

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_GeneralSimplify

object IR_WaLBerlaUtil {

  def make_shared(templateDt : String, args : IR_Expression*) =
    IR_FunctionCall(IR_ExternalFunctionReference(s"std::make_shared< $templateDt >"), args : _*)
  def make_unique(templateDt : String, args : IR_Expression*) =
    IR_FunctionCall(IR_ExternalFunctionReference(s"std::make_unique< $templateDt >"), args : _*)

  def initCommSchemes = true // TODO: adapt condition

  def memberSuffix = "_gen"
  def getGeneratedName(s : String) : String = s + memberSuffix

  def adaptIndexForAccessors(index : IR_ExpressionIndex, gridDatatype : IR_Datatype, numDimsGrid : Int, numDimsData : Int) = {
    // add zero entries for grid dims < 3
    var newIndex = Duplicate(index)
    newIndex = IR_ExpressionIndex(
      index.indices.take(numDimsGrid) ++
        Array.fill(3 - numDimsGrid)(0 : IR_Expression) ++
        Duplicate(index).indices.drop(numDimsGrid))

    // indices need to be flattened
    val linearizedHigherDimIndex = if (numDimsData > numDimsGrid) {
      gridDatatype match {
        case mat : IR_MatrixDatatype =>
          val matIndices = newIndex.indices.takeRight(2)
          IR_ExpressionIndex( newIndex.indices.dropRight(2) :+ (matIndices(1) + matIndices(0) * mat.sizeN ) )
        case _ : IR_ScalarDatatype   =>
          newIndex
        // TODO: other datatypes?
        case _                       =>
          Logger.error("Unsupported higher dimensional datatype.")
      }
    } else {
      newIndex
    }
    IR_GeneralSimplify.doUntilDoneStandalone(linearizedHigherDimIndex)

    linearizedHigherDimIndex
  }
}
