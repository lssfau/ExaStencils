package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.config.Knowledge
import exastencils.field.ir.IR_FieldCollection
import exastencils.field.ir.IR_InitFieldsWithZero
import exastencils.globals.ir.IR_AllocateDataFunction
import exastencils.logger.Logger
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest

// collection with setup functions for coupling
object IR_WaLBerlaInitCouplingWrapperFunctions {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  if (Knowledge.domain_rect_generate)
    functions += IR_WaLBerlaInitStaticRectDomain()
  else
    Logger.error("waLBerla coupling is currently only available for rectilinear domains (i.e. domain_rect_generate = true)")

  functions += IR_WaLBerlaResizeExaBuffersWrapper()
  functions += IR_WaLBerlaInitExaBuffersWrapper()
}

// call resize funcs for variable field sizes
private case class IR_WaLBerlaResizeExaBuffersWrapper() extends IR_WaLBerlaWrapperFunction {
  override def name : String = "resizeExaBuffers"

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    val blockForest = IR_WaLBerlaBlockForest()
    var body = ListBuffer[IR_Statement]()

    // TODO: adapt for different blocksizes
    if (IR_FieldCollection.objects.nonEmpty && Knowledge.data_genVariableFieldSizes) {
      for (lvl <- IR_FieldCollection.objects.groupBy(_.level).keys)
        body += IR_FunctionCall(IR_LeveledInternalFunctionReference("resizeAllInner", lvl, IR_UnitDatatype),
          (0 until Knowledge.dimensionality).map(d => blockForest.getNumberOfCellsPerBlock(d) / Math.pow(2, Knowledge.maxLevel - lvl).toInt : IR_Expression).to[ListBuffer])
    }

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)
  }

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true
}

object IR_WaLBerlaInitExaBuffersWrapper {
  def fctName : String = "initExaBuffers"
}

case class IR_WaLBerlaInitExaBuffersWrapper() extends IR_WaLBerlaWrapperFunction {
  override def name : String = IR_WaLBerlaInitExaBuffersWrapper.fctName

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    var body = ListBuffer[IR_Statement]()

    body += IR_FunctionCall(IR_AllocateDataFunction.fctName)
    if (IR_FieldCollection.objects.nonEmpty) {
      if (Knowledge.data_initAllFieldsWithZero)
        body += IR_FunctionCall(IR_InitFieldsWithZero().name)
      if (IR_UserFunctions.get.functions.exists(_.name == "InitFields"))
        body += IR_FunctionCall("InitFields")
    }

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)
  }

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true
}