package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.field.ir._
import exastencils.globals.ir._

object IR_WaLBerlaInitFunctionCollection {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaInitBlockForest()
  functions += IR_WaLBerlaDeResizeBuffersWrapper()
  functions += IR_WaLBerlaInitStaticRectDomain()
  if (IR_WaLBerlaUtil.initCommSchemes)
    functions += IR_WaLBerlaInitCommSchemes()

  functions += IR_WaLBerlaDeInitWrapperBuffers()
}

object IR_WaLBerlaDeInitFunctionCollection {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaDeDestroyBuffersWrapper()
}

sealed trait IR_WaLBerlaDeInitWrapperFunction extends IR_WaLBerlaFuturePlainFunction {
  override def isInterfaceFunction : Boolean = true
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint
}

// call resize funcs for variable field sizes
private case class IR_WaLBerlaDeResizeBuffersWrapper() extends IR_WaLBerlaDeInitWrapperFunction {
  override def name : String = "resizeExaBuffers"

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    val blockForest = IR_WaLBerlaBlockForest()
    var body = ListBuffer[IR_Statement]()

    // TODO: adapt for different blocksizes
    if (IR_FieldCollection.objects.nonEmpty && Knowledge.data_genVariableFieldSizes) {
      for (lvl <- IR_FieldCollection.objects.groupBy(_.level).keys)
        body += IR_FunctionCall(IR_LeveledInternalFunctionReference("resizeAllInner", lvl, IR_UnitDatatype),
          (0 until Knowledge.dimensionality).map(blockForest.getNumberOfCells).to[ListBuffer])
    }

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)
  }
}

// wrappers for exa (de-)init functions
private case class IR_WaLBerlaDeInitWrapperBuffers() extends IR_WaLBerlaDeInitWrapperFunction {
  override def name : String = "setupExaBuffers"

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    var body = ListBuffer[IR_Statement]()

    body += IR_FunctionCall(IR_AllocateDataFunction.fctName)
    if (IR_FieldCollection.objects.nonEmpty && Knowledge.data_initAllFieldsWithZero)
      body += IR_FunctionCall(IR_InitFieldsWithZero().name)

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)
  }
}

private case class IR_WaLBerlaDeDestroyBuffersWrapper() extends IR_WaLBerlaDeInitWrapperFunction {
  override def name : String = "cleanupExaBuffers"

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction =
    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(),
      ListBuffer[IR_Statement](IR_FunctionCall("destroyGlobals")))
}

