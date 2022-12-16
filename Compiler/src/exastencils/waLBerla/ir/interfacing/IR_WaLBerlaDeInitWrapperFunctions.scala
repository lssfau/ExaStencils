package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.config.Knowledge
import exastencils.field.ir._
import exastencils.globals.ir._
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaInitBlockForest
import exastencils.waLBerla.ir.communication.IR_WaLBerlaInitCommSchemes
import exastencils.waLBerla.ir.cuda.CUDA_WaLBerlaAddGPUFieldToStorage
import exastencils.waLBerla.ir.field._
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

object IR_WaLBerlaInitFunctionCollection {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaInitGlobalsWrapper()
  functions += IR_WaLBerlaInitBlockForest()
  for (field <- IR_WaLBerlaFieldCollection.objects.groupBy(_.name)) {
    val leveledFields = field._2.groupBy(_.level).map(_._2.head).to[ListBuffer]
    functions += IR_WaLBerlaAddFieldToStorage(leveledFields : _*)

    if (Knowledge.cuda_enabled)
      functions += CUDA_WaLBerlaAddGPUFieldToStorage(leveledFields : _*)
  }
  functions += IR_WaLBerlaDeResizeBuffersWrapper()
  functions += IR_WaLBerlaInitStaticRectDomain()
  if (IR_WaLBerlaUtil.initCommSchemes)
    functions += IR_WaLBerlaInitCommSchemes()

  functions += IR_WaLBerlaInitBuffersWrapper()
}

object IR_WaLBerlaDeInitFunctionCollection {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaDeDestroyBuffersWrapper()
}

// TODO: initGeometry

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
          (0 until Knowledge.dimensionality).map(d => blockForest.getNumberOfCells(d) / Math.pow(2, Knowledge.maxLevel - lvl).toInt : IR_Expression).to[ListBuffer])
    }

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), body)
  }
}

// wrappers for exa (de-)init functions

private case class IR_WaLBerlaInitGlobalsWrapper() extends IR_WaLBerlaDeInitWrapperFunction {
  override def name : String = "initExaGlobals"

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction =
    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(),
      ListBuffer[IR_Statement](IR_FunctionCall("initGlobals")))
}

private case class IR_WaLBerlaInitBuffersWrapper() extends IR_WaLBerlaDeInitWrapperFunction {
  override def name : String = "setupExaBuffers"

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
}

private case class IR_WaLBerlaDeDestroyBuffersWrapper() extends IR_WaLBerlaDeInitWrapperFunction {
  override def name : String = "cleanupExaBuffers"

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction =
    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(),
      ListBuffer[IR_Statement](IR_FunctionCall("destroyGlobals")))
}

