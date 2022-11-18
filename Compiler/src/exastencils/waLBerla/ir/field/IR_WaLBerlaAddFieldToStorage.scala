package exastencils.waLBerla.ir.field

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaCollection
import exastencils.waLBerla.ir.interfacing._

case class IR_WaLBerlaAddFieldToStorage(wbFields : IR_WaLBerlaField*) extends IR_WaLBerlaFuturePlainFunction {

  def blockForest = IR_WaLBerlaBlockForest()
  def blocks = blockForest.ctorParameter
  def initValue = IR_FunctionArgument("initVal", IR_RealDatatype)

  if (!wbFields.forall(_.name == wbFields.head.name))
    Logger.error("\"IR_WaLBerlaAddGPUFieldToStorage\" used incorrectly. Assumes fields with identical name but potentially different slots and levels.")

  override def isInterfaceFunction : Boolean = false

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    // add deps
    IR_WaLBerlaCollection.get.addExternalDependency("field/Field.h")
    IR_WaLBerlaCollection.get.addExternalDependency("field/AddToStorage.h")

    var params : ListBuffer[IR_FunctionArgument] = ListBuffer()
    params += blocks
    params += initValue

    // calc size function for wb fields
    def calcSize(level : Int) = IR_VariableAccess(s"calcSize_$level", "auto")

    val init = wbFields.sortBy(_.level).flatMap(leveledField => {
      (0 until leveledField.numSlots).map(slot =>
        leveledField.addToStorage(blocks.access, slot, initValue, calcSize(leveledField.level)))
    })

    var body : ListBuffer[IR_Statement] = ListBuffer()

    // set up calcSize function
    for (wbf <- wbFields.sortBy(_.level)) {
      val func = IR_WaLBerlaGetSizeForLevel(wbf.level)
      if (!IR_WaLBerlaCollection.get.functions.contains(func))
        IR_WaLBerlaCollection.get.functions += func

      body += IR_VariableDeclaration(calcSize(wbf.level),
        IR_FunctionCall(IR_ExternalFunctionReference("std::bind"), func.getReference,
          // use placeholders for:
          IR_Native("std::placeholders::_1"), // blockstorage
          IR_Native("std::placeholders::_2") // iblock
        ))
    }

    body += IR_Return(IR_InitializerList(init : _*))

    val returnType = IR_WaLBerlaBlockDataID(wbFields.head, slot = 0).datatype

    IR_WaLBerlaPlainFunction(name, returnType, params, body)
  }

  override def prettyprint_decl() : String = prettyprint()
  override def name : String = s"addToStorage_${ wbFields.head.name }"
  override def name_=(newName : String) : Unit = name = newName
}
