package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.logger.Logger

case class IR_WaLBerlaAddFieldToStorage(wbFields : ListBuffer[IR_WaLBerlaField]) extends IR_WaLBerlaFuturePlainFunction {

  def blockForest = IR_WaLBerlaBlockForest()
  def blocks = blockForest.ctorParameter
  def initValue = IR_FunctionArgument("initVal", IR_RealDatatype)

  override def generateFct() : IR_WaLBerlaPlainFunction = {
    // add deps
    IR_WaLBerlaCollection.get.addExternalDependency("field/Field.h")
    IR_WaLBerlaCollection.get.addExternalDependency("field/AddToStorage.h")

    var params : ListBuffer[IR_FunctionArgument] = ListBuffer()
    params += blocks
    params += initValue

    // calc size function for leveled wb fields
    def calcSizeNeeded = wbFields.size > 1
    def calcSize(level : Int) = {
      if (calcSizeNeeded)
        Some(IR_VariableAccess(s"calcSize_$level", "auto"))
      else
        None
    }
    def calcSizeFunction = IR_ExternalFunctionReference(
      s"pde::VCycles< ${IR_WaLBerlaUtil.stencilTemplate(wbFields.head.numDimsGrid)} >::getSizeForLevel",
    )

    val init = wbFields.sortBy(_.level).flatMap(leveledField => {
      (0 until leveledField.numSlots).map(slot =>
        leveledField.addToStorage(blocks.access, slot, initValue, calcSize(leveledField.level)))
    })

    var body : ListBuffer[IR_Statement] = ListBuffer()

    if (calcSizeNeeded) {
      val maxLevel = if (blockForest.maxLevelWaLBerlaField.isDefined) {
        blockForest.maxLevelWaLBerlaField.get.level
      } else {
        Logger.error("AddFieldToStorage not applicable without waLBerla fields")
      }

      // add dependency
      IR_WaLBerlaCollection.get.addExternalDependency("pde/iterations/VCycles.h")

      for (wbf <- wbFields.sortBy(_.level)) {
        body += IR_VariableDeclaration(calcSize(wbf.level).get,
          IR_FunctionCall(IR_ExternalFunctionReference("std::bind"), IR_AddressOf(IR_VariableAccess(calcSizeFunction.name, calcSizeFunction.returnType)),
            maxLevel - wbf.level, // bind level
            // use placeholders for
            IR_Native("std::placeholders::_1"), // blockstorage
            IR_Native("std::placeholders::_2") // iblock
          ))
      }
    }

    body += IR_Return(IR_InitializerList(init :_*))

    val returnType = IR_WaLBerlaBlockDataID(wbFields.head, slot = 0).datatype

    val func = IR_WaLBerlaPlainFunction(name, returnType, params, body)
    func.isInterfaceFunction = false
    func
  }

  override def prettyprint_decl() : String = prettyprint()
  override def name : String = s"addToStorage_${wbFields.head.name}"
  override def name_=(newName : String) : Unit = name = newName
}
