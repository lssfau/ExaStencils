package exastencils.waLBerla.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Cast
import exastencils.base.ir.IR_ConstReferenceDatatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_LeveledInternalFunctionReference
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnitDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.config.Knowledge
import exastencils.field.ir.IR_FieldLayoutLike
import exastencils.field.ir.IR_FieldLike
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes.WB_BlockDataID
import exastencils.waLBerla.ir.IR_WaLBerlaUtil._

// store context

case class IR_WaLBerlaInterfaceGenerationContext(var functions : ListBuffer[IR_WaLBerlaFunction]) {

  IR_CollectAccessedWaLBerlaFields.applyStandalone(functions)
  var wbFieldAccesses = IR_CollectAccessedWaLBerlaFields.wbFieldAccesses
  var wbFieldNames : ListBuffer[String] = wbFieldAccesses.map(_.name)

  IR_CollectAccessedLowerLevelWaLBerlaFields.applyStandalone(functions)
  var lowerLevelFieldAccs =
    IR_CollectAccessedLowerLevelWaLBerlaFields.fieldAccesses.groupBy(f => f.name + f.level).map(_._2.head).to[ListBuffer]

  private def toBlockDataID(name : String) = IR_VariableAccess(name + "_ID", WB_BlockDataID)

  def blockDataIDs : Map[String, IR_FunctionArgument] = wbFieldNames.sorted.map(acc => acc -> IR_FunctionArgument(toBlockDataID(acc))).toMap

  // ctor params and members
  var ctorParams : ListBuffer[IR_FunctionArgument] = ListBuffer()
  var members : ListBuffer[IR_VariableAccess] = ListBuffer()

  // block data IDs and params of waLBerla function
  ctorParams ++= blockDataIDs.values
  members ++= blockDataIDs.values.map(arg => IR_VariableAccess(getGeneratedName(arg.name), arg.datatype))

  // block storage shared_ptr
  ctorParams += IR_FunctionArgument(IR_VariableAccess(blockStoragePtr.name, IR_ConstReferenceDatatype(blockStoragePtr.datatype)))
  members += IR_VariableAccess(getGeneratedName(blockStoragePtr.name), blockStoragePtr.datatype)

  // init global layout variables for waLBerla fields and their lower-level counterparts
  if (lowerLevelFieldAccs.nonEmpty && !Knowledge.data_genVariableFieldSizes)
    Logger.error("Knowledge flag \"data_genVariableFieldSizes\" must be enabled when multi-leveled waLBerla fields are present.")
  private def newInnerSize(d: Int, origLayout : IR_FieldLayoutLike, lvlDiff : Int) = IR_Cast(IR_IntegerDatatype, IR_SimplifyExpression.simplifyIntegralExpr(
    origLayout.defIdxById("IE", d) - origLayout.defIdxById("IB", d)) / Math.pow(2, lvlDiff).toInt)
  var ctorBody : ListBuffer[IR_Statement] = ListBuffer()
  if (Knowledge.data_genVariableFieldSizes) {
    var resizeForLevels : mutable.HashMap[IR_FieldLike, Int] = mutable.HashMap()
    resizeForLevels ++= lowerLevelFieldAccs.collect { case f => (f, f.level) }.groupBy(_._2).map(_._2.head)
    resizeForLevels.put(wbFieldAccesses.head, wbFieldAccesses.head.level)

    resizeForLevels foreach { fieldMap =>
      val maxLevelField = wbFieldAccesses.find(f => fieldMap._1.name == f.name).get
      ctorBody += IR_FunctionCall(
        IR_LeveledInternalFunctionReference("resizeAllInner", fieldMap._2, IR_UnitDatatype),
        Knowledge.dimensions.map(dim => newInnerSize(dim, maxLevelField.layout, maxLevelField.level - fieldMap._2) : IR_Expression).to[ListBuffer]
      )
    }
  }
  ctorBody = ListBuffer(IR_WaLBerlaLoopOverBlocks(ctorBody).expandSpecial().inner : IR_Statement)
}
