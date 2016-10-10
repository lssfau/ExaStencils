package exastencils.interfacing.ir

import exastencils.knowledge.ir.IR_KnowledgeCollection
import exastencils.logger.Logger

/// IR_ExternalFieldCollection

object IR_ExternalFieldCollection extends IR_KnowledgeCollection[IR_ExternalField] {
  exastencils.core.Duplicate.registerConstant(this)

  // special function to find external fields according to their layout
  @deprecated("to be removed", "04.10.16")
  def getByLayoutIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[IR_ExternalField] = {
    val ret = objects.find(field => field.fieldLayout.identifier == identifier && field.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"External field with layout $identifier on level $level was not found")
    ret
  }

  def generateCopyFunction() = {
    sortedObjects.flatMap(extField =>
      List(IR_CopyToExternalField(extField.targetField, extField),
        IR_CopyFromExternalField(extField.targetField, extField)))
  }
}
