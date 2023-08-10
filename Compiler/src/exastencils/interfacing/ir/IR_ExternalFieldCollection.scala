//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.interfacing.ir

import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.knowledge.ir.IR_KnowledgeCollection
import exastencils.logger.Logger
import exastencils.scheduling.NoStrategyWrapper

/// IR_ExternalFieldCollection

object IR_ExternalFieldCollection extends IR_KnowledgeCollection[IR_ExternalField] {
  exastencils.core.Duplicate.registerConstant(this)

  // special function to find external fields according to their layout
  @deprecated("to be removed", "04.10.16")
  def getByLayoutIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[IR_ExternalField] = {
    val ret = objects.find(field => field.fieldLayout.name == identifier && field.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"External field with layout $identifier on level $level was not found")
    ret
  }

  def generateCopyFunction() = {
    sortedObjects.flatMap(extField =>
      List(IR_CopyToExternalField(extField.targetField, extField),
        IR_CopyFromExternalField(extField.targetField, extField)))
  }
}

/// IR_SetupExternalCopyFunctionsWrapper

object IR_SetupExternalCopyFunctionsWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => IR_ExternalFieldCollection.generateCopyFunction().foreach(IR_UserFunctions.get += _)
}
