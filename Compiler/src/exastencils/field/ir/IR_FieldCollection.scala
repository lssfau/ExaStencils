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

package exastencils.field.ir

import exastencils.base.ir._
import exastencils.knowledge.ir.IR_LeveledKnowledgeCollection
import exastencils.logger.Logger

object IR_FieldCollection extends IR_LeveledKnowledgeCollection[IR_Field] {
  exastencils.core.Duplicate.registerConstant(this)

  // special handling for level expressions
  def getByIdentifierLevExp(identifier : String, level : IR_Expression, suppressError : Boolean = false) : Option[IR_Field] = {
    level match {
      case IR_IntegerConstant(constLevel) => getByIdentifier(identifier, constLevel.toInt, suppressError)
      case _                              =>
        if (!suppressError) Logger.warn(s"Trying to find field $identifier on level ${ level.prettyprint } - non-constant levels are not supported")
        None
    }
  }

  // special function to find fields according to their layout
  @deprecated("to be removed", "04.10.16")
  def getByLayoutIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[IR_Field] = {
    val ret = objects.find(field => field.layout.name == identifier && field.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"External field with layout $identifier on level $level was not found")
    ret
  }

  def remove(oldObj : IR_Field) : Unit = objects -= oldObj
}
