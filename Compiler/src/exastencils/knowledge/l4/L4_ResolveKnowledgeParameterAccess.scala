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

package exastencils.knowledge.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.config._
import exastencils.datastructures._
import exastencils.logger.Logger

object L4_ResolveKnowledgeParameterAccess extends DefaultStrategy("Resolve accesses to knowledge, settings and platform parameters") {
  def resolveParameterToConstant(obj : AnyRef, ident : String) : L4_Expression = {
    obj.getClass.getMethod(ident).invoke(obj) match {
      case value : java.lang.Integer => L4_IntegerConstant(value.toInt)
      case value : java.lang.Float   => L4_RealConstant(value.toFloat)
      case value : java.lang.Boolean => L4_BooleanConstant(value)
      case value : String            => L4_StringConstant(value) // String is already a subclass of object
      case _                         => Logger.error(s"Trying to access parameter $ident from L4 with unsupported type")
    }
  }

  this += new Transformation("special functions and constants", {
    // get knowledge/settings/platform
    case L4_FunctionCall(L4_UnresolvedFunctionReference("getKnowledge", None, None), ListBuffer(L4_StringConstant(ident))) =>
      resolveParameterToConstant(Knowledge, ident)
    case L4_FunctionCall(L4_UnresolvedFunctionReference("getSetting", None, None), ListBuffer(L4_StringConstant(ident)))   =>
      resolveParameterToConstant(Settings, ident)
    case L4_FunctionCall(L4_UnresolvedFunctionReference("getPlatform", None, None), ListBuffer(L4_StringConstant(ident)))  =>
      resolveParameterToConstant(Platform, ident)
  })
}
