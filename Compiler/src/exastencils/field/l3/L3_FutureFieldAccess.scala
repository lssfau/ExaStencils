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

package exastencils.field.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.fieldlike.l4.L4_FutureFieldLikeAccess
import exastencils.knowledge.l3.L3_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_FutureFieldAccess

case class L3_FutureFieldAccess(
    var name : String, var level : Int,
    var slot : L3_SlotSpecification,
    var offset : Option[L3_ConstIndex] = None,
    var frozen : Boolean = false) extends L3_FutureKnowledgeAccess with L3_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    if (frozen) out << "frozen ( "
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (frozen) out << " )"
  }

  override def progress = ProgressLocation {
    Logger.warn(s"Trying to progress future field access to $name on level $level")
    L4_FutureFieldLikeAccess(name, level,
      slot.progress,
      L3_ProgressOption(offset)(_.progress),

      frozen)
  }
}

/// L3_ResolveFrozenFields

object L3_ResolveFrozenFields extends DefaultStrategy("Resolve frozen field accesses") {
  this += new Transformation("Resolve", {
    case fct : L3_FunctionCall if "frozen" == fct.name =>
      if (fct.arguments.length != 1) Logger.error("Calls to frozen need exactly one argument")
      if (!fct.arguments.head.isInstanceOf[L3_FutureFieldAccess]) Logger.error("Calls to frozen must done with exactly one field access")
      val fieldAccess = fct.arguments.head.asInstanceOf[L3_FutureFieldAccess]
      fieldAccess.frozen = true
      fieldAccess
  })
}
