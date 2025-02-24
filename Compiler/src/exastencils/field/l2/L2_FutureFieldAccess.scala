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

package exastencils.field.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.field.l3._
import exastencils.knowledge.l2.L2_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_FutureFieldAccess

case class L2_FutureFieldAccess(
    var name : String, var level : Int,
    var offset : Option[L2_ConstIndex] = None,
    var frozen : Boolean = false) extends L2_FutureKnowledgeAccess with L2_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    if (frozen) out << "frozen ( "
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (frozen) out << " )"
  }

  override def progress = ProgressLocation {
    Logger.warn(s"Trying to progress future field access to $name on level $level")
    L3_FutureFieldAccess(name, level,
      L3_ActiveSlot,
      L2_ProgressOption(offset)(_.progress),
      frozen)
  }
}

/// L2_ResolveFrozenFields

object L2_ResolveFrozenFields extends DefaultStrategy("Resolve frozen field accesses") {
  this += new Transformation("Resolve", {
    case fct : L2_FunctionCall if "frozen" == fct.name =>
      if (fct.arguments.length != 1) Logger.error("Calls to frozen need exactly one argument")
      if (!fct.arguments.head.isInstanceOf[L2_FutureFieldAccess]) Logger.error("Calls to frozen must done with exactly one field access")
      val fieldAccess = fct.arguments.head.asInstanceOf[L2_FutureFieldAccess]
      fieldAccess.frozen = true
      fieldAccess
  })
}
