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

package exastencils.field.l4

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_FutureFieldAccess

case class L4_FutureFieldAccess(
    var name : String,
    var level : Int,
    var slot : L4_SlotSpecification,
    var offset : Option[L4_ConstIndex] = None,
    var frozen : Boolean = false,
    var matIndex : Option[L4_MatIndex] = None
) extends L4_FutureKnowledgeAccess with L4_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    if (frozen) out << "frozen ( "
    out << name << slot << '@' << level
    if (offset.isDefined) out << '@' << offset
    if(matIndex.isDefined) {
      out << matIndex.get
    }
    if (frozen) out << " )"
  }

  override def progress = Logger.error(s"Trying to progress future field access to $name on level $level")
}

/// L4_ResolveFrozenFields

object L4_ResolveFrozenFields extends DefaultStrategy("Resolve frozen field accesses") {
  this += new Transformation("Resolve", {
    case fct : L4_FunctionCall if "frozen" == fct.name =>
      if (fct.arguments.length != 1) Logger.error("Calls to frozen need exactly one argument")
      if (!fct.arguments.head.isInstanceOf[L4_FutureFieldAccess]) Logger.error("Calls to frozen must done with exactly one field access")
      val fieldAccess = fct.arguments.head.asInstanceOf[L4_FutureFieldAccess]
      fieldAccess.frozen = true
      fieldAccess
  })
}
