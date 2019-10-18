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

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Statement
import exastencils.datastructures._
import exastencils.knowledge.ir._
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_GeneralParameter

/// L4_InlineKnowledge

object L4_InlineKnowledge {
  def apply(parameters : List[L4_KnowledgeParameter]) = new L4_InlineKnowledge(parameters.to[ListBuffer])
}

case class L4_InlineKnowledge(parameters : ListBuffer[L4_KnowledgeParameter]) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "Knowledge {\n" <<< (parameters, "\n") << "\n}"
  override def progress = ProgressLocation(IR_InlineKnowledge(parameters.map(_.progress)))
}

/// L4_KnowledgeParameter

case class L4_KnowledgeParameter(var name : String, var value : Any) extends L4_Statement with L4_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def progress = ProgressLocation(IR_KnowledgeParameter(name, value))
}

/// L4_ProcessInlineKnowledge

object L4_ProcessInlineKnowledge extends DefaultStrategy("Process inline knowledge blocks") {
  this += Transformation("Process", {
    case knowledge : L4_InlineKnowledge =>
      knowledge.parameters.foreach(_.set())
      None // consume statements
  })
}
