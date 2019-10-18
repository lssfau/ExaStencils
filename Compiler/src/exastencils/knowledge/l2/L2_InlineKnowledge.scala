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

package exastencils.knowledge.l2

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l2.L2_Statement
import exastencils.datastructures._
import exastencils.knowledge.l3._
import exastencils.prettyprinting.PpStream
import exastencils.util.l2.L2_GeneralParameter

/// L2_InlineKnowledge

object L2_InlineKnowledge {
  def apply(parameters : List[L2_KnowledgeParameter]) = new L2_InlineKnowledge(parameters.to[ListBuffer])
}

case class L2_InlineKnowledge(parameters : ListBuffer[L2_KnowledgeParameter]) extends L2_Statement {
  override def prettyprint(out : PpStream) = out << "Knowledge {\n" <<< (parameters, "\n") << "\n}"
  override def progress = ProgressLocation(L3_InlineKnowledge(parameters.map(_.progress)))
}

/// L2_KnowledgeParameter

case class L2_KnowledgeParameter(var name : String, var value : Any) extends L2_Statement with L2_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def progress = ProgressLocation(L3_KnowledgeParameter(name, value))
}

/// L2_ProcessInlineKnowledge

object L2_ProcessInlineKnowledge extends DefaultStrategy("Process inline knowledge blocks") {
  this += Transformation("Process", {
    case knowledge : L2_InlineKnowledge =>
      knowledge.parameters.foreach(_.set())
      None // consume statements
  })
}
