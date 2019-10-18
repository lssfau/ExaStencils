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

package exastencils.knowledge.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_Statement
import exastencils.datastructures._
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_GeneralParameter

/// IR_InlineKnowledge

case class IR_InlineKnowledge(parameters : ListBuffer[IR_KnowledgeParameter]) extends IR_Statement {
  override def prettyprint(out : PpStream) = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
}

/// IR_KnowledgeParameter

case class IR_KnowledgeParameter(var name : String, var value : Any) extends IR_Statement with IR_GeneralParameter {
  override def prettyprint(out : PpStream) = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
}

/// IR_ProcessInlineKnowledge

object IR_ProcessInlineKnowledge extends DefaultStrategy("Process inline knowledge blocks") {
  this += Transformation("Process", {
    case knowledge : IR_InlineKnowledge =>
      knowledge.parameters.foreach(_.set())
      None // consume statements
  })
}
