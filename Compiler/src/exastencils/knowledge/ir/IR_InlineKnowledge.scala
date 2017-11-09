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
