package exastencils.knowledge.l4

import scala.collection.mutable._

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
  override def progress = IR_InlineKnowledge(parameters.map(_.progress))
}

/// L4_KnowledgeParameter

case class L4_KnowledgeParameter(var name : String, var value : Any) extends L4_Statement with L4_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def progress = IR_KnowledgeParameter(name, value)
}

/// L4_ProcessInlineKnowledge

object L4_ProcessInlineKnowledge extends DefaultStrategy("Process inline knowledge blocks") {
  this += Transformation("Process", {
    case knowledge : L4_InlineKnowledge =>
      knowledge.parameters.foreach(_.set())
      None // consume statements
  })
}