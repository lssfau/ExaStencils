package exastencils.knowledge.l3

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l3.L3_Statement
import exastencils.datastructures._
import exastencils.knowledge.l4._
import exastencils.prettyprinting.PpStream
import exastencils.util.l3.L3_GeneralParameter

/// L3_InlineKnowledge

object L3_InlineKnowledge {
  def apply(parameters : List[L3_KnowledgeParameter]) = new L3_InlineKnowledge(parameters.to[ListBuffer])
}

case class L3_InlineKnowledge(parameters : ListBuffer[L3_KnowledgeParameter]) extends L3_Statement {
  override def prettyprint(out : PpStream) = out << "Knowledge {\n" <<< (parameters, "\n") << "\n}"
  override def progress = ProgressLocation(L4_InlineKnowledge(parameters.map(_.progress)))
}

/// L3_KnowledgeParameter

case class L3_KnowledgeParameter(var name : String, var value : Any) extends L3_Statement with L3_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def progress = ProgressLocation(L4_KnowledgeParameter(name, value))
}

/// L3_ProcessInlineKnowledge

object L3_ProcessInlineKnowledge extends DefaultStrategy("Process inline knowledge blocks") {
  this += Transformation("Process", {
    case knowledge : L3_InlineKnowledge =>
      knowledge.parameters.foreach(_.set())
      None // consume statements
  })
}
