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
