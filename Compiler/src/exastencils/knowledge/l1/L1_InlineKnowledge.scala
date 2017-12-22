package exastencils.knowledge.l1

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l1.L1_Statement
import exastencils.datastructures._
import exastencils.knowledge.l2._
import exastencils.prettyprinting.PpStream
import exastencils.util.l1.L1_GeneralParameter

/// L1_InlineKnowledge

object L1_InlineKnowledge {
  def apply(parameters : List[L1_KnowledgeParameter]) = new L1_InlineKnowledge(parameters.to[ListBuffer])
}

case class L1_InlineKnowledge(parameters : ListBuffer[L1_KnowledgeParameter]) extends L1_Statement {
  override def prettyprint(out : PpStream) = out << "Knowledge {\n" <<< (parameters, "\n") << "\n}"
  override def progress = ProgressLocation(L2_InlineKnowledge(parameters.map(_.progress)))
}

/// L1_KnowledgeParameter

case class L1_KnowledgeParameter(var name : String, var value : Any) extends L1_Statement with L1_GeneralParameter {
  override def prettyprint(out : PpStream) = out << name << " = " << printVal
  override def progress = ProgressLocation(L2_KnowledgeParameter(name, value))
}

/// L1_ProcessInlineKnowledge

object L1_ProcessInlineKnowledge extends DefaultStrategy("Process inline knowledge blocks") {
  this += Transformation("Process", {
    case knowledge : L1_InlineKnowledge =>
      knowledge.parameters.foreach(_.set())
      None // consume statements
  })
}
