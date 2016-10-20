package exastencils.domain.l4

import exastencils.domain.ir._
import exastencils.knowledge.l4.L4_KnowledgeCollection

/// L4_DomainCollection

object L4_DomainCollection extends L4_KnowledgeCollection[L4_Domain, IR_Domain] {
  def progress() = objects.foreach(obj => IR_DomainCollection.add(obj.progress()))
}

