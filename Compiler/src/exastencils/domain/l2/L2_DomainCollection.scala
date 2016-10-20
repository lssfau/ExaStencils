package exastencils.domain.l2

import exastencils.domain.l3._
import exastencils.knowledge.l2.L2_KnowledgeCollection

/// L2_DomainCollection

object L2_DomainCollection extends L2_KnowledgeCollection[L2_Domain, L3_Domain] {
  def progress() = objects.foreach(obj => L3_DomainCollection.add(obj.progress()))
}
