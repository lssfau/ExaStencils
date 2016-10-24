package exastencils.domain.l3

import exastencils.domain.l4._
import exastencils.knowledge.l3.L3_KnowledgeCollection

/// L3_DomainCollection

object L3_DomainCollection extends L3_KnowledgeCollection[L3_Domain, L4_Domain] {
  def progress() = objects.foreach(obj => L4_DomainCollection.add(obj.progress()))
}
