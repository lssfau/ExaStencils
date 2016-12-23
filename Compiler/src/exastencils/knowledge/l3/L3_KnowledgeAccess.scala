package exastencils.knowledge.l3

import exastencils.base.l3.L3_Access

/// L3_KnowledgeAccess

trait L3_KnowledgeAccess extends L3_Access {
  def target : L3_KnowledgeObject[_]
  override def name : String = target.name
}