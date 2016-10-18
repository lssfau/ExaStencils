package exastencils.knowledge.l4

import exastencils.base.l4.L4_Access

trait L4_KnowledgeAccess extends L4_Access {
  def target : L4_KnowledgeObject
  override def name : String = target.name
}