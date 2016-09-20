package exastencils.knowledge.l4

import exastencils.datastructures.l4.Access

trait L4_KnowledgeAccess extends Access {
  def target : L4_KnowledgeObjectWithIdent
  override def name : String = target.identifier
}