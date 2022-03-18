package exastencils.waLBerla.l4

import exastencils.field.l4.L4_FieldAccess
import exastencils.field.l4.L4_FutureFieldAccess
import exastencils.fieldlike.l4.L4_FieldLikeCollection
import exastencils.knowledge.l4.L4_KnowledgeContainer
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_PrepareAccesses
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_PrepareDeclarations
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ProcessDeclarations
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ResolveAccesses
import exastencils.waLBerla.ir.IR_WaLBerlaField
import exastencils.waLBerla.ir.IR_WaLBerlaFieldCollection

object L4_WaLBerlaFieldCollection extends L4_FieldLikeCollection[L4_WaLBerlaField, IR_WaLBerlaField] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_PrepareFieldDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessFieldDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareFieldAccesses
  L4_ResolveAccesses.strategies += L4_ResolveFieldAccesses

  def contains(access : L4_FutureFieldAccess) : Boolean = getByFieldAccess(access).isDefined
  def contains(access : L4_FieldAccess) : Boolean = getByFieldAccess(access).isDefined

  override def name = "L4_WaLBerlaFieldCollection"
  override def progress() : Unit = objects.foreach(obj => IR_WaLBerlaFieldCollection.add(obj.progress()))
}
