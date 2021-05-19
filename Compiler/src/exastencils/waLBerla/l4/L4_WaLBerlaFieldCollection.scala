package exastencils.waLBerla.l4

import exastencils.field.l4.L4_FutureFieldAccess
import exastencils.knowledge.l4.L4_KnowledgeContainer
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection
import exastencils.waLBerla.ir.IR_WaLBerlaField
import exastencils.waLBerla.ir.IR_WaLBerlaFieldCollection

object L4_WaLBerlaFieldCollection extends L4_LeveledKnowledgeCollection[L4_WaLBerlaField, IR_WaLBerlaField] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  def getByFieldAccess(access : L4_FutureFieldAccess) : Option[L4_WaLBerlaField] = getByIdentifier(access.name, access.level, suppressError = true)

  def contains(access : L4_FutureFieldAccess) : Boolean = getByFieldAccess(access).isDefined

  override def name = "L4_WaLBerlaFieldCollection"
  override def progress() : Unit = objects.foreach(obj => IR_WaLBerlaFieldCollection.add(obj.progress()))
}
