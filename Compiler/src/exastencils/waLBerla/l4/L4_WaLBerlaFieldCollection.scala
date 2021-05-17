package exastencils.waLBerla.l4

import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.l4.L4_FutureFieldAccess
import exastencils.knowledge.l4.L4_KnowledgeContainer
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ResolveAccesses
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection
import exastencils.waLBerla.ir.IR_WaLBerlaField
import exastencils.waLBerla.ir.IR_WaLBerlaFieldCollection

object L4_WaLBerlaFieldCollection extends L4_LeveledKnowledgeCollection[L4_WaLBerlaField, IR_WaLBerlaField] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_ResolveAccesses.strategies += L4_ResolveWaLBerlaFieldAccesses

  override def name = "L4_WaLBerlaFieldCollection"
  override def progress() : Unit = objects.foreach(obj => IR_WaLBerlaFieldCollection.add(obj.progress()))
}

object L4_ResolveWaLBerlaFieldAccesses extends DefaultStrategy("Resolve accesses to WB fields") {
  this += new Transformation("Resolve applicable future accesses", {
    case fAcc : L4_FutureFieldAccess if L4_WaLBerlaFieldCollection.exists(fAcc.name, fAcc.level) =>
      val wbField = L4_WaLBerlaFieldCollection.getByIdentifier(fAcc.name, fAcc.level).get
      L4_WaLBerlaFieldAccess(wbField, fAcc.slot, fAcc.offset, fAcc.arrayIndex, fAcc.frozen, fAcc.matIndex)
  })
}