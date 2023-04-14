package exastencils.waLBerla.l4.refinement

import exastencils.knowledge.l4.L4_BasicKnowledgeCollection
import exastencils.knowledge.l4.L4_KnowledgeContainer
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_PrepareDeclarations
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ProcessDeclarations
import exastencils.waLBerla.ir.refinement.IR_WaLBerlaRefinementSelection
import exastencils.waLBerla.ir.refinement.IR_WaLBerlaRefinementSelectionCollection

object L4_WaLBerlaRefinementSelectionCollection extends L4_BasicKnowledgeCollection[L4_WaLBerlaRefinementSelection, IR_WaLBerlaRefinementSelection] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_PrepareRefinementSelectionDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessRefinementSelectionDeclarations

  override def name = "IR_WaLBerlaRefinementSelectionCollection"
  override def progress() = objects.foreach(obj => IR_WaLBerlaRefinementSelectionCollection.add(obj.progress()))

}
