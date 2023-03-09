package exastencils.waLBerla.ir.grid

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_Domain
import exastencils.grid.ir.IR_VF_NodePositionAsVec
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.grid.ir.IR_VirtualFieldAccess
import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks

case class IR_WaLBerlaNodePositionAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_WaLBerlaVirtualFieldWithVec {

  override protected val vf = IR_VF_NodePositionAsVec(level, domain)

  override def createDuplicate() = IR_WaLBerlaNodePositionAsVec(level, domain)
}

case class IR_WaLBerlaNodePositionPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_WaLBerlaVirtualFieldPerDim {

  override protected val vf = IR_VF_NodePositionPerDim(level, domain, dim)

  override def createDuplicate() : IR_KnowledgeObject = IR_WaLBerlaNodePositionPerDim(level, domain, dim)

  override def resolve(index : IR_ExpressionIndex) =
    IR_WaLBerlaBlockAABB(IR_WaLBerlaLoopOverBlocks.block).min(dim) + index(dim) * IR_VirtualFieldAccess(IR_WaLBerlaCellWidthPerDim(level, domain, dim), Duplicate(index))
}
