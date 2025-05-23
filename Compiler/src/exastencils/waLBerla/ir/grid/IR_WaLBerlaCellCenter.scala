package exastencils.waLBerla.ir.grid

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.domain.ir._
import exastencils.grid.ir.IR_VirtualFieldAccess
import exastencils.grid.ir._
import exastencils.knowledge.ir.IR_KnowledgeObject

case class IR_WaLBerlaCellCenterAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_WaLBerlaVirtualFieldWithVec {

  override protected val vf = IR_VF_CellCenterAsVec(level, domain)

  override def createDuplicate() = IR_WaLBerlaCellCenterAsVec(level, domain)
}

case class IR_WaLBerlaCellCenterPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_WaLBerlaVirtualFieldPerDim {

  override protected val vf = IR_VF_CellCenterPerDim(level, domain, dim)

  override def createDuplicate() : IR_KnowledgeObject = IR_WaLBerlaCellCenterPerDim(level, domain, dim)

  override def resolve(index : IR_ExpressionIndex) =
    IR_VirtualFieldAccess(IR_WaLBerlaNodePositionPerDim(level, domain, dim), Duplicate(index)) + 0.5 * IR_VirtualFieldAccess(IR_WaLBerlaCellWidthPerDim(level, domain, dim), Duplicate(index))
}
