package exastencils.waLBerla.l4

import exastencils.domain.l4._
import exastencils.grid.l4._
import exastencils.waLBerla.ir.IR_WaLBerlaCellCenterAsVec
import exastencils.waLBerla.ir.IR_WaLBerlaCellCenterPerDim

case class L4_WaLBerlaCellCenterAsVec(
    var level : Int,
    var domain : L4_Domain
) extends L4_WaLBerlaVirtualFieldWithVec {

  override protected val vf = L4_VF_CellCenterAsVec(level, domain)

  override def createDuplicate() = L4_WaLBerlaCellCenterAsVec(level, domain)

  override def progressImpl() = IR_WaLBerlaCellCenterAsVec(level, domain.getProgressedObj())
}

case class L4_WaLBerlaCellCenterPerDim(
    var level : Int,
    var domain : L4_Domain,
    var dim : Int
) extends L4_WaLBerlaVirtualFieldPerDim {

  override protected val vf = L4_VF_CellCenterPerDim(level, domain, dim)

  override def createDuplicate() = L4_WaLBerlaCellCenterPerDim(level, domain, dim)

  override def progressImpl() = IR_WaLBerlaCellCenterPerDim(level, domain.getProgressedObj(), dim)
}