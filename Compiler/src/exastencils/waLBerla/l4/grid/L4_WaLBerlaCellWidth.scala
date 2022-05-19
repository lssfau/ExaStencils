package exastencils.waLBerla.l4.grid

import exastencils.domain.l4.L4_Domain
import exastencils.grid.l4.L4_VF_CellWidthAsVec
import exastencils.grid.l4.L4_VF_CellWidthPerDim
import exastencils.waLBerla.ir.grid._

case class L4_WaLBerlaCellWidthAsVec(
    var level : Int,
    var domain : L4_Domain
) extends L4_WaLBerlaVirtualFieldWithVec {

  override protected val vf = L4_VF_CellWidthAsVec(level, domain)

  override def createDuplicate() = L4_WaLBerlaCellWidthAsVec(level, domain)

  override def progressImpl() = IR_WaLBerlaCellWidthAsVec(level, domain.getProgressedObj())
}

case class L4_WaLBerlaCellWidthPerDim(
    var level : Int,
    var domain : L4_Domain,
    var dim : Int
) extends L4_WaLBerlaVirtualFieldPerDim {

  override protected val vf = L4_VF_CellWidthPerDim(level, domain, dim)

  override def createDuplicate() = L4_WaLBerlaCellWidthPerDim(level, domain, dim)

  override def progressImpl() = IR_WaLBerlaCellWidthPerDim(level, domain.getProgressedObj(), dim)
}
