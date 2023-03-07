package exastencils.waLBerla.ir.grid

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_Domain
import exastencils.grid.ir.IR_VF_CellWidthAsVec
import exastencils.grid.ir.IR_VF_CellWidthPerDim
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaRefinementLevel

case class IR_WaLBerlaCellWidthAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_WaLBerlaVirtualFieldWithVec {

  override protected val vf = IR_VF_CellWidthAsVec(level, domain)

  override def createDuplicate() = IR_WaLBerlaCellWidthAsVec(level, domain)
}

case class IR_WaLBerlaCellWidthPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_WaLBerlaVirtualFieldPerDim {

  override protected val vf = IR_VF_CellWidthPerDim(level, domain, dim)

  override def createDuplicate() = IR_WaLBerlaCellWidthPerDim(level, domain, dim)

  override def resolve(index : IR_ExpressionIndex) = {
    val refinementLvl = if (Knowledge.waLBerla_useRefinement)
      Some(IR_WaLBerlaRefinementLevel())
    else
      None

    IR_WaLBerlaBlockForest().getStepSize(dim, refinementLvl)
  }
}