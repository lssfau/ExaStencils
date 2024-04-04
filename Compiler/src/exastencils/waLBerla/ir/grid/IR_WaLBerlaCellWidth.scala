package exastencils.waLBerla.ir.grid

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_Domain
import exastencils.grid.ir.IR_VF_CellWidthAsVec
import exastencils.grid.ir.IR_VF_CellWidthPerDim
import exastencils.waLBerla.ir.blockforest._
import exastencils.waLBerla.ir.refinement.IR_WaLBerlaRefinementLevel
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_RealType

/// IR_WaLBerlaCellWidthAsVec

case class IR_WaLBerlaCellWidthAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_WaLBerlaVirtualFieldWithVec {

  override protected val vf = IR_VF_CellWidthAsVec(level, domain)

  override def createDuplicate() = IR_WaLBerlaCellWidthAsVec(level, domain)
}

/// IR_WaLBerlaCellWidthBlockPerDim
// block-loop variable for better readability

case class IR_WaLBerlaCellWidthBlockPerDim(dim : Int, refinementLevel : Option[IR_Expression] = None) extends IR_WaLBerlaBlockLoopVariable {
  override def resolveName() : String = s"d${ ('x' + dim).toChar.toString }_"

  override def resolveDatatype() : IR_Datatype = if (Knowledge.cuda_enabled) IR_RealDatatype else WB_RealType

  override def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(resolveDatatype(), resolveName(), IR_WaLBerlaBlockForest().getStepSize(dim, refinementLevel))
}

/// IR_WaLBerlaCellWidthPerDim

case class IR_WaLBerlaCellWidthPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_WaLBerlaVirtualFieldPerDim {

  override protected val vf = IR_VF_CellWidthPerDim(level, domain, dim)

  override def createDuplicate() = IR_WaLBerlaCellWidthPerDim(level, domain, dim)

  override def resolve(index : IR_ExpressionIndex) = {
    val refinementLvl = if (Knowledge.waLBerla_useRefinement)
      Some(IR_WaLBerlaRefinementLevel(IR_WaLBerlaBlock()))
    else
      None

    val blockForest = IR_WaLBerlaBlockForest()
    val maxLevel = if (blockForest.maxLevelWaLBerlaField.isDefined)
      blockForest.maxLevelWaLBerlaField.get.level
    else
      Knowledge.maxLevel

    math.pow(2.0, maxLevel - level) * IR_WaLBerlaCellWidthBlockPerDim(dim, refinementLvl)
  }
}