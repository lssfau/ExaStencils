package exastencils.grid

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir._

/// GridGeometry_uniform

trait GridGeometry_uniform extends GridGeometry {
  // properties of uniform grids
  override def cellWidth(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = {
    val levelIndex = level - Knowledge.minLevel
    dim match {
      case 0 => Knowledge.discr_hx(levelIndex)
      case 1 => Knowledge.discr_hy(levelIndex)
      case 2 => Knowledge.discr_hz(levelIndex)
    }
  }

  override def nodePosition(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = {
    index(dim) * cellWidth(level, index, arrayIndex, dim) + IR_IV_FragmentPositionBegin(dim)
  }

  override def cellCenter(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) : IR_Expression = {
    (index(dim) + 0.5) * cellWidth(level, index, arrayIndex, dim) + IR_IV_FragmentPositionBegin(dim)
  }
}

