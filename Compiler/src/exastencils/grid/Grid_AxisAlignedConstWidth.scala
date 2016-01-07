package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._

object Grid_AxisAlignedConstWidth {
  def geom = GridGeometry.getGeometry

  def initL4() = {}
  def generateInitCode() = ListBuffer()

  def cellWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    val levelIndex = level.asInstanceOf[IntegerConstant].v.toInt - Knowledge.minLevel
    dim match {
      case 0 => Knowledge.discr_hx(levelIndex)
      case 1 => Knowledge.discr_hy(levelIndex)
      case 2 => Knowledge.discr_hz(levelIndex)
    }
  }

  def nodePosition(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    index(dim) * geom.gridWidth(level, index, arrayIndex, dim) + ArrayAccess(iv.PrimitivePositionBegin(), dim)
  }

  def cellCenter(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    (index(dim) + 0.5) * geom.gridWidth(level, index, arrayIndex, dim) + ArrayAccess(iv.PrimitivePositionBegin(), dim)
  }
}
