package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._

object Grid_AxisAlignedConstWidth extends Grid {
  override def initL4() = {}
  override def generateInitCode() = ListBuffer()

  override def resolveGridMemberFunction(name : String) : Option[java.lang.reflect.Method] = {
    this.getClass().getMethods.find(_.getName.toLowerCase() == name.toLowerCase())
  }

  override def cellWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    val levelIndex = level.asInstanceOf[IntegerConstant].v.toInt - Knowledge.minLevel
    dim match {
      case 0 => Knowledge.discr_hx(levelIndex)
      case 1 => Knowledge.discr_hy(levelIndex)
      case 2 => Knowledge.discr_hz(levelIndex)
    }
  }

  override def nodePosition(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    index(dim) * gridWidth(level, index, arrayIndex, dim) + ArrayAccess(iv.PrimitivePositionBegin(), dim)
  }

  override def cellCenter(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    (index(dim) + 0.5) * gridWidth(level, index, arrayIndex, dim) + ArrayAccess(iv.PrimitivePositionBegin(), dim)
  }

  override def invokeEvalResolve(functionName : String, fieldAccess : FieldAccess, interpolation : String) : Expression = ???
  override def invokeIntegrateResolve(functionName : String, exp : Expression) : Expression = ???
}
