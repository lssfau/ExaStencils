package exastencils.grid

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._

object Grid_AxisAlignedConstWidth extends Grid {
  override def resolveGridMemberFunction(name : String) : Option[java.lang.reflect.Method] = {
    this.getClass().getMethods.find(_.getName.toLowerCase() == name.toLowerCase())
  }

  def gridWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    val levelIndex = level.asInstanceOf[IntegerConstant].v.toInt - Knowledge.minLevel
    dim match {
      case 0 => Knowledge.discr_hx(levelIndex)
      case 1 => Knowledge.discr_hy(levelIndex)
      case 2 => Knowledge.discr_hz(levelIndex)
    }
  }

  def nodePosition(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    dim match {
      case 0 => "xPos"
      case 1 => "yPos"
      case 2 => "zPos"
    }
  }

  override def invokeEvalResolve(functionName : String, fieldAccess : FieldAccess) : Expression = ???
  override def invokeIntegrateResolve(functionName : String, exp : Expression) : Expression = ???
}