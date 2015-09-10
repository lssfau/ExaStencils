package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._

object Grid_AxisAlignedConstWidth extends Grid {
  // helper method to map names of special fields to actual member functions implementing the resolving step
  override def invokeAccessResolve(specialField : SpecialFieldAccess) : Expression = {
    val functionName = specialField.fieldName
    functionName.substring(functionName.length() - 2) match {
      case "_x" => {
        val method = this.getClass().getMethods.find(_.getName == functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, specialField.level, specialField.index, specialField.arrayIndex, 0 : Integer).asInstanceOf[Expression]
      }
      case "_y" => {
        val method = this.getClass().getMethods.find(_.getName == functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, specialField.level, specialField.index, specialField.arrayIndex, 1 : Integer).asInstanceOf[Expression]
      }
      case "_z" => {
        val method = this.getClass().getMethods.find(_.getName == functionName.substring(0, functionName.length - 2))
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, specialField.level, specialField.index, specialField.arrayIndex, 2 : Integer).asInstanceOf[Expression]
      }
      case _ => {
        val method = this.getClass().getMethods.find(_.getName == functionName)
        if (!method.isDefined) Logger.debug(s"Trying to access invalid method $functionName")
        method.get.invoke(this, specialField.level, specialField.index, specialField.arrayIndex).asInstanceOf[Expression]
      }
    }
  }

  def vf_gridWidth(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    val levelIndex = level.asInstanceOf[IntegerConstant].v.toInt - Knowledge.minLevel
    dim match {
      case 0 => Knowledge.discr_hx(levelIndex)
      case 1 => Knowledge.discr_hy(levelIndex)
      case 2 => Knowledge.discr_hz(levelIndex)
    }
  }

  def vf_nodePosition(level : Expression, index : MultiIndex, arrayIndex : Option[Int], dim : Int) : Expression = {
    dim match {
      case 0 => "xPos"
      case 1 => "yPos"
      case 2 => "zPos"
    }
  }

  override def invokeEvalResolve(functionName : String, fieldAccess : FieldAccess) : Expression = ???
  override def invokeIntegrateResolve(functionName : String, exp : Expression) : Expression = ???
}