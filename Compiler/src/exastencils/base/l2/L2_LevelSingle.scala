package exastencils.base.l2

import exastencils.base.l3._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_SingleLevel

case class L2_SingleLevel(var level : Int) extends L2_DeclarationLevelSpecification with L2_AccessLevelSpecification {
  def prettyprint(out : PpStream) = out << level
  override def toString = level.toString
  override def resolveLevel : Int = level
  override def progress = L3_SingleLevel(level)
}

/// L2_RelativeLevel

// FIXME: op -> BinOp
case class L2_RelativeLevel(var base : L2_LevelSpecification, var op : String, var offset : Int) extends L2_DeclarationLevelSpecification with L2_AccessLevelSpecification {
  def prettyprint(out : PpStream) = out << '(' << base << ' ' << op << ' ' << offset << ')'
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
  override def progress = L3_RelativeLevel(base.progress, op, offset)
}
