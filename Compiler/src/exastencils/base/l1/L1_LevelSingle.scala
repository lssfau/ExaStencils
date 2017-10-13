package exastencils.base.l1

import exastencils.base.l2._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_SingleLevel

case class L1_SingleLevel(var level : Int) extends L1_DeclarationLevelSpecification with L1_AccessLevelSpecification {
  def prettyprint(out : PpStream) = out << level
  override def toString = level.toString
  override def resolveLevel : Int = level
  override def progress = L2_SingleLevel(level)
}

/// L1_RelativeLevel

// FIXME: op -> BinOp
case class L1_RelativeLevel(var base : L1_LevelSpecification, var op : String, var offset : Int) extends L1_DeclarationLevelSpecification with L1_AccessLevelSpecification {
  def prettyprint(out : PpStream) = out << '(' << base << ' ' << op << ' ' << offset << ')'
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
  override def progress = L2_RelativeLevel(base.progress, op, offset)
}
