package exastencils.base.l3

import exastencils.base.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_SingleLevel

case class L3_SingleLevel(var level : Int) extends L3_DeclarationLevelSpecification with L3_AccessLevelSpecification {
  def prettyprint(out : PpStream) = out << level
  override def toString = level.toString
  override def resolveLevel : Int = level
  override def progress = L4_SingleLevel(level)
}

/// L3_RelativeLevel

// FIXME: op -> BinOp
case class L3_RelativeLevel(var base : L3_LevelSpecification, var op : String, var offset : Int) extends L3_DeclarationLevelSpecification with L3_AccessLevelSpecification {
  def prettyprint(out : PpStream) = out << '(' << base << ' ' << op << ' ' << offset << ')'
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
  override def progress = L4_RelativeLevel(base.progress, op, offset)
}
