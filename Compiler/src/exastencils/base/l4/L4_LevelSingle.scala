package exastencils.base.l4

import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_SingleLevel

case class L4_SingleLevel(var level : Int) extends L4_DeclarationLevelSpecification with L4_AccessLevelSpecification {
  def prettyprint(out : PpStream) = out << level
  override def toString = level.toString
  override def resolveLevel : Int = level
}

/// L4_RelativeLevel

// FIXME: op -> BinOp
case class L4_RelativeLevel(var base : L4_LevelSpecification, var op : String, var offset : Int) extends L4_DeclarationLevelSpecification with L4_AccessLevelSpecification {
  def prettyprint(out : PpStream) = out << '(' << base << ' ' << op << ' ' << offset << ')'
  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)
}
