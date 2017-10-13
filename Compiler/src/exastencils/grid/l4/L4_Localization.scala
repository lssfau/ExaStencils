package exastencils.grid.l4

import exastencils.base.l4._
import exastencils.grid.ir._
import exastencils.prettyprinting._

/// L4_Localization

object L4_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: sth more elegant?
  def dimToString(dim : Int) = ('x'.toInt + dim).toChar
  def stringToDim(dim : Char) = dim.toInt - 'x'.toInt

  // TODO: max number of dimensions?
  def availableLocalizations = List(L4_AtNode, L4_AtCellCenter, L4_AtFaceCenter(0), L4_AtFaceCenter(1), L4_AtFaceCenter(2), L4_AtBoundary)

  def resolve(name : String) = availableLocalizations.find(_.name.toLowerCase == name.toLowerCase).get
}

abstract class L4_Localization extends L4_Node with PrettyPrintable with L4_Progressable {
  def name : String
  override def prettyprint(out : PpStream) = out << name
  override def progress : IR_Localization
}

/// L4_AtNodes

case object L4_AtNode extends L4_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Node"
  def progress = IR_AtNode
}

/// L4_AtCells

case object L4_AtCellCenter extends L4_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Cell"
  def progress = IR_AtCellCenter
}

/// L4_AtFace

case class L4_AtFaceCenter(dim : Int) extends L4_Localization {
  def name = s"Face_${ L4_Localization.dimToString(dim) }"
  def progress = IR_AtFaceCenter(dim)
}

/// L4_AtBoundary

// special object for accessing coordinates mapped to the boundary
case object L4_AtBoundary extends L4_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Boundary"
  def progress = IR_AtBoundary
}

/// L4_HACK_OtherLocalization

case class L4_HACK_OtherLocalization(name : String) extends L4_Localization {
  override def progress = ???
}
