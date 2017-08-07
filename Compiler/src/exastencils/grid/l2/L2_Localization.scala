package exastencils.grid.l2

import exastencils.base.l2._
import exastencils.grid.l3._
import exastencils.prettyprinting._

/// L2_Localization

object L2_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: sth more elegant?
  def dimToString(dim : Int) = ('x'.toInt + dim).toChar
  def stringToDim(dim : Char) = dim.toInt - 'x'.toInt

  // TODO: max number of dimensions?
  def availableLocalizations = List(L2_AtNode, L2_AtCellCenter, L2_AtFaceCenter(0), L2_AtFaceCenter(1), L2_AtFaceCenter(2), L2_AtBoundary)

  def resolve(name : String) = availableLocalizations.find(_.name.toLowerCase == name.toLowerCase).get
}

abstract class L2_Localization extends L2_Node with PrettyPrintable with L2_Progressable {
  def name : String
  override def prettyprint(out : PpStream) = out << name
  override def progress : L3_Localization
}

/// L2_AtNodes

case object L2_AtNode extends L2_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Node"
  def progress = L3_AtNode
}

/// L2_AtCells

case object L2_AtCellCenter extends L2_Localization {
  def name = "Cell"
  def progress = L3_AtCellCenter
}

/// L2_AtFace

case class L2_AtFaceCenter(dim : Int) extends L2_Localization {
  def name = s"Face_${ L2_Localization.dimToString(dim) }"
  def progress = L3_AtFaceCenter(dim)
}

/// L2_AtBoundary

// special object for accessing coordinates mapped to the boundary
case object L2_AtBoundary extends L2_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Boundary"
  def progress = L3_AtBoundary
}
