package exastencils.grid.l2

/// L2_Localization

object L2_Localization {
  // TODO: sth more elegant?
  def dimToString(dim : Int) = ('x'.toInt + dim).toChar
  def stringToDim(dim : Char) = dim.toInt - 'x'.toInt
}

abstract class L2_Localization {
  def name : String
}

/// L2_AtNodes

case object L2_AtNode extends L2_Localization {
  def name = "node"
}

/// L2_AtCells

case object L2_AtCellCenter extends L2_Localization {
  def name = "cell"
}

/// L2_AtFace

case class L2_AtFaceCenter(dim : Int) extends L2_Localization {
  def name = s"face_${ L2_Localization.dimToString(dim) }"
}

/// L2_AtBoundary

// special object for accessing coordinates mapped to the boundary
case object L2_AtBoundary extends L2_Localization {
  def name = "boundary"
}
