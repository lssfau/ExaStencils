package exastencils.grid.l4

/// L4_Localization

object L4_Localization {
  // TODO: sth more elegant?
  def dimToString(dim : Int) = ('x'.toInt + dim).toChar
  def stringToDim(dim : Char) = dim.toInt - 'x'.toInt
}

abstract class L4_Localization {
  def name : String
}

/// L4_AtNodes

case object L4_AtNode extends L4_Localization {
  def name = "node"
}

/// L4_AtCells

case object L4_AtCellCenter extends L4_Localization {
  def name = "cell"
}

/// L4_AtFace

case class L4_AtFaceCenter(dim : Int) extends L4_Localization {
  def name = s"face_${ L4_Localization.dimToString(dim) }"
}

/// L4_AtBoundary

// special object for accessing coordinates mapped to the boundary
case object L4_AtBoundary extends L4_Localization {
  def name = "boundary"
}
