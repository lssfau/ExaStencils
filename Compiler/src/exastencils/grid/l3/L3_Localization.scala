package exastencils.grid.l3

/// L3_Localization

object L3_Localization {
  // TODO: sth more elegant?
  def dimToString(dim : Int) = ('x'.toInt + dim).toChar
  def stringToDim(dim : Char) = dim.toInt - 'x'.toInt
}

abstract class L3_Localization {
  def name : String
}

/// L3_AtNodes

case object L3_AtNode extends L3_Localization {
  def name = "node"
}

/// L3_AtCells

case object L3_AtCellCenter extends L3_Localization {
  def name = "cell"
}

/// L3_AtFace

case class L3_AtFaceCenter(dim : Int) extends L3_Localization {
  def name = s"face_${ L3_Localization.dimToString(dim) }"
}

/// L3_AtBoundary

// special object for accessing coordinates mapped to the boundary
case object L3_AtBoundary extends L3_Localization {
  def name = "boundary"
}
