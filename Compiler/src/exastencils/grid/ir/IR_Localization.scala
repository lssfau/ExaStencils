package exastencils.grid.ir

/// IR_Localization

object IR_Localization {
  // TODO: sth more elegant?
  def dimToString(dim : Int) = ('x'.toInt + dim).toChar
  def stringToDim(dim : Char) = dim.toInt - 'x'.toInt
}

abstract class IR_Localization {
  def name : String
}

/// IR_AtNodes

case object IR_AtNode extends IR_Localization {
  def name = "node"
}

/// IR_AtCells

case object IR_AtCellCenter extends IR_Localization {
  def name = "cell"
}

/// IR_AtFace

case class IR_AtFaceCenter(dim : Int) extends IR_Localization {
  def name = s"face_${ IR_Localization.dimToString(dim) }"
}

/// IR_AtBoundary

// special object for accessing coordinates mapped to the boundary
case object IR_AtBoundary extends IR_Localization {
  def name = "boundary"
}
