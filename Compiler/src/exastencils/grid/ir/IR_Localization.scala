package exastencils.grid.ir

import exastencils.base.ir.IR_Node

/// IR_Localization

object IR_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: sth more elegant?
  def dimToString(dim : Int) = ('x'.toInt + dim).toChar
  def stringToDim(dim : Char) = dim.toInt - 'x'.toInt

  // TODO: max number of dimensions?
  def availableLocalizations = List(IR_AtNode, IR_AtCellCenter, IR_AtFaceCenter(0), IR_AtFaceCenter(1), IR_AtFaceCenter(2), IR_AtBoundary)

  def resolve(name : String) = availableLocalizations.find(_.name.toLowerCase == name.toLowerCase).get
}

abstract class IR_Localization extends IR_Node {
  def name : String
}

/// IR_AtNodes

case object IR_AtNode extends IR_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Node"
}

/// IR_AtCells

case object IR_AtCellCenter extends IR_Localization {
  def name = "Cell"
}

/// IR_AtFace

case class IR_AtFaceCenter(dim : Int) extends IR_Localization {
  def name = s"Face_${ IR_Localization.dimToString(dim) }"
}

/// IR_AtBoundary

// special object for accessing coordinates mapped to the boundary
case object IR_AtBoundary extends IR_Localization {
  exastencils.core.Duplicate.registerConstant(this)

  def name = "Boundary"
}
