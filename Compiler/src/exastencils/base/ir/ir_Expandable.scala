package exastencils.base.ir

import exastencils.datastructures.Transformation

trait IR_Expandable {
  def expand() : Transformation.OutputType
}
