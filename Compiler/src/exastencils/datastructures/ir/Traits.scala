package exastencils.datastructures.ir

import exastencils.datastructures.Transformation

trait Debuggable {
  def debug() : String
}

trait Expandable {
  def expand() : Transformation.OutputType
}
