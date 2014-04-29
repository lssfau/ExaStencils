package exastencils.datastructures.ir

import exastencils.core.collectors._
import exastencils.datastructures._

trait FilePrettyPrintable {
  def printToFile() : Unit
}

trait CppPrettyPrintable {
  def cpp() : String
}

trait CudaPrettyPrintable {
  def cuda() : String
}

trait Debuggable {
  def debug() : String
}

trait Expandable {
  def expand() : Node
}
