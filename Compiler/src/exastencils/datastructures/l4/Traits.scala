package exastencils.datastructures.l4

trait CppPrettyPrintable {
  def cpp() : String
}

trait CudaPrettyPrintable {
  def cuda() : String
}

trait Debuggable {
  def debug() : String
}
