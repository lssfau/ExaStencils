package exastencils.datastructures.ir

trait CppPrettyPrintable {
  def cpp() : String
}

trait CudaPrettyPrintable {
  def cuda() : String
}
