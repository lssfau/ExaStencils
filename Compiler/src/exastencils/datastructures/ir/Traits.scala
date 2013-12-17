package exastencils.datastructures.ir

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
