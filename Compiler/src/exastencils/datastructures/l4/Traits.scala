package exastencils.datastructures.l4

trait Debuggable {
  def debug() : String
}

trait ProgressableToIr {
  def progress : Any
}