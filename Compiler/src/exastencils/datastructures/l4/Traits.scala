package exastencils.datastructures.l4

import exastencils.datastructures._

trait Debuggable {
  def debug() : String
}

trait ProgressableToIr {
  def progressToIr : Any
}