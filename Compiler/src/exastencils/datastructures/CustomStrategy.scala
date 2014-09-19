package exastencils.datastructures

import exastencils.core.StateManager
import exastencils.core.Logger._
import exastencils.core._

/**
  * A Strategy for custom execution patterns of [[exastencils.datastructures.Transformation]]s.
  *
  * @param name name The name of the Strategy. Used for traceability and debugging purposes.
  */
abstract class CustomStrategy(name : String) extends Strategy(name) {
  def apply() : Unit
}