package exastencils.datastructures

import exastencils.core.StateManager
import exastencils.core.Logger._
import exastencils.core._

abstract class CustomStrategy(name : String) extends Strategy(name) {
  def apply() : Unit
}