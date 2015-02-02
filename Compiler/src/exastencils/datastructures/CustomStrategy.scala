package exastencils.datastructures

/**
  * A Strategy for custom execution patterns of [[exastencils.datastructures.Transformation]]s.
  *
  * @param name name The name of the Strategy. Used for traceability and debugging purposes.
  */
abstract class CustomStrategy(name : String) extends Strategy(name) {
  def apply() : Unit
}