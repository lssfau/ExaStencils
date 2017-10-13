package exastencils.datastructures

/**
  * A Strategy for custom execution patterns of [[exastencils.datastructures.Transformation]]s.
  *
  * @param name Name of the Strategy. Used for traceability and debugging purposes.
  */
abstract class CustomStrategy(name : String) extends Strategy(name) {
  def apply() : Unit
}