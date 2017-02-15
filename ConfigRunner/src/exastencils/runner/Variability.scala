package exastencils.runner

import scala.collection.mutable.ListBuffer

/// Variability

abstract class Variability {
  def print() : String
}

/// VariabilitiesFromList

object VariabilitiesFromList {
  def apply(name : String, values : List[Any]) = new VariabilitiesFromList(name, values.to[ListBuffer])
}

case class VariabilitiesFromList(var name : String, var values : ListBuffer[Any]) extends Variability {
  override def print() : String = name + " => { " + values.mkString(", ") + " }"
}

/// VariabilitiesFromLambda

case class VariabilitiesFromLambda(var name : String, var expression : CodeWrapper) extends Variability {
  override def print() : String = name + " => " + expression.print()
}
