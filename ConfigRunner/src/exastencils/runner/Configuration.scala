package exastencils.runner

import scala.collection.mutable.ListBuffer

/// Configuration

case class Configuration(var parameters : ListBuffer[Parameter]) {
  def print() : String = "[" + parameters.map(param => "(" + param.name + ", " + param.value.toString + ")").mkString(", ") + "]"
  def apply() : Unit = parameters.foreach(_.apply())
}