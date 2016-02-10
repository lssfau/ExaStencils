package exastencils.datastructures.l1

import exastencils.datastructures._
import scala.collection.mutable.ListBuffer
import java.io.StringWriter

case class Root(nodes : List[Node]) extends Node {
  var domains = Map[String, Domain]()
  var operators = Map[String, Operator]()
  var equations = ListBuffer[Equation]()
  var rhss = ListBuffer[RHS]()
  var mappings = ListBuffer[Mapping]()

  def sort() = {
    nodes.foreach(n => n match {
      case x : Domain   => domains += ((x.identifier, x))
      case x : Operator => operators += ((x.identifier, x))
      case x : Equation => equations += x
      case x : RHS      => rhss += x
      case x : Mapping  => mappings += x
    })
  }

  override def toString() = {
    "L1 Root" +
      "\nDomains:   " + domains.toString() +
      "\nOperators: " + operators.toString() +
      "\nEquations: " + equations.toString() +
      "\nRHS:       " + rhss.toString() +
      "\nMappings:  " + mappings.toString()
  }
}
