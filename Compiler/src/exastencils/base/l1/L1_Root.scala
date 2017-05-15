package exastencils.base.l1

import exastencils.datastructures._
import scala.collection.mutable.ListBuffer

case class L1_Root(nodes : List[Node]) extends Node {
  var domains = Map[String, L1_Domain]()
  var operators = Map[String, L1_Operator]()
  //  var equations = ListBuffer[L1_Equation]()
  //  var rhss = ListBuffer[L1_RHS]()
  var mappings = ListBuffer[L1_Mapping]()

  def sort() = {
    nodes.foreach(n => n match {
      //      case x : L1_Domain   => domains += ((x.identifier, x))
      case x : L1_Operator => operators += ((x.identifier, x))
      //      case x : L1_Equation => equations += x
      //      case x : L1_RHS      => rhss += x
      case x : L1_Mapping  => mappings += x
    })
  }

  override def toString() = {
    "L1 Root" +
      "\nDomains:   " + domains.toString() +
      "\nOperators: " + operators.toString() +
      //      "\nEquations: " + equations.toString() +
      //      "\nRHS:       " + rhss.toString() +
      "\nMappings:  " + mappings.toString()
  }
}
