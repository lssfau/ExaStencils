package exastencils.base.l1

import exastencils.datastructures._
import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_Root
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L1_Root

object L1_Root {
  def apply() = new L1_Root(ListBuffer())
  def apply(node : L1_Node) = new L1_Root(ListBuffer(node))
  def apply(nodes : List[L1_Node]) = new L1_Root(nodes.to[ListBuffer])
}

case class L1_Root(var nodes : ListBuffer[L1_Node]) extends L1_Node with L1_Progressable with PrettyPrintable {
  var domains = ListBuffer[L1_Domain]()
  // var operators = Map[String, L1_Operator]()
  // var equations = ListBuffer[L1_Equation]()
  // var rhss = ListBuffer[L1_RHS]()
  // var mappings = ListBuffer[L1_Mapping]()

  override def prettyprint(out : PpStream) = {
    // TODO: print knowledge collections

    nodes.foreach {
      case p : PrettyPrintable => out << p << "\n\n"
      case other               => Logger.warn(s"Trying to print unsupported L1 node $other")
    }
  }

  override def progress = {
    val (progressable, invalid) = nodes.partition(_.isInstanceOf[L1_Progressable])
    var domains = Map[String, L1_Domain]()

    invalid.foreach(node => Logger.warn(s"Trying to progress unsupported L1 node $node"))

    L2_Root(progressable.map(_.asInstanceOf[L1_Progressable].progress))
    //nodes.foreach(n => n match {
    //      case x : L1_Domain   => domains += ((x.identifier, x))
    //case x : L1_Operator => operators += ((x.identifier, x))
    //      case x : L1_Equation => equations += x
    //      case x : L1_RHS      => rhss += x
    //case x : L1_Mapping  => mappings += x
    //})
  }

  // resolve nested root nodes
  def flatten() = {
    while (nodes.exists(_.isInstanceOf[L1_Root]))
      nodes = nodes.flatMap {
        case root : L1_Root => root.nodes
        case other          => ListBuffer(other)
      }
  }
  override def toString() = {
    "L1 Root" +
      "\nDomains:   " + domains.toString()
    //"\nOperators: " + operators.toString() +
    //      "\nEquations: " + equations.toString() +
    //      "\nRHS:       " + rhss.toString() +
    //"\nMappings:  " + mappings.toString()
  }
}
