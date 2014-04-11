package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._

case class IterationSet(var identifier : String,
    var begin : MultiIndex,
    var end : MultiIndex,
    var increment : MultiIndex) extends Node {}

case class IterationSetCollection(var sets : ListBuffer[IterationSet] = ListBuffer()) extends Node {
  def getIterationSetByIdentifier(identifier : String) : Option[IterationSet] = {
    sets.find(f => f.identifier == identifier)
  }
}