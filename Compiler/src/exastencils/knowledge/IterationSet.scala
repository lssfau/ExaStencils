package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.core.Logger._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._

case class IterationSet(var identifier : String,
  var begin : MultiIndex,
  var end : MultiIndex,
  var increment : MultiIndex,
  var condition : Option[Expression]) {}

object IterationSetCollection {
  var sets : ListBuffer[IterationSet] = ListBuffer()

  def getIterationSetByIdentifier(identifier : String) : Option[IterationSet] = {
    val ret = sets.find(f => f.identifier == identifier)
    if (ret.isEmpty) warn(s"Iteration set $identifier was not found")
    ret
  }
}