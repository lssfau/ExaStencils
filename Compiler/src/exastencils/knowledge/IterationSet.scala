package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir._
import exastencils.logger._

case class IterationSet(var identifier : String,
  var begin : MultiIndex,
  var end : MultiIndex,
  var increment : MultiIndex,
  var condition : Option[Expression]) {}

object IterationSetCollection {
  var sets : ListBuffer[IterationSet] = ListBuffer()

  def getIterationSetByIdentifier(identifier : String) : Option[IterationSet] = {
    val ret = sets.find(f => f.identifier == identifier)
    if (ret.isEmpty) Logger.warn(s"Iteration set $identifier was not found")
    ret
  }
}