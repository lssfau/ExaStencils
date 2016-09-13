package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.logger._

case class IterationSet(var identifier : String,
    var begin : IR_ExpressionIndex,
    var end : IR_ExpressionIndex,
    var increment : IR_ExpressionIndex,
    var condition : Option[IR_Expression]) {}

object IterationSetCollection {
  var sets : ListBuffer[IterationSet] = ListBuffer()

  def getIterationSetByIdentifier(identifier : String) : Option[IterationSet] = {
    val ret = sets.find(f => f.identifier == identifier)
    if (ret.isEmpty) Logger.warn(s"Iteration set $identifier was not found")
    ret
  }
}