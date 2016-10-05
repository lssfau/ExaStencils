package exastencils.baseExt.ir

import exastencils.base.ir._
import exastencils.datastructures.Node
import exastencils.strategies.SimplifyStrategy

/// IR_ExpressionIndexRange

case class IR_ExpressionIndexRange(var begin : IR_ExpressionIndex, var end : IR_ExpressionIndex) extends Node {
  def print = s"${ begin.prettyprint() } to ${ end.prettyprint() }"

  // minimum length of begin and end
  def length = math.min(begin.length, end.length)

  // evaluate the number of points in the given index range
  def getTotalSize : IR_Expression = {
    // wrapping due to strategies not being applied to top level nodes
    val totalSize = IR_ExpressionStatement((end - begin).reduce(_ * _))
    SimplifyStrategy.doUntilDoneStandalone(totalSize)
    totalSize.expression
  }

  // interpret the index range as subset of a multidimensional space and linearize the given index in this subspace
  def linearizeIndex(index : IR_Index) : IR_Expression = IR_Linearization.linearizeIndex(index, end - begin)
}
