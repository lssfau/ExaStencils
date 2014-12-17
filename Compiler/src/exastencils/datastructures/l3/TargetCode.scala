package exastencils.datastructures.l3

import exastencils.datastructures._

/** Dynamic computations */
object TargetCode {
  def apply(comps : Node*) = new TargetCode(comps.toList)
}
class TargetCode(val computation : List[Node]) {

  def ++(rhs : TargetCode) : TargetCode = {
    new TargetCode(computation ++ rhs.computation)
  }

  def toList() : List[Node] = computation
}

class DynamicRValue(
  computation : List[l4.Statement],
  val dcExpression : String,
  val scType : ScType)
    extends TargetCode(computation)

class DynamicLValue(
  computation : List[l4.Statement],
  val dcExpression : String,
  val scType : ScType) extends TargetCode(computation)

