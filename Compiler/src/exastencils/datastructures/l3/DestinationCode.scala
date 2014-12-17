package exastencils.datastructures.l3

import exastencils.datastructures._

/** Dynamic computations */
object DestinationCode {
  def apply(comps : l4.Statement*) = new DestinationCode(comps.toList)
}
class DestinationCode(val computation : List[l4.Statement]) {

  def ++(rhs : DestinationCode) : DestinationCode = {
    new DestinationCode(computation ++ rhs.computation)
  }

  def toList() : List[l4.Statement] = computation
}

class DynamicRValue(
  computation : List[l4.Statement],
  val dcExpression : String,
  val scType : ScType)
    extends DestinationCode(computation)

class DynamicLValue(
  computation : List[l4.Statement],
  val dcExpression : String,
  val scType : ScType) extends DestinationCode(computation)

