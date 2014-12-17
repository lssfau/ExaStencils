package exastencils.datastructures.l3

/** Dynamic computations */
class DestinationCode(val computation: String) {
  override def toString(): String = computation
}
class DynamicRValue(
  override val computation: String,
  val dcExpression: String,
  val scType: ScType)
  extends DestinationCode(computation)

class DynamicLValue(
  override val computation: String,
  val dcExpression: String,
  val scType: ScType) extends DestinationCode(computation)


