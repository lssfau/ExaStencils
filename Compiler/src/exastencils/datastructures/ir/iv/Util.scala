package exastencils.datastructures.ir.iv

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class Timer(var name : Expression) extends UnduplicatedVariable {
  override def resolveName = s"timer_" + stripName
  override def resolveDataType = "StopWatch"

  def stripName = name.prettyprint.replaceAll("[^a-zA-Z0-9]", "_")

  override def getCtor() : Option[Statement] = {
    Some(AssignmentStatement(resolveName ~ ".timerName", StringConstant(stripName)))
  }
}
