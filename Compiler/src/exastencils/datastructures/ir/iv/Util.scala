package exastencils.datastructures.ir.iv

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class Timer(var name : Expression) extends UnduplicatedVariable {
  // TODO: strip result of resolveName (no spaces, etc.)
  override def resolveName = s"timer_" + name.prettyprint
  override def resolveDataType = "StopWatch"

  override def getCtor() : Option[Statement] = {
    Some(AssignmentStatement(resolveName ~ ".timerName", s""""${name}""""))
  }
}
