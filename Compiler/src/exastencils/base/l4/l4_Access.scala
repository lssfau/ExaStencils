package exastencils.base.l4

import exastencils.base.ir._
import exastencils.core.collectors.L4VariableCollector
import exastencils.datastructures._
import exastencils.datastructures.l4.UnresolvedAccess
import exastencils.prettyprinting._

trait L4_Access extends L4_Expression {
  def name : String
  // FIXME: override def progress : IR_Access
}

/// L4_VariableAccess

case class L4_VariableAccess(var name : String, var datatype : L4_Datatype) extends L4_Access {
  override def prettyprint(out : PpStream) : Unit = out << name
  override def progress = IR_VariableAccess(name, datatype.progress)
}

/// L4_ResolveVariableAccesses

object L4_ResolveVariableAccesses extends DefaultStrategy("Resolve variable accesses") {
  var collector = new L4VariableCollector
  register(collector)

  // FIXME: apply to globals first

  this += new Transformation("Resolve variable accesses", {
    case access @ UnresolvedAccess(accessName, _, Some(L4_SingleLevel(level)), _, _, _) if collector.exists(accessName + "@@" + level) =>
      L4_VariableAccess(accessName, collector.getValue(accessName + "@@" + level).get)
    case access @ UnresolvedAccess(accessName, _, None, _, _, _) if collector.exists(accessName)                                       =>
      L4_VariableAccess(accessName, collector.getValue(accessName).get)
  })
}
