package exastencils.base.l4

import exastencils.base.ir._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.prettyprinting._
import exastencils.util.l4.L4_VariableCollector

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
  var collector = new L4_VariableCollector
  register(collector)

  // FIXME: apply to globals first

  this += new Transformation("Resolve variable accesses", {
    case access @ L4_UnresolvedAccess(accessName, Some(L4_SingleLevel(level)), _, _, _, _) if collector.exists(accessName + "@@" + level) =>
      // TODO: why doesn't L4_VA have a level?
      L4_VariableAccess(accessName + "_" + level, collector.getValue(accessName + "@@" + level).get)
    case access @ L4_UnresolvedAccess(accessName, _, None, _, _, _) if collector.exists(accessName)                                       =>
      L4_VariableAccess(accessName, collector.getValue(accessName).get)
  })
}
