package exastencils.waLBerla.ir.communication

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.communication.ir.IR_HasMessageDirection
import exastencils.config.Knowledge
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember

abstract class IR_WaLBerlaCommVariable extends IR_WaLBerlaInterfaceMember(true, false, true) with IR_HasMessageDirection {

  def send : Boolean

  def neighIdx : IR_Expression

  def indexOfRefinedNeighbor : Option[IR_Expression]

  def fragmentIdx : IR_Expression

  def baseDatatype : IR_Datatype

  override def isPrivate : Boolean = true

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    val access = super.resolveAccess(baseAccess, fragment, domain, field, level, neigh)

    if (Knowledge.refinement_enabled) IR_ArrayAccess(access, if (indexOfRefinedNeighbor.isDefined) indexOfRefinedNeighbor.get else 0) else access
  }

  override def resolveDatatype() = if (Knowledge.refinement_enabled)
    IR_StdArrayDatatype(baseDatatype, Knowledge.refinement_maxFineNeighborsForCommAxis)
  else
    baseDatatype

  def resolveAccessOverWrappedLoops(explicitIndexOfRefinedNeighbor : Int) : IR_Expression = {
    val access = super.resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_LoopOverNeighbors.defIt)

    IR_ArrayAccess(access, explicitIndexOfRefinedNeighbor)
  }

  override def getCtor() : Option[IR_Statement] = {
    if (Knowledge.refinement_enabled && resolveDefValue().isDefined) {

      Some(wrapInLoops(IR_Scope((0 until Knowledge.refinement_maxFineNeighborsForCommAxis).map(i => IR_Assignment(resolveAccessOverWrappedLoops(i), resolveDefValue().get)) : _*)))
    } else {
      super.getCtor()
    }
  }
}
