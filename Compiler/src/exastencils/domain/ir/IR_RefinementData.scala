package exastencils.domain.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge

abstract class IR_RefinementData(
    canBePerFragment : Boolean,
    canBePerDomain : Boolean,
    canBePerField : Boolean,
    canBePerLevel : Boolean,
    canBePerNeighbor : Boolean,
    canBePerRefinedNeighbor : Boolean
) extends IR_InternalVariable(canBePerFragment, canBePerDomain, canBePerField, canBePerLevel, canBePerNeighbor) {

  def usesRefinedNeighborArrays : Boolean = true

  override def getDeclaration() : IR_VariableDeclaration = {
    var datatype : IR_Datatype = resolveDatatype()

    if (canBePerRefinedNeighbor && usesRefinedNeighborArrays && Knowledge.refinement_enabled)
      datatype = IR_ArrayDatatype(datatype, Knowledge.refinement_maxFineNeighborsForCommAxis)

    new IR_VariableDeclaration(datatype, resolveName())
  }

  override def wrapInLoops(body : IR_Statement) : IR_Statement = {
    var wrappedBody = super.wrapInLoops(body)

    if (canBePerRefinedNeighbor && usesRefinedNeighborArrays && Knowledge.refinement_enabled)
      wrappedBody = IR_LoopOverRefinedNeighbors(wrappedBody)

    wrappedBody
  }

  override def getCtor() : Option[IR_Statement] = {
    if (resolveDefValue().isDefined)
      Some(wrapInLoops(IR_Assignment(resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt, IR_LoopOverRefinedNeighbors.defIt), resolveDefValue().get)))
    else
      None
  }

  def resolvePostfix(fragment : String, domain : String, field : String, level : String, neigh : String, refinedNeigh : IR_Expression) : String = {
    var postfix : String = super.resolvePostfix(fragment, domain, field, level, neigh)

    if (canBePerRefinedNeighbor && usesRefinedNeighborArrays && Knowledge.refinement_enabled)
      postfix += "_" + refinedNeigh

    postfix
  }

  def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression, refinedNeigh : IR_Expression) : IR_Expression = {
    var access = baseAccess

    if (canBePerRefinedNeighbor && usesRefinedNeighborArrays && Knowledge.refinement_enabled)
      access = IR_ArrayAccess(access, refinedNeigh)

    super.resolveAccess(access, fragment, domain, field, level, neigh)
  }
}
