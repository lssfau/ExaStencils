package exastencils.globals.ir

import exastencils.base.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.ir._

/// IR_ResolveConstIVs

// TODO: split and move to appropriate modules
object IR_ResolveConstIVs extends DefaultStrategy("Resolve constant internal variables") {
  override def apply(applyAtNode : Option[Node]) = {
    this.transaction()

    if (IR_DomainCollection.objects.size <= 1)
      this.execute(new Transformation("Resolve IsValidForSubdomain", {
        case IR_Assignment(_ : IR_IV_IsValidForDomain, _, _) => IR_NullStatement
        case _ : IR_IV_IsValidForDomain                      => IR_BooleanConstant(true)
      }))

    if (!Knowledge.mpi_enabled || Knowledge.mpi_numThreads <= 1)
      this.execute(new Transformation("Resolve NeighborIsRemote and NeighborRemoteRank", {
        case IR_Assignment(_ : IR_IV_NeighborIsRemote, _, _)   => IR_NullStatement
        case _ : IR_IV_NeighborIsRemote                        => IR_BooleanConstant(false)
        case IR_Assignment(_ : IR_IV_NeighborRemoteRank, _, _) => IR_NullStatement
        case _ : IR_IV_NeighborRemoteRank                      => IR_IntegerConstant(-1)
      }))

    if (Knowledge.domain_numFragmentsTotal <= 1 && !Knowledge.domain_rect_hasPeriodicity) {
      this.execute(new Transformation("Resolve NeighborIsValid", {
        case IR_Assignment(_ : IR_IV_NeighborIsValid, _, _) => IR_NullStatement
        case _ : IR_IV_NeighborIsValid                      => IR_BooleanConstant(false)
      }))
    } else if (Knowledge.domain_rect_generate) {
      for (dim <- 0 until Knowledge.dimensionality)
        if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1 && !Knowledge.domain_rect_periodicAsVec(dim))
          this.execute(new Transformation(s"Resolve NeighborIsValid in dimension $dim", {
            case IR_Assignment(niv : IR_IV_NeighborIsValid, _, _) if niv.neighIdx.isInstanceOf[IR_IntegerConstant]
              && (DefaultNeighbors.neighbors(niv.neighIdx.asInstanceOf[IR_IntegerConstant].value.toInt).dir(dim) != 0) => IR_NullStatement
            case niv : IR_IV_NeighborIsValid if niv.neighIdx.isInstanceOf[IR_IntegerConstant]
              && (DefaultNeighbors.neighbors(niv.neighIdx.asInstanceOf[IR_IntegerConstant].value.toInt).dir(dim) != 0) => IR_BooleanConstant(false)
          }))
    }

    if (Knowledge.domain_numFragmentsPerBlock <= 1 || Knowledge.comm_disableLocalCommSync) {
      this.execute(new Transformation("Resolve local synchronization", {
        case IR_Assignment(_ : IR_IV_LocalCommReady, _, _) => IR_NullStatement
        case _ : IR_IV_LocalCommReady                      => IR_BooleanConstant(true)

        case IR_Assignment(_ : IR_IV_LocalCommDone, _, _) => IR_NullStatement
        case _ : IR_IV_LocalCommDone                      => IR_BooleanConstant(true)

        case IR_FunctionCall(IR_InternalFunctionAccess("waitForFlag", _), _) => IR_NullExpression
      }))
    }

    this.commit()
  }
}
