package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.domain.ir.IR_DomainFunctions
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.parallelization.api.mpi.MPI_AllReduce
import exastencils.parallelization.api.mpi.MPI_IV_MpiComm
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank

object IR_IV_FragmentInfo {
  var firstCall = true

  // determine number of valid fragments per block and total number of valid fragments
  def init(domainIdx : IR_Expression, calculateFragOffset : Boolean = false) : ListBuffer[IR_Statement] = {
    // prepare fragment info
    var statements = ListBuffer[IR_Statement]()

    // communicate only when necessary. for rect meshes the fragment info can be set with knowledge parameters
    if (Knowledge.domain_onlyRectangular) {
      statements += IR_Assignment(IR_IV_NumValidFrags(domainIdx), Knowledge.domain_numFragmentsPerBlock)
      statements += IR_Assignment(IR_IV_TotalNumFrags(domainIdx), Knowledge.domain_numFragmentsTotal)
      statements += IR_Assignment(IR_IV_FragmentOffset(domainIdx), MPI_IV_MpiRank * Knowledge.domain_numFragmentsPerBlock)
    } else {
      // the number of fragments per block is not uniformly distributed -> communicate fragment info
      if (firstCall) {
        // communicate once at startup
        val mpiInt = IR_VariableAccess(IR_IntegerDatatype.prettyprint_mpi, IR_UnknownDatatype)
        StateManager.findFirst[IR_DomainFunctions]().get.functions foreach {
          case func : IR_PlainFunction if func.name == "initGeometry" =>
            firstCall = false

            // total number of valid frags
            func.body += IR_Assignment(IR_IV_NumValidFrags(domainIdx), 0)
            func.body += IR_LoopOverFragments(IR_IfCondition(IR_IV_IsValidForDomain(0), IR_Assignment(IR_IV_NumValidFrags(domainIdx), IR_IV_NumValidFrags(domainIdx) + 1)))
            func.body += IR_Assignment(IR_IV_TotalNumFrags(domainIdx), IR_IV_NumValidFrags(domainIdx))
            if (Knowledge.mpi_enabled) {
              func.body += MPI_AllReduce(IR_AddressOf(IR_IV_TotalNumFrags(domainIdx)), IR_IntegerDatatype, 1, "+")
            }

            if (Knowledge.mpi_enabled) {
              // valid frags per block
              func.body += IR_FunctionCall(
                IR_ExternalFunctionReference("MPI_Allgather"),
                IR_AddressOf(IR_IV_NumValidFrags(domainIdx)), 1, mpiInt,
                IR_IV_NumValidFragsPerBlock(domainIdx), 1, mpiInt, MPI_IV_MpiComm
              )
            }
          case _                                                      =>
        }
      }

      // Special case: PrintVtk calculates the fragmentOffset in a MPI_Sequential and for fpp it needs to be zero
      // for other approaches this flag needs to be set
      statements ++= ListBuffer(IR_Assignment(IR_IV_FragmentOffset(domainIdx), 0))
      if (calculateFragOffset) {
        statements += IR_ForLoop(
          IR_VariableDeclaration(IR_IntegerDatatype, "curRank", 0),
          IR_Lower("curRank", MPI_IV_MpiRank),
          IR_PreIncrement("curRank"),
          IR_Assignment(IR_IV_FragmentOffset(domainIdx), IR_IV_NumValidFragsPerBlock(domainIdx).resolveAccess("curRank"), "+=")
        )
      }
    }

    statements
  }
}

case class IR_IV_TotalNumFrags(var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(false, true, false, false, false) {
  override def resolveName() : String = "totalNumFrags" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype
  override def resolveDefValue() : Option[IR_Expression] = Some(Knowledge.domain_numFragmentsTotal)
}

case class IR_IV_NumValidFragsPerBlock(var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(false, true, false, false, false) {
  override def resolveName() : String = "numValidFragsBlock" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype() : IR_Datatype = IR_ArrayDatatype(IR_IntegerDatatype, Knowledge.mpi_numThreads)
  override def resolveDefValue() : Option[IR_Expression] = Some(Knowledge.domain_numFragmentsPerBlock)

  override def getCtor() : Option[IR_Statement] = {
    val i = IR_VariableAccess("i", IR_IntegerDatatype)

    if (resolveDefValue().isDefined)
      Some(IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, Knowledge.mpi_numThreads), IR_PreIncrement(i),
        IR_Assignment(resolveAccess(i), resolveDefValue().get)))
    else
      None
  }

  def resolveAccess(index : IR_Expression) : IR_Expression = IR_ArrayAccess(this, index)
}

case class IR_IV_NumValidFrags(var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(false, true, false, false, false) {
  override def resolveName() : String = "numValidFrags" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype
  override def resolveDefValue() : Option[IR_Expression] = Some(Knowledge.domain_numFragmentsPerBlock)
}

case class IR_IV_FragmentOffset(var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(false, true, false, false, false) {
  override def resolveName() : String = "fragmentOffset" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype
  override def resolveDefValue() : Option[IR_Expression] = Some(0)
}