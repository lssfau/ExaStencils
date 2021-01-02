package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_AddressOf
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnknownDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.parallelization.api.mpi.MPI_AllReduce
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank


object IR_IV_FragmentInfo {
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
      statements ++= ListBuffer(
        IR_Assignment(IR_IV_NumValidFrags(domainIdx), 0),
        IR_Assignment(IR_IV_FragmentOffset(domainIdx), 0),
        IR_LoopOverFragments(IR_IfCondition(IR_IV_IsValidForDomain(0), IR_Assignment(IR_IV_NumValidFrags(domainIdx), IR_IV_NumValidFrags(domainIdx) + 1))),
        IR_Assignment(IR_IV_TotalNumFrags(domainIdx), IR_IV_NumValidFrags(domainIdx)))

      if (Knowledge.mpi_enabled)
        statements += MPI_AllReduce(IR_AddressOf(IR_IV_TotalNumFrags(domainIdx)), IR_IntegerDatatype, 1, "+")

      // Special case: PrintVtk calculates the fragmentOffset in a MPI_Sequential, for other approaches this flag needs to be set
      if (calculateFragOffset) {
        val actualFragsPerBlock = IR_VariableAccess("validFragsPerBlock", IR_ArrayDatatype(IR_IntegerDatatype, Knowledge.mpi_numThreads))
        val mpiInt = IR_VariableAccess(IR_IntegerDatatype.prettyprint_mpi, IR_UnknownDatatype)
        val mpiComm = IR_VariableAccess("mpiCommunicator", IR_UnknownDatatype)
        statements += IR_VariableDeclaration(actualFragsPerBlock)
        statements += IR_FunctionCall(
          IR_ExternalFunctionReference("MPI_Allgather"),
          IR_AddressOf(IR_IV_NumValidFrags(domainIdx)), 1, mpiInt,
          actualFragsPerBlock, 1, mpiInt, mpiComm
        )
        statements += IR_ForLoop(
          IR_VariableDeclaration(IR_IntegerDatatype, "curRank", 0),
          IR_Lower("curRank", MPI_IV_MpiRank),
          IR_PreIncrement("curRank"),
          IR_Assignment(IR_IV_FragmentOffset(domainIdx), IR_ArrayAccess(actualFragsPerBlock, "curRank"), "+=")
        )
      }
    }

    statements
  }
}

case class IR_IV_TotalNumFrags(var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(false, true, false, false, false)  {
  override def resolveName() : String = "totalNumFrags" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype
  override def resolveDefValue() : Option[IR_Expression] = Some(Knowledge.domain_numFragmentsTotal)
}

case class IR_IV_NumValidFrags(var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(false, true, false, false, false)  {
  override def resolveName() : String = "numValidFrags" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype
  override def resolveDefValue() : Option[IR_Expression] = Some(Knowledge.domain_numFragmentsPerBlock)
}

case class IR_IV_FragmentOffset(var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(false, true, false, false, false) {
  override def resolveName() : String = "fragmentOffset" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype
  override def resolveDefValue() : Option[IR_Expression] = Some(0)
}