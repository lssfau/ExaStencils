package exastencils.mpi

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.omp._
import exastencils.prettyprinting._
import exastencils.util._

trait MPI_Statement extends IR_Statement

// TODO: replace pp with expand where suitable

case class MPI_IsRootProc() extends IR_Expression {
  override def datatype = IR_BooleanDatatype
  override def prettyprint(out : PpStream) : Unit = out << "0 == mpiRank"
}

case class MPI_Init() extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "MPI_Init(&argc, &argv);"
}

case class MPI_Finalize() extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "MPI_Finalize();"
}

case class MPI_SetRankAndSize(var communicator : IR_Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "int mpiRank;\n"
    out << "int mpiSize;\n"
    out << "MPI_Comm_rank(" << communicator << ", &mpiRank);\n"
    out << "MPI_Comm_size(" << communicator << ", &mpiSize);\n"
  }
}

case class MPI_Receive(var buffer : IR_Expression, var size : IR_Expression, var datatype : IR_Datatype, var rank : IR_Expression, var tag : IR_Expression, var request : IR_Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "MPI_Irecv(" << buffer << ", " << size << ", " << datatype.prettyprint_mpi << ", " << rank << ", " << tag << ", mpiCommunicator, &" << request << ");"
  }
}

case class MPI_Send(var buffer : IR_Expression, var size : IR_Expression, var datatype : IR_Datatype, var rank : IR_Expression, var tag : IR_Expression, var request : IR_Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "MPI_Isend(" << buffer << ", " << size << ", " << datatype.prettyprint_mpi << ", " << rank << ", " << tag << ", mpiCommunicator, &" << request << ");"
  }
}

case class MPI_Allreduce(var sendbuf : IR_Expression, var recvbuf : IR_Expression, var datatype : IR_Datatype, var count : IR_Expression, var op : IR_Expression) extends MPI_Statement {
  def this(sendbuf : IR_Expression, recvbuf : IR_Expression, datatype : IR_Datatype, count : IR_Expression, op : String) = this(sendbuf, recvbuf, datatype, count, MPI_Allreduce.mapOp(op))
  def this(buf : IR_Expression, datatype : IR_Datatype, count : IR_Expression, op : IR_Expression) = this("MPI_IN_PLACE", buf, datatype, count, op)
  def this(buf : IR_Expression, datatype : IR_Datatype, count : IR_Expression, op : String) = this("MPI_IN_PLACE", buf, datatype, count, MPI_Allreduce.mapOp(op))

  override def prettyprint(out : PpStream) : Unit = {
    out << "MPI_Allreduce(" << sendbuf << ", " << recvbuf << ", " << count << ", " << datatype.prettyprint_mpi << ", " << op << ", mpiCommunicator);"
  }
}

object MPI_Allreduce {
  def mapOp(op : String) : IR_Expression = {
    op match {
      case "+"   => "MPI_SUM"
      case "*"   => "MPI_PROD"
      case "min" => "MPI_MIN"
      case "max" => "MPI_MAX"
      case _     => "FIXME"
    }
  }
}

case class MPI_Reduce(var root : IR_Expression, var sendbuf : IR_Expression, var recvbuf : IR_Expression, var datatype : IR_Datatype, var count : IR_Expression, var op : IR_Expression) extends MPI_Statement {
  def this(root : IR_Expression, sendbuf : IR_Expression, recvbuf : IR_Expression, datatype : IR_Datatype, count : IR_Expression, op : String) = this(root, sendbuf, recvbuf, datatype, count, MPI_Allreduce.mapOp(op))
  def this(root : IR_Expression, buf : IR_Expression, datatype : IR_Datatype, count : IR_Expression, op : IR_Expression) = this(root, "MPI_IN_PLACE", buf, datatype, count, op)
  def this(root : IR_Expression, buf : IR_Expression, datatype : IR_Datatype, count : IR_Expression, op : String) = this(root, "MPI_IN_PLACE", buf, datatype, count, MPI_Allreduce.mapOp(op))

  def reallyPrint(out : PpStream) : Unit = {
    out << "MPI_Reduce(" << sendbuf << ", " << recvbuf << ", " << count << ", " << datatype.prettyprint_mpi << ", " << op << ", " << root << ", mpiCommunicator);"
  }

  override def prettyprint(out : PpStream) : Unit = {
    sendbuf match {
      case IR_StringLiteral("MPI_IN_PLACE") => // special handling for MPI_IN_PLACE required
        out << "if (" << IR_EqEqExpression(root, "mpiRank") << ") {\n"
        MPI_Reduce(root, sendbuf, recvbuf, datatype, count, op).reallyPrint(out) // MPI_IN_PLACE for root proc
        out << "\n} else {\n"
        MPI_Reduce(root, recvbuf, recvbuf, datatype, count, op).reallyPrint(out) // same behavior, different call required on all other procs -.-
        out << "\n}"
      case _                                => reallyPrint(out) // otherwise a simple print suffices
    }

  }
}

case class MPI_Gather(var sendbuf : IR_Expression, var recvbuf : IR_Expression, var datatype : IR_Datatype, var count : IR_Expression) extends MPI_Statement {
  def this(buf : IR_Expression, datatype : IR_Datatype, count : IR_Expression) = this("MPI_IN_PLACE", buf, datatype, count)

  override def prettyprint(out : PpStream) : Unit = {
    (out << "MPI_Gather("
      << sendbuf << ", " << count << ", " << datatype.prettyprint_mpi << ", "
      << recvbuf << ", " << count << ", " << datatype.prettyprint_mpi << ", "
      << 0 << ", mpiCommunicator);")
  }
}

case class MPI_Barrier() extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "MPI_Barrier(mpiCommunicator);"
}

case class MPI_DataType(var field : FieldSelection, var indexRange : IndexRange, var condition : Option[IR_Expression]) extends IR_Datatype {
  override def prettyprint(out : PpStream) : Unit = out << generateName
  override def prettyprint_mpi = generateName

  override def dimensionality : Int = ???
  override def getSizeArray : Array[Int] = ???
  override def resolveBaseDatatype : IR_Datatype = field.field.resolveBaseDatatype
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???

  if (!MPI_DataType.shouldBeUsed(indexRange, condition))
    Logger.warn(s"Trying to setup an MPI data type for unsupported index range ${ indexRange.print }")

  // determine data type parameters
  var blockLengthExp : IR_Expression = indexRange.end(0) - indexRange.begin(0)
  var blockCountExp : IR_Expression = 1
  var strideExp : IR_Expression = field.fieldLayout(0).total

  var done = false
  for (dim <- 1 until indexRange.size; if !done) {
    if (1 == SimplifyExpression.evalIntegral(indexRange.end(dim) - indexRange.begin(dim))) {
      strideExp *= field.fieldLayout.defIdxById("TOT", dim)
    } else {
      blockCountExp = indexRange.end(dim) - indexRange.begin(dim)
      done = true // break
    }
  }

  val blockLength = SimplifyExpression.evalIntegral(blockLengthExp)
  val blockCount = SimplifyExpression.evalIntegral(blockCountExp)
  val stride = SimplifyExpression.evalIntegral(strideExp)

  def generateName : String = s"mpiDatatype_${ blockCount }_${ blockLength }_${ stride }"
  def mpiTypeNameArg : IR_Expression = IR_AddressofExpression(generateName)

  def generateDecl : IR_VariableDeclaration = {
    IR_VariableDeclaration("MPI_Datatype", generateName)
  }

  def generateCtor : ListBuffer[IR_Statement] = {
    val scalarDatatype = field.field.resolveBaseDatatype.prettyprint_mpi

    // compile statement(s)
    ListBuffer[IR_Statement](
      IR_FunctionCall("MPI_Type_vector", ListBuffer[IR_Expression](blockCount, blockLength, stride, scalarDatatype, mpiTypeNameArg)),
      IR_FunctionCall("MPI_Type_commit", ListBuffer(mpiTypeNameArg)))
  }

  def generateDtor : ListBuffer[IR_Statement] = {
    ListBuffer[IR_Statement](
      IR_FunctionCall("MPI_Type_free", ListBuffer(mpiTypeNameArg)))
  }
}

object MPI_DataType {
  def shouldBeUsed(indexRange : IndexRange, condition : Option[IR_Expression]) : Boolean = {
    if (!Knowledge.mpi_useCustomDatatypes || Knowledge.data_genVariableFieldSizes)
      false
    else if (condition.isDefined) // skip communication steps with conditions for now
      false
    else {
      val numNonDummyDims = (1 until indexRange.size).map(dim => // size in the zero dimension is irrelevant
        if (SimplifyExpression.evalIntegral(indexRange.end(dim) - indexRange.begin(dim)) > 1) 1 else 0).reduce(_ + _)
      return (numNonDummyDims <= 1) // avoid nested data types for now
    }
  }
}

case class MPI_Sequential(var body : ListBuffer[IR_Statement]) extends IR_Statement with IR_Expandable {
  def this(body : IR_Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand : Output[IR_ForLoop] = {
    IR_ForLoop(
      IR_VariableDeclaration(IR_IntegerDatatype, "curRank", 0),
      IR_LowerExpression("curRank", Knowledge.mpi_numThreads),
      IR_PreIncrementExpression("curRank"),
      ListBuffer[IR_Statement](
        MPI_Barrier(),
        IR_IfCondition(IR_EqEqExpression("mpiRank", "curRank"), body)))
  }
}

case class MPI_WaitForRequest() extends IR_AbstractFunction with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "waitForMPIReq"

  override def expand : Output[IR_Function] = {
    def request = IR_VariableAccess("request", IR_PointerDatatype(IR_SpecialDatatype("MPI_Request")))
    def stat = IR_VariableAccess("stat", IR_SpecialDatatype("MPI_Status"))
    def flag = IR_VariableAccess("flag", IR_IntegerDatatype)
    def result = IR_VariableAccess("result", IR_IntegerDatatype)

    def msg = IR_VariableAccess("msg", IR_ArrayDatatype(IR_SpecialDatatype("char"), 64 * 1024))
    def len = IR_VariableAccess("len", IR_IntegerDatatype)

    if (Knowledge.mpi_useBusyWait) {
      IR_Function(IR_UnitDatatype, name, ListBuffer(IR_FunctionArgument(request.name, request.innerDatatype.get)),
        ListBuffer[IR_Statement](
          IR_VariableDeclaration(stat),
          IR_VariableDeclaration(result),
          IR_VariableDeclaration(flag, 0),
          IR_WhileLoop(IR_EqEqExpression(0, flag),
            new IR_Assignment(result, IR_FunctionCall("MPI_Test", ListBuffer(
              request, IR_AddressofExpression(flag), IR_AddressofExpression(stat)))) with OMP_PotentiallyCritical,
            IR_IfCondition(IR_EqEqExpression("MPI_ERR_IN_STATUS", result), ListBuffer[IR_Statement](
              IR_VariableDeclaration(msg),
              IR_VariableDeclaration(len),
              IR_FunctionCall("MPI_Error_string", ListBuffer(
                MemberAccess(stat, "MPI_ERROR"), msg, IR_AddressofExpression(len))),
              PrintStatement(ListBuffer[IR_Expression]("\"MPI Error encountered (\"", msg, "\")\""))))),
          IR_Assignment(DerefAccess(request), IR_FunctionCall("MPI_Request"))),
        false)
    } else {
      IR_Function(IR_UnitDatatype, s"waitForMPIReq", ListBuffer(IR_FunctionArgument(request.name, request.innerDatatype.get)),
        ListBuffer[IR_Statement](
          IR_VariableDeclaration(stat),
          IR_VariableDeclaration(result),
          new IR_Assignment(result, IR_FunctionCall("MPI_Wait", ListBuffer[IR_Expression](request, IR_AddressofExpression(stat)))) with OMP_PotentiallyCritical,
          IR_IfCondition(IR_EqEqExpression("MPI_ERR_IN_STATUS", result), ListBuffer[IR_Statement](
            IR_VariableDeclaration(msg),
            IR_VariableDeclaration(len),
            IR_FunctionCall("MPI_Error_string", ListBuffer(
              MemberAccess(stat, "MPI_ERROR"), msg, IR_AddressofExpression(len))),
            PrintStatement(ListBuffer[IR_Expression]("\"MPI Error encountered (\"", msg, "\")\"")))),
          IR_Assignment(DerefAccess(request), IR_FunctionCall("MPI_Request"))),
        false)
    }
  }
}
