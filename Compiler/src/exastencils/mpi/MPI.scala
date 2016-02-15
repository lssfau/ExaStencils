package exastencils.mpi

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.break

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.StatementList
import exastencils.knowledge._
import exastencils.logger._
import exastencils.omp._
import exastencils.prettyprinting._
import exastencils.util._

trait MPI_Statement extends Statement

// TODO: replace pp with expand where suitable

case class MPI_IsRootProc() extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "0 == mpiRank"
}

case class MPI_Init() extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "MPI_Init(&argc, &argv);"
}

case class MPI_Finalize() extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "MPI_Finalize();"
}

case class MPI_SetRankAndSize(var communicator : Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "int mpiRank;\n"
    out << "int mpiSize;\n"
    out << "MPI_Comm_rank(" << communicator << ", &mpiRank);\n"
    out << "MPI_Comm_size(" << communicator << ", &mpiSize);\n"
  }
}

case class MPI_Receive(var buffer : Expression, var size : Expression, var datatype : Datatype, var rank : Expression, var tag : Expression, var request : Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "MPI_Irecv(" << buffer << ", " << size << ", " << datatype.prettyprint_mpi << ", " << rank << ", " << tag << ", mpiCommunicator, &" << request << ");"
  }
}

case class MPI_Send(var buffer : Expression, var size : Expression, var datatype : Datatype, var rank : Expression, var tag : Expression, var request : Expression) extends MPI_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "MPI_Isend(" << buffer << ", " << size << ", " << datatype.prettyprint_mpi << ", " << rank << ", " << tag << ", mpiCommunicator, &" << request << ");"
  }
}

case class MPI_Allreduce(var sendbuf : Expression, var recvbuf : Expression, var datatype : Datatype, var count : Expression, var op : Expression) extends MPI_Statement {
  def this(sendbuf : Expression, recvbuf : Expression, datatype : Datatype, count : Expression, op : String) = this(sendbuf, recvbuf, datatype, count, MPI_Allreduce.mapOp(op))
  def this(buf : Expression, datatype : Datatype, count : Expression, op : Expression) = this("MPI_IN_PLACE", buf, datatype, count, op)
  def this(buf : Expression, datatype : Datatype, count : Expression, op : String) = this("MPI_IN_PLACE", buf, datatype, count, MPI_Allreduce.mapOp(op))

  override def prettyprint(out : PpStream) : Unit = {
    out << "MPI_Allreduce(" << sendbuf << ", " << recvbuf << ", " << count << ", " << datatype.prettyprint_mpi << ", " << op << ", mpiCommunicator);"
  }
}

object MPI_Allreduce {
  def mapOp(op : String) : Expression = {
    op match {
      case "+"   => "MPI_SUM"
      case "*"   => "MPI_PROD"
      case "min" => "MPI_MIN"
      case "max" => "MPI_MAX"
      case _     => "FIXME"
    }
  }
}

case class MPI_Gather(var sendbuf : Expression, var recvbuf : Expression, var datatype : Datatype, var count : Expression) extends MPI_Statement {
  def this(buf : Expression, datatype : Datatype, count : Expression) = this("MPI_IN_PLACE", buf, datatype, count)

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

case class MPI_DataType(var field : FieldSelection, var indices : IndexRange) extends Datatype {
  override def prettyprint(out : PpStream) : Unit = out << generateName
  override def prettyprint_mpi = generateName

  override def dimensionality : Int = ???
  override def getSizeArray : Array[Int] = ???
  override def resolveUnderlyingDatatype : Datatype = ???
  override def resolvePostscript : String = ""
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???

  var count : Int = 0
  var blocklen : Int = 0
  var stride : Int = 0

  // this assumes that the conditions implied by 'shouldBeUsed' are fulfilled
  if (1 == SimplifyExpression.evalIntegral(indices.end(2) - indices.begin(2))) {
    count = SimplifyExpression.evalIntegral(indices.end(1) - indices.begin(1)).toInt
    blocklen = SimplifyExpression.evalIntegral(indices.end(0) - indices.begin(0)).toInt
    stride = field.fieldLayout.defIdxById("TOT", 0)
  } else if (1 == SimplifyExpression.evalIntegral(indices.end(1) - indices.begin(1))) {
    count = SimplifyExpression.evalIntegral(indices.end(2) - indices.begin(2)).toInt
    blocklen = SimplifyExpression.evalIntegral(indices.end(0) - indices.begin(0)).toInt
    stride = field.fieldLayout.defIdxById("TOT", 0) * field.fieldLayout.defIdxById("TOT", 1)
  }

  def generateName : String = {
    s"mpiDatatype_${count}_${blocklen}_${stride}"
  }

  def generateDecl : VariableDeclarationStatement = {
    VariableDeclarationStatement("MPI_Datatype", generateName)
  }

  def generateCtor : ListBuffer[Statement] = {
    ListBuffer[Statement](
      s"MPI_Type_vector($count, $blocklen, $stride, MPI_DOUBLE, &${generateName})",
      s"MPI_Type_commit(&${generateName})")
  }

  def generateDtor : ListBuffer[Statement] = {
    ListBuffer[Statement](
      s"MPI_Type_free(&${generateName})")
  }
}

object MPI_DataType {
  def shouldBeUsed(indexRange : IndexRange) : Boolean = {
    if (!Knowledge.mpi_useCustomDatatypes || Knowledge.data_genVariableFieldSizes)
      false
    else {
      val numNonDummyDims = (1 until indexRange.size).map(dim => // size in the zero dimension is irrelevant
        if (SimplifyExpression.evalIntegral(indexRange.end(dim) - indexRange.begin(dim)) > 1) 1 else 0).reduce(_ + _)
      return (numNonDummyDims <= 1) // avoid nested data types for now
    }
  }
}

case class InitMPIDataType(mpiTypeName : String, field : Field, indexRange : IndexRange) extends MPI_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = InitMPIDataType\n"

  override def expand : Output[StatementList] = {
    if (!MPI_DataType.shouldBeUsed(indexRange))
      Logger.warn(s"Trying to setup an MPI data type for unsupported index range ${indexRange.print}")

    // determine data type parameters
    var blockLength : Expression = indexRange.end(0) - indexRange.begin(0) + 1
    var blockCount : Expression = 1
    var stride : Expression = field.fieldLayout(0).total

    for (dim <- 1 until indexRange.size) {
      if (1 == SimplifyExpression.evalIntegral(indexRange.end(dim) - indexRange.begin(dim))) {
        stride *= field.fieldLayout(dim).total
      } else {
        blockCount = indexRange.end(dim) - indexRange.begin(dim) + 1
        break
      }
    }

    def mpiTypeNameArg = AddressofExpression(mpiTypeName)
    val scalarDatatype = field.fieldLayout.scalarDataType.prettyprint_mpi

    // compile statement(s)
    var stmts = ListBuffer[Statement]()
    stmts += FunctionCallExpression("MPI_Type_vector", ListBuffer(blockCount, blockLength, stride, scalarDatatype, mpiTypeNameArg))
    stmts += FunctionCallExpression("MPI_Type_commit", ListBuffer(mpiTypeNameArg))
    stmts
  }
}

case class MPI_Sequential(var body : ListBuffer[Statement]) extends Statement with Expandable {
  def this(body : Statement) = this(ListBuffer(body))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = MPI_Sequential\n"

  override def expand : Output[ForLoopStatement] = {
    ForLoopStatement(
      VariableDeclarationStatement(IntegerDatatype, "curRank", Some(0)),
      LowerExpression("curRank", Knowledge.mpi_numThreads),
      PreIncrementExpression("curRank"),
      ListBuffer[Statement](
        MPI_Barrier(),
        new ConditionStatement(EqEqExpression("mpiRank", "curRank"), body)))
  }
}

case class MPI_WaitForRequest() extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = WaitForMPIReq\n"
  override def prettyprint_decl : String = prettyprint

  override def expand : Output[FunctionStatement] = {
    def request = VariableAccess("request", Some(PointerDatatype(SpecialDatatype("MPI_Request"))))
    def stat = VariableAccess("stat", Some(SpecialDatatype("MPI_Status")))
    def flag = VariableAccess("flag", Some(IntegerDatatype))
    def result = VariableAccess("result", Some(IntegerDatatype))

    def msg = VariableAccess("msg", Some(ArrayDatatype(SpecialDatatype("char"), 64 * 1024)))
    def len = VariableAccess("len", Some(IntegerDatatype))

    if (Knowledge.mpi_useBusyWait) {
      FunctionStatement(UnitDatatype, s"waitForMPIReq", ListBuffer(request),
        ListBuffer[Statement](
          new VariableDeclarationStatement(stat),
          new VariableDeclarationStatement(result),
          new VariableDeclarationStatement(flag, 0),
          new WhileLoopStatement(EqEqExpression(0, flag),
            new AssignmentStatement(result, FunctionCallExpression("MPI_Test", ListBuffer(
              request, AddressofExpression(flag), AddressofExpression(stat)))) with OMP_PotentiallyCritical,
            new ConditionStatement(EqEqExpression("MPI_ERR_IN_STATUS", result), ListBuffer[Statement](
              new VariableDeclarationStatement(msg),
              new VariableDeclarationStatement(len),
              new FunctionCallExpression("MPI_Error_string", ListBuffer[Expression](
                MemberAccess(stat, VariableAccess("MPI_ERROR", Some(IntegerDatatype))), msg, AddressofExpression(len))),
              new PrintStatement(ListBuffer[Expression]("\"MPI Error encountered (\"", msg, "\")\""))))),
          new AssignmentStatement(DerefAccess(request), FunctionCallExpression("MPI_Request", ListBuffer()))),
        false)
    } else {
      FunctionStatement(UnitDatatype, s"waitForMPIReq", ListBuffer(request),
        ListBuffer[Statement](
          new VariableDeclarationStatement(stat),
          new VariableDeclarationStatement(result),
          new AssignmentStatement(result, FunctionCallExpression("MPI_Wait", ListBuffer(request, AddressofExpression(stat)))) with OMP_PotentiallyCritical,
          new ConditionStatement(EqEqExpression("MPI_ERR_IN_STATUS", result), ListBuffer[Statement](
            new VariableDeclarationStatement(msg),
            new VariableDeclarationStatement(len),
            new FunctionCallExpression("MPI_Error_string", ListBuffer[Expression](
              MemberAccess(stat, VariableAccess("MPI_ERROR", Some(IntegerDatatype))), msg, AddressofExpression(len))),
            new PrintStatement(ListBuffer[Expression]("\"MPI Error encountered (\"", msg, "\")\"")))),
          new AssignmentStatement(DerefAccess(request), FunctionCallExpression("MPI_Request", ListBuffer()))),
        false)
    }
  }
}
