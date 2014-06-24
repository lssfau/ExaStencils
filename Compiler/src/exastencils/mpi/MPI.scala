package exastencils.mpi

import scala.collection.mutable.ListBuffer
import exastencils.core.collectors._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.omp._
import exastencils.knowledge._
import exastencils.strategies._
import exastencils.util._

case class MPI_IsRootProc() extends Expression {
  def cpp : String = { s"0 == mpiRank" }
}

case class MPI_Init() extends Statement {
  def cpp : String = {
    (s"MPI_Init(&argc, &argv);")
  }
}

case class MPI_Finalize() extends Statement {
  def cpp : String = {
    (s"MPI_Finalize();")
  }
}

case class MPI_SetRankAndSize(var communicator : Expression) extends Statement {
  def cpp : String = {
    (s"int mpiRank;\n"
      + s"int mpiSize;\n"
      + s"MPI_Comm_rank(${communicator.cpp}, &mpiRank);\n"
      + s"MPI_Comm_size(${communicator.cpp}, &mpiSize);\n")
  }
}

case class MPI_Receive(var buffer : Expression, var size : Expression, var datatype : Datatype, var rank : Expression, var tag : Expression, var request : Expression) extends Statement {
  def cpp : String = {
    (s"MPI_Irecv(${buffer.cpp}, ${size.cpp}, ${datatype.cpp_mpi}, ${rank.cpp}, ${tag.cpp}, mpiCommunicator, &${request.cpp});")
  }
}

case class MPI_Send(var buffer : Expression, var size : Expression, var datatype : Datatype, var rank : Expression, var tag : Expression, var request : Expression) extends Statement {
  def cpp : String = {
    (s"MPI_Isend(${buffer.cpp}, ${size.cpp}, ${datatype.cpp_mpi}, ${rank.cpp}, ${tag.cpp}, mpiCommunicator, &${request.cpp});")
  }
}

case class MPI_Allreduce(var sendbuf : Expression, var recvbuf : Expression, var datatype : Datatype, var count : Expression, var op : Expression) extends Statement {
  def this(sendbuf : Expression, recvbuf : Expression, datatype : Datatype, count : Expression, op : BinaryOperators.Value) =
    this(sendbuf, recvbuf, datatype, count, (op match {
      case BinaryOperators.Addition       => "MPI_SUM"
      case BinaryOperators.Multiplication => "MPI_PROD"
      case _                              => "FIXME"
    }) : Expression)
  def this(buf : Expression, datatype : Datatype, count : Expression, op : Expression) = this("MPI_IN_PLACE", buf, datatype, count, op)
  def this(buf : Expression, datatype : Datatype, count : Expression, op : BinaryOperators.Value) = this("MPI_IN_PLACE", buf, datatype, count, op)

  def cpp : String = {
    (s"MPI_Allreduce(${sendbuf.cpp}, ${recvbuf.cpp}, ${count.cpp}, ${datatype.cpp_mpi}, ${op.cpp}, mpiCommunicator);")
  }
}

case class MPI_Barrier() extends Statement {
  def cpp : String = {
    (s"MPI_Barrier(mpiCommunicator);")
  }
}

case class MPI_DataType(var field : FieldSelection, var indices : IndexRange) extends Datatype {
  override def cpp = generateName
  override def cpp_mpi = generateName

  var count : Int = 0
  var blocklen : Int = 0
  var stride : Int = 0

  // this assumes that the conditions implied by 'shouldBeUsed' are fulfilled 
  if (1 == SimplifyExpression.evalIntegral(indices.end(2) - indices.begin(2))) {
    count = SimplifyExpression.evalIntegral(indices.end(1) - indices.begin(1)).toInt
    blocklen = SimplifyExpression.evalIntegral(indices.end(0) - indices.begin(0)).toInt
    stride = field.layout(0).total
  } else if (1 == SimplifyExpression.evalIntegral(indices.end(1) - indices.begin(1))) {
    count = SimplifyExpression.evalIntegral(indices.end(2) - indices.begin(2)).toInt
    blocklen = SimplifyExpression.evalIntegral(indices.end(0) - indices.begin(0)).toInt
    stride = field.layout(0).total * field.layout(1).total
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
  def shouldBeUsed(indices : IndexRange) : Boolean = {
    if (!Knowledge.mpi_useCustomDatatypes)
      false
    else
      Knowledge.dimensionality match {
        case 2 => (
          1 == SimplifyExpression.evalIntegral(indices.end(1) - indices.begin(1)) || 1 == SimplifyExpression.evalIntegral(indices.end(2) - indices.begin(2)))
        case 3 => (
          1 == SimplifyExpression.evalIntegral(indices.end(3) - indices.begin(3)) &&
          (1 == SimplifyExpression.evalIntegral(indices.end(1) - indices.begin(1)) || 1 == SimplifyExpression.evalIntegral(indices.end(2) - indices.begin(2))))
      }
  }
}

case class InitMPIDataType(mpiTypeName : String, field : Field, indexRange : IndexRange) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = InitMPIDataType\n"

  def expand : StatementBlock = {
    if (indexRange.begin(2) == indexRange.end(2)) {
      return StatementBlock(ListBuffer[Statement](s"MPI_Type_vector(" ~
        (indexRange.end(1) - indexRange.begin(1) + 1) ~ ", " ~
        (indexRange.end(0) - indexRange.begin(0) + 1) ~ ", " ~
        (field.layout(0).total) ~
        s", MPI_DOUBLE, &$mpiTypeName)",
        s"MPI_Type_commit(&$mpiTypeName)"))
    } else if (indexRange.begin(1) == indexRange.end(1)) {
      return StatementBlock(ListBuffer[Statement](s"MPI_Type_vector(" ~
        (indexRange.end(2) - indexRange.begin(2) + 1) ~ ", " ~
        (indexRange.end(0) - indexRange.begin(0) + 1) ~ ", " ~
        (field.layout(0).total * field.layout(1).total) ~
        s", MPI_DOUBLE, &$mpiTypeName)",
        s"MPI_Type_commit(&$mpiTypeName)"))
    } else return StatementBlock(ListBuffer[Statement]())
  }
}

case class MPI_Sequential(var body : ListBuffer[Statement]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = MPI_Sequential\n"

  def expand : ForLoopStatement = {
    ForLoopStatement("int curRank = 0", "curRank < mpiSize", "++curRank", ListBuffer[Statement](
      MPI_Barrier(),
      new ConditionStatement("mpiRank == curRank", body)))
  }
}
