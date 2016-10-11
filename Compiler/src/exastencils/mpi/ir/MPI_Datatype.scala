package exastencils.mpi.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.globals.Globals
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.SimplifyExpression

/// MPI_DataType

object MPI_DataType {
  def shouldBeUsed(indexRange : IR_ExpressionIndexRange, condition : Option[IR_Expression]) : Boolean = {
    // honor user-give knowledge parameters
    if (!Knowledge.mpi_useCustomDatatypes || Knowledge.data_genVariableFieldSizes)
      return false

    // skip communication steps with conditions for now
    if (condition.isDefined)
      return false

    // count the number of dimensions with more than one entry - size in the zero dimension is irrelevant
    val numNonDummyDims = (1 until indexRange.length).map(dim =>
      if (SimplifyExpression.evalIntegral(indexRange.end(dim) - indexRange.begin(dim)) > 1) 1 else 0).sum

    // avoid nested data types for now
    return numNonDummyDims <= 1
  }
}

case class MPI_DataType(var field : IR_FieldSelection, var indexRange : IR_ExpressionIndexRange, var condition : Option[IR_Expression]) extends IR_Datatype {
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
  for (dim <- 1 until indexRange.length; if !done) {
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

/// MPI_AddDatatypes

object MPI_AddDatatypeSetup extends DefaultStrategy("Add declaration, initialization and de-initialization for MPI datatypes") {
  var datatypes : HashMap[String, MPI_DataType] = HashMap()

  override def apply(node : Option[Node] = None) = {
    datatypes.clear
    super.apply(node)
  }

  this += new Transformation("Looking for data types", {
    case dt : MPI_DataType => {
      datatypes(dt.generateName) = dt
      dt
    }
  })

  this += new Transformation("Adding declaration and init code", {
    case globals : Globals                                   =>
      for (dt <- datatypes)
        globals.variables += dt._2.generateDecl
      globals
    case func : IR_Function if "initGlobals" == func.name    =>
      for (dt <- datatypes)
        func.body ++= dt._2.generateCtor
      func
    case func : IR_Function if "destroyGlobals" == func.name =>
      for (dt <- datatypes)
        func.body ++= dt._2.generateDtor
      func
  })
}