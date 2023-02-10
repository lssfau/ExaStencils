//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.parallelization.api.mpi

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.datastructures._
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.layoutTransformation.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.prettyprinting.PpStream

/// MPI_DataType

object MPI_DataType {
  def shouldBeUsed(field : IR_FieldLike, indexRange : IR_ExpressionIndexRange, condition : Option[IR_Expression]) : Boolean = {
    // TODO: extend range of supported cases
    // honor user-give knowledge parameters
    if (!Knowledge.mpi_useCustomDatatypes || Knowledge.data_genVariableFieldSizes)
      return false

    // skip communication steps with conditions for now
    if (condition.isDefined)
      return false

    // skip vector and matrix fields for now
    if (field.layout.numDimsData > field.layout.numDimsGrid)
      return false

    // skip fields targeted by layout transformations
    StateManager.findAll[IR_LayoutTransformStatement]().foreach {
      case _ : IR_ExternalFieldAlias => // nothing to do

      case trafo : IR_GenericTransform =>
        if (trafo.fields.exists(f => f._1 == field.name && f._2 == field.level))
          return false

      case trafo : IR_FieldConcatenation =>
        if (trafo.fieldsToMerge.contains(field.name) && trafo.levels.contains(field.level))
          return false
    }

    // count the number of dimensions with more than one entry - size in the zero dimension is irrelevant
    val numNonDummyDims = (1 until indexRange.length).map(dim =>
      if (IR_SimplifyExpression.evalIntegral(indexRange.end(dim) - indexRange.begin(dim)) > 1) 1 else 0).sum

    // avoid nested data types for now
    numNonDummyDims <= 1
  }

  // filter which mpi datatype send and receive buffers should have based on the field datatype
  // currently implements only the special case of field<complex>, which leads to use of MPI_CXX_DOUBLE_COMPLEX
  // (complex can be mapped to a built-in MPI datatype)
  // (matrices are mapped to buffers of IR_RealDatatype)
  def determineInnerMPIDatatype(field : IR_FieldLike) : IR_Datatype = {
    field.layout.datatype match {
      case cd : IR_ComplexDatatype => IR_ComplexDatatype(IR_RealDatatype)
      case _ => IR_RealDatatype
    }
  }
}

case class MPI_DataType(var field : IR_FieldLike, var indexRange : IR_ExpressionIndexRange, var condition : Option[IR_Expression]) extends IR_Datatype {
  override def prettyprint(out : PpStream) : Unit = out << generateName
  override def prettyprint_mpi : String = generateName

  override def dimensionality : Int = ???
  override def getSizeArray : Array[Int] = ???
  override def resolveBaseDatatype : IR_Datatype = field.resolveBaseDatatype
  override def resolveDeclType : IR_Datatype = this
  override def resolveDeclPostscript : String = ""
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize : Int = ???

  if (!MPI_DataType.shouldBeUsed(field, indexRange, condition))
    Logger.warn(s"Trying to setup an MPI data type for unsupported index range ${ indexRange.print }")

  // compute inside an separate scope to prevent the additional variables (done and *Exp) to be visible to transformations later on
  private val (blockLength, blockCount, stride) : (Long, Long, Long) = {

    // determine data type parameters
    val blockLengthExp : IR_Expression = indexRange.end(0) - indexRange.begin(0)
    var blockCountExp : IR_Expression = 1
    var strideExp : IR_Expression = field.layout(0).total

    var done = false
    for (dim <- 1 until indexRange.length; if !done) {
      if (1 == IR_SimplifyExpression.evalIntegral(indexRange.end(dim) - indexRange.begin(dim))) {
        strideExp *= field.layout.defIdxById("TOT", dim)
      } else {
        blockCountExp = indexRange.end(dim) - indexRange.begin(dim)
        done = true // break
      }
    }

    (IR_SimplifyExpression.evalIntegral(blockLengthExp),
      IR_SimplifyExpression.evalIntegral(blockCountExp),
      IR_SimplifyExpression.evalIntegral(strideExp))
  }

  def generateName : String = s"mpiDatatype_${ blockCount }_${ blockLength }_${ stride }"
  def mpiTypeNameArg : IR_Expression = IR_AddressOf(generateName)

  def generateDecl : IR_VariableDeclaration = {
    IR_VariableDeclaration("MPI_Datatype", generateName)
  }

  def generateCtor : ListBuffer[IR_Statement] = {
    val scalarDatatype = field.resolveBaseDatatype.prettyprint_mpi

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
    case dt : MPI_DataType =>
      datatypes(dt.generateName) = dt
      dt
  })

  this += new Transformation("Adding declaration and init code", {
    case globals : IR_GlobalCollection                       =>
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
