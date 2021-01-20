package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_AddressOf
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_InitializerList
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnknownDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.globals.ir.IR_GlobalCollection

/// MPI_View
object MPI_View {
  private val localViews : ListBuffer[MPI_View] = ListBuffer()
  private val globalViews : ListBuffer[MPI_View] = ListBuffer()
  private val lookupTableLocal : ListBuffer[Int] = ListBuffer()
  private val lookupTableGlobal : ListBuffer[Int] = ListBuffer()
  def addView(view : MPI_View) : Boolean = {
    var ret = true
    if (!view.isLocal) {
      var lookup = globalViews.indexOf(view)
      if (lookup != -1) {
        ret = false
      } else {
        globalViews.append(view)
        lookup = globalViews.length-1
      }
      lookupTableGlobal.append(lookup)
    } else {
      var lookup = localViews.indexOf(view)
      if (lookup != -1) {
        ret = false
      } else {
        localViews.append(view)
        lookup = localViews.length-1
      }
      lookupTableLocal.append(lookup)
    }

    // add to globals if a new view instance is required, otherwise reuse already created instances
    if (ret) {
      val globalCollection = IR_GlobalCollection.get
      globalCollection.variables += view.declaration
      globalCollection.initGlobals match {
        case f : IR_Function => f.body += view.initDatatype()
      }
      globalCollection.destroyGlobals match {
        case f : IR_Function => f.body += view.freeDatatype()
      }
    }

    ret
  }
  def getView(idx : Int, global : Boolean) : MPI_View = if (global) globalViews(lookupTableGlobal(idx)) else localViews(lookupTableLocal(idx))
  def resetViews() : Unit = {
    localViews.clear()
    globalViews.clear()
    lookupTableLocal.clear()
    lookupTableGlobal.clear()
  }
}

/// IR_MPI_View
// describes the data layout in memory and/or in file via MPI derived datatypes

case class MPI_View(
    var totalDims : IR_VariableAccess,
    var count : IR_VariableAccess,
    var start : IR_VariableAccess,
    var numDims : Int,
    var createViewPerFragment : Boolean,
    var isLocal : Boolean,
    var buffer : IR_DataBuffer,
    var name : String) {

  // mpi specific constants/datatypes
  val rowMajor = IR_VariableAccess("MPI_ORDER_C", IR_UnknownDatatype)
  val baseDatatypeMPI = IR_VariableAccess(buffer.datatype.resolveBaseDatatype.prettyprint_mpi, IR_UnknownDatatype)
  val MPI_Datatype = IR_SpecialDatatype("MPI_Datatype")
  val MPI_DATATYPE_NULL = IR_VariableAccess("MPI_DATATYPE_NULL", IR_UnknownDatatype)

  // decl
  lazy val declName : String = IR_FileAccess.declareVariable(name)
  lazy val declDatatype : IR_Datatype = if (createViewPerFragment) IR_ArrayDatatype(MPI_Datatype, Knowledge.domain_numFragmentsPerBlock) else MPI_Datatype
  lazy val declaration : IR_VariableDeclaration = IR_VariableDeclaration(declDatatype, declName)

  // override autogen. equals/hashcode methods to detect identical views
  override def equals(other : Any) : Boolean = {
    other match {
      case view : MPI_View => view.canEqual(this) &&
        view.name == this.name &&
        view.totalDims.name == this.totalDims.name &&
        view.count.name == this.count.name &&
        view.start.name == this.start.name &&
        view.isLocal == this.isLocal &&
        view.createViewPerFragment == this.createViewPerFragment &&
        view.numDims == this.numDims
      case _               => false
    }
  }
  override def hashCode() : Int = {
    (name + totalDims.name + count.name + start.name).hashCode + (42 * numDims * (if (isLocal) 31 else 0))
  }

  // views are currently only defined per fragment (array access) or per block (variable access)
  lazy val getAccess : IR_Access = if (createViewPerFragment) {
    IR_ArrayAccess(IR_VariableAccess(declaration), IR_LoopOverFragments.defIt)
  } else {
    IR_VariableAccess(declaration)
  }

  def freeDatatype(access : IR_Access = getAccess) : IR_Statement = {
    val free = IR_IfCondition(access Neq MPI_DATATYPE_NULL, IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_free"), IR_AddressOf(access)))
    if (createViewPerFragment) IR_LoopOverFragments(free) else free
  }

  def initDatatype(access : IR_Access = getAccess) : IR_Statement = {
    val setDefValue = IR_Assignment(access, MPI_DATATYPE_NULL)
    if (createViewPerFragment) IR_LoopOverFragments(setDefValue) else setDefValue
  }

  def commitDatatype(access : IR_Access = getAccess) : IR_Statement =  IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_commit"), IR_AddressOf(access))

  def createDatatype : IR_Statement = {
    if (isLocal && buffer.accessPattern.accessIndices.isDefined) {
      // an access pattern was defined where certain indices are selected per cell
      // currently only used for nodal fields in SWE (e.g node positions & bath with 6 values per grid cell) to describe in-memory accesses
      val accessPattern = buffer.accessPattern
      val indices = accessPattern.accessIndices.get
      val indexedDatatype = IR_VariableAccess("indexedSelection", MPI_Datatype)
      val numBlocks = indices.length // number of blocks
      val displacements = IR_VariableAccess(IR_FileAccess.declareVariable("dispIndexedAcc"), IR_ArrayDatatype(IR_IntegerDatatype, numBlocks))
      val blocklength = 1 // always access 1 element per block
      val idxRangeTotal = IR_ExpressionIndexRange(buffer.zeroIndex.toExpressionIndex, IR_ExpressionIndex(buffer.totalDimsLocal : _*))

      val stmts : ListBuffer[IR_Statement] = ListBuffer()

      // create "indexed_block" datatype for a grid cell
      stmts += IR_VariableDeclaration(indexedDatatype, MPI_DATATYPE_NULL)
      stmts += IR_VariableDeclaration(displacements, IR_InitializerList( // displacement indices for indexed selection beginning at first inner point
        indices.map(accIdx => idxRangeTotal.linearizeIndex(buffer.referenceOffset + accIdx))))
      stmts += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_create_indexed_block"), numBlocks, blocklength, displacements, baseDatatypeMPI, IR_AddressOf(indexedDatatype))
      stmts += commitDatatype(indexedDatatype)

      // get bytesize of base datatype to compute byte offsets
      val sizeBaseDatatype = IR_VariableAccess("sizeBaseDt", IR_IntegerDatatype)
      stmts += IR_VariableDeclaration(sizeBaseDatatype)
      stmts += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_size"), baseDatatypeMPI, IR_AddressOf(sizeBaseDatatype))

      // created nested "hvectors" to construct the access pattern for the whole grid
      val cellCount = buffer.innerDimsPerFrag.map(_ - 1) // node -> cell
      val stride = buffer.imap.map(_ * sizeBaseDatatype) // strides for the next cell in x/y/z/... direction (in bytes)
      val numDimsGrid = Knowledge.dimensionality
      var nestedDatatypes = ListBuffer(indexedDatatype)
      for (d <- 0 until numDimsGrid) {
        val oldDatatype = nestedDatatypes.last
        val hvector = IR_VariableAccess("hvector_dim" + d, MPI_Datatype)
        stmts += IR_VariableDeclaration(hvector, MPI_DATATYPE_NULL)
        // create new datatype from old and free old afterwards
        stmts += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_hvector"), cellCount(d), blocklength, stride(d), oldDatatype, IR_AddressOf(hvector))
        if (d != numDimsGrid - 1) stmts += commitDatatype(hvector)
        stmts += freeDatatype(oldDatatype)

        nestedDatatypes += hvector // use as base for next dimension
      }

      IR_IfCondition(getAccess EqEq MPI_DATATYPE_NULL, stmts)
    } else {
      // access pattern describes the selection of a subarray within a local/global array of data
      val create = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_create_subarray"), numDims, totalDims, count, start, rowMajor, baseDatatypeMPI, IR_AddressOf(getAccess))

      IR_IfCondition(getAccess EqEq MPI_DATATYPE_NULL, ListBuffer[IR_Statement](create, commitDatatype()))
    }
  }
}
