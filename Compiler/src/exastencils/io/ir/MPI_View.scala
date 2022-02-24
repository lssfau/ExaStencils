package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.grid.ir.IR_AtNode
import exastencils.logger.Logger

/// MPI_View
object MPI_View {
  val localViews : ListBuffer[MPI_View] = ListBuffer()
  val globalViews : ListBuffer[MPI_View] = ListBuffer()
  private val lookupTableLocal : ListBuffer[Int] = ListBuffer()
  private val lookupTableGlobal : ListBuffer[Int] = ListBuffer()
  def addView(view : MPI_View) : Boolean = {
    var ret = true
    val container = if (!view.isLocal) globalViews else localViews
    val lut = if (!view.isLocal) lookupTableGlobal else lookupTableLocal

    var lookup = container.indexOf(view)
    if (lookup != -1) {
      ret = false
    } else {
      container.append(view)
      lookup = container.length - 1
    }
    lut.append(lookup)

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

  def commitDatatype(access : IR_Access = getAccess) : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_commit"), IR_AddressOf(access))

  def createDatatype : IR_Statement = {
    if (buffer.accessPattern.accessIndices.isDefined) {
      createIndexedBlocks
    } else {
      createSubarray
    }
  }

  // SWE specialized implementation. to be used for (non-reduced) nodal fields (e.g node positions & bath with "6" values per grid cell)
  private def createIndexedBlocks : IR_Statement = {
    // check if datatype is only used for its special use-case
    if (Knowledge.swe_nodalReductionPrint) {
      // make sure this datatype is only used when nodal data reduction is turned off
      Logger.error("MPI View: Wrong derived datatype for \"Knowledge.swe_nodalReductionPrint\"=true. Should be: \"createSubarray\".")
    }
    if (buffer.localization != IR_AtNode && Knowledge.dimensionality != 2) {
      Logger.error("MPI View: \"createIndexedBlocks\" is currently only supported for nodal meshes in SWE applications (2D).")
    }

    val accessPattern = buffer.accessPattern
    val indices = accessPattern.accessIndices.get

    if (isLocal) {
      val cellCountFrag = buffer.innerDimsPerFrag.map(_ - 1) // node -> cell
      val numBlocks = indices.length // number of accesses
      val blocklength = 1 // always access 1 element per block
      val displacements = IR_VariableAccess(IR_FileAccess.declareVariable("dispIndexedAcc"), IR_ArrayDatatype(IR_IntegerDatatype, numBlocks)) // displacements for each access
      val idxRangeTotal = IR_ExpressionIndexRange(buffer.zeroIndex.toExpressionIndex, IR_ExpressionIndex(buffer.totalDimsLocal : _*))
      val indexedDatatype = IR_VariableAccess("indexedSelection", MPI_Datatype)

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
      val stride = buffer.imap.map(_ * sizeBaseDatatype) // strides for the next cell in x/y/z/... direction (in bytes)
      val numDimsGrid = Knowledge.dimensionality
      var nestedDatatypes : ListBuffer[IR_Access] = ListBuffer(indexedDatatype)
      for (d <- 0 until numDimsGrid) {
        val oldDatatype = nestedDatatypes.last
        val hvector = IR_VariableAccess("hvector_dim" + d, MPI_Datatype)
        val newDatatype = if (d != numDimsGrid - 1) {
          stmts += IR_VariableDeclaration(hvector, MPI_DATATYPE_NULL)
          hvector
        } else {
          getAccess
        }
        // create new datatype from old and free old afterwards
        stmts += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_create_hvector"), cellCountFrag(d), blocklength, stride(d), oldDatatype, IR_AddressOf(newDatatype))
        stmts += commitDatatype(newDatatype)
        stmts += freeDatatype(oldDatatype)

        nestedDatatypes += newDatatype // use as base for next dimension
      }

      IR_IfCondition(getAccess EqEq MPI_DATATYPE_NULL, stmts)
    } else {
      // create global subarray with modified dimensions (nodal -> zonal with 6 accesses each)
      val dimsGlobal = accessPattern.transformDataExtents(buffer.globalDimsKJI, buffer.localization, orderKJI = true)
      val countGlobal = IR_IntegerConstant(1) +: dimsGlobal.drop(1) // TotalNumFrags -> 1 Fragment per write
      val fragOffset = IR_IV_FragmentOffset(buffer.domainIdx) + IR_TernaryCondition(IR_IV_IsValidForDomain(buffer.domainIdx), IR_LoopOverFragments.defIt, 0) // dummy: set to "0" for "invalid" frags
      val startGlobal = fragOffset +: dimsGlobal.drop(1).map(_ => IR_IntegerConstant(0)) // start at global "valid" fragment
      val newDims = dimsGlobal.length
      val globalDims = IR_VariableAccess("globalDims", IR_ArrayDatatype(IR_IntegerDatatype, newDims))
      val globalCount = IR_VariableAccess("globalCount", IR_ArrayDatatype(IR_IntegerDatatype, newDims))
      val globalStart = IR_VariableAccess("globalStart", IR_ArrayDatatype(IR_IntegerDatatype, newDims))

      IR_IfCondition(getAccess EqEq MPI_DATATYPE_NULL,
        ListBuffer[IR_Statement](
          // specify modified dims in KJI order
          IR_VariableDeclaration(globalDims, IR_InitializerList(dimsGlobal : _*)),
          IR_VariableDeclaration(globalCount, IR_InitializerList(countGlobal : _*)),
          IR_VariableDeclaration(globalStart, IR_InitializerList(startGlobal : _*)),
          IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_create_subarray"), newDims, globalDims, globalCount, globalStart, rowMajor, baseDatatypeMPI, IR_AddressOf(getAccess)),
          commitDatatype()))
    }
  }

  // access pattern describes the selection of a subarray within a local (global) array of data in memory (in file)
  private def createSubarray : IR_Statement = {
    IR_IfCondition(getAccess EqEq MPI_DATATYPE_NULL,
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_create_subarray"), numDims, totalDims, count, start, rowMajor, baseDatatypeMPI, IR_AddressOf(getAccess)),
        commitDatatype()))
  }

}
