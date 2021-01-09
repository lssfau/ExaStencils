package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_AddressOf
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnknownDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.core.Duplicate
import exastencils.logger.Logger

/// MPI_View
object IR_MPI_View {
  // TODO simplify?
  private var localViews : ListBuffer[IR_MPI_View] = ListBuffer()
  private var globalViews : ListBuffer[IR_MPI_View] = ListBuffer()
  private var lookupTableLocal : ListBuffer[Int] = ListBuffer()
  private var lookupTableGlobal : ListBuffer[Int] = ListBuffer()
  def addView(idx : Int, global : Boolean, view : IR_MPI_View) : Boolean = {
    var ret = true
    if (global) {
      var lookup = globalViews.indexOf(view)
      if (lookup != -1) {
        ret = false
      } else {
        globalViews.append(view)
        lookup = globalViews.length-1
      }
      lookupTableGlobal += lookup
    } else {
      var lookup = localViews.indexOf(view)
      if (lookup != -1) {
        ret = false
      } else {
        localViews.append(view)
        lookup = localViews.length-1
      }
      lookupTableLocal += lookup
    }
    ret
  }
  def getView(idx : Int, global : Boolean) : IR_MPI_View = if (global) globalViews(lookupTableGlobal(idx)) else localViews(lookupTableLocal(idx))
  def getAllViews : ListBuffer[IR_MPI_View] = Duplicate(globalViews) ++ Duplicate(localViews)
  def resetViews() : Unit = {
    localViews = ListBuffer()
    globalViews = ListBuffer()
    lookupTableLocal = ListBuffer()
    lookupTableGlobal = ListBuffer()
  }
}

case class IR_MPI_View(
    var totalDims : IR_VariableAccess,
    var count : IR_VariableAccess,
    var start : IR_VariableAccess,
    var numDims : Int,
    var domainIdx : Int,
    var mpiBaseDatatype : IR_VariableAccess,
    var datatype : IR_Datatype,
    var name : String) {

  val rowMajor = IR_VariableAccess("MPI_ORDER_C", IR_UnknownDatatype)
  val MPI_Datatype = IR_SpecialDatatype("MPI_Datatype")

  lazy val declName : String = IR_FileAccess.declareVariable(name)
  lazy val declaration = IR_VariableDeclaration(datatype, declName)

  // views are currently only defined per fragment (array access) or per block (variable access)
  lazy val getAccess : IR_Access = datatype match {
    case IR_ArrayDatatype(MPI_Datatype, _) =>
      IR_ArrayAccess(IR_VariableAccess(declaration), IR_LoopOverFragments.defIt)
    case MPI_Datatype =>
      IR_VariableAccess(declaration)
    case _ =>
      Logger.error("Wrong datatype passed to MPI_View: " + datatype.prettyprint)
  }

  def createDatatype : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_create_subarray"),
    numDims, totalDims, count, start, rowMajor, mpiBaseDatatype, IR_AddressOf(getAccess))
  def commitDatatype : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_commit"), IR_AddressOf(getAccess))
  def freeDatatype : IR_Statement = datatype match {
    case IR_ArrayDatatype(MPI_Datatype, _) => IR_LoopOverFragments(IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_free"), IR_AddressOf(getAccess)))
    case MPI_Datatype => IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_free"), IR_AddressOf(getAccess))
    case _ =>
      Logger.error("Wrong datatype passed to MPI_View: " + datatype.prettyprint)
  }

  // TODO: indexed selection for blockstructured meshes: "MPI_Type_indexed"
}
