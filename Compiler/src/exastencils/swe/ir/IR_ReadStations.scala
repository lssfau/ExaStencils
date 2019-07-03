package exastencils.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir.IR_ReadLineFromFile
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid.ir.IR_VF_NodePositionAsVec
import exastencils.parallelization.api.mpi.MPI_Bcast
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_ReadStream

case class IR_ReadStations() extends IR_FuturePlainFunction {
  override var name = "readStations"
  override def prettyprint_decl() = prettyprint

  def fileName = IR_VariableAccess("fileName", IR_StringDatatype)

  def bcastStations() = {
    var bcastStmts = ListBuffer[IR_Statement]()

    bcastStmts += MPI_Bcast(IR_AddressOf(IR_IV_Stations(0, 0)), Knowledge.swe_stationsMax, IR_IV_Stations(0, 0).datatype.resolveBaseDatatype, 0)
  }

  def isInTriangle(xEval : IR_Expression, yEval : IR_Expression,
      x1 : IR_Expression, y1 : IR_Expression,
      x2 : IR_Expression, y2 : IR_Expression,
      x3 : IR_Expression, y3 : IR_Expression) = {

    def det(ax : IR_Expression, ay : IR_Expression, bx : IR_Expression, by : IR_Expression) = ax * by - ay * bx

    val det1 = det(x2 - x1, y2 - y1, xEval - x1, yEval - y1)
    val det2 = det(x3 - x2, y3 - y2, xEval - x2, yEval - y2)
    val det3 = det(x1 - x3, y1 - y3, xEval - x3, yEval - y3)

    IR_AndAnd(IR_GreaterEqual(det1, 0), IR_AndAnd(IR_GreaterEqual(det2, 0), IR_GreaterEqual(det3, 0)))
  }

  def readStationsFromFile() = {
    var body = ListBuffer[IR_Statement]()

    // read stations on rank 0
    val file = IR_VariableAccess("file", IR_SpecialDatatype("std::ifstream"))

    body += IR_VariableDeclaration(file)
    body += IR_MemberFunctionCall(file, "open", fileName)
    body += IR_Assert(IR_MemberFunctionCall(file, "is_open"), ListBuffer("\"Unable to open file \"", fileName), IR_FunctionCall("exit", 1))

    val iss = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream"))
    body += IR_VariableDeclaration(iss)
    val stationNumber = IR_VariableAccess("stationNumber", IR_IntegerDatatype)
    val stationX = IR_VariableAccess("x", IR_FloatDatatype)
    val stationY = IR_VariableAccess("y", IR_FloatDatatype)
    body += IR_VariableDeclaration(stationNumber, 0)
    body += IR_VariableDeclaration(stationX)
    body += IR_VariableDeclaration(stationY)

    body += IR_WhileLoop(
      IR_FunctionCall(IR_ReadLineFromFile.name, file, iss),
      ListBuffer[IR_Statement](
        IR_ReadStream(iss, ListBuffer(stationX, stationY)),
        IR_Assignment(IR_IV_Stations(stationNumber, 0), stationX),
        IR_Assignment(IR_IV_Stations(stationNumber, 1), stationY),
        IR_PreIncrement(stationNumber)
      )
    )
  }

  def findStationsInDomain() = {
    var body = ListBuffer[IR_Statement]()

    val field = IR_VF_NodePositionAsVec.find(Knowledge.maxLevel).associatedField

    def fieldSelection = IR_FieldSelection(field, field.level, 0)

    def numDims = field.fieldLayout.numDimsGrid

    def resolveIndex(indexId : String, dim : Int) = field.fieldLayout.idxById(indexId, dim)

    def nodePositions(dim : Int, offset : IR_ExpressionIndex = IR_ExpressionIndex(0, 0)) = {
      val hdIndex = IR_LoopOverDimensions.defIt(numDims) + offset
      hdIndex.indices :+= (dim : IR_Expression)
      hdIndex.indices :+= (0 : IR_Expression) // matrix dt...
      IR_FieldAccess(IR_FieldSelection(IR_VF_NodePositionAsVec.find(field.level).associatedField, field.level, 0), hdIndex)
    }

    //  v3 -- v2
    //  |     |
    //  v0 -- v1
    val vPos = IR_VariableAccess("vPos", IR_ArrayDatatype(IR_DoubleDatatype, 8))
    body += IR_VariableDeclaration(vPos)

    def linVertArray(vid : Int, dim : Int) = IR_ArrayAccess(vPos, 2 * vid + dim)

    var fragStmts = ListBuffer[IR_Statement]()
    fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(0, i), nodePositions(i, IR_ExpressionIndex(0, 0))) }
    fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(1, i), nodePositions(i, IR_ExpressionIndex(1, 0))) }
    fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(2, i), nodePositions(i, IR_ExpressionIndex(1, 1))) }
    fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(3, i), nodePositions(i, IR_ExpressionIndex(0, 1))) }

    val stationStmts = ListBuffer[IR_Statement]()
    val stationId = IR_VariableAccess("stationId", IR_IntegerDatatype)

    stationStmts += IR_IfCondition(IR_EqEq(IR_IV_Stations(stationId, 0), IR_IV_Stations(stationId, 0).resolveDefValue().get), IR_Break())

    val stationX = IR_IV_Stations(stationId, 0)
    val stationY = IR_IV_Stations(stationId, 1)

    // lower (0,1,3)
    stationStmts += IR_Comment("lower triangle (0,1,3)")
    stationStmts += IR_IfCondition(isInTriangle(stationX, stationY,
      linVertArray(0, 0), linVertArray(0, 1),
      linVertArray(1, 0), linVertArray(1, 1),
      linVertArray(3, 0), linVertArray(3, 1)), ListBuffer[IR_Statement](
      IR_Comment("Add to lower triangle array"),
      IR_Assignment(IR_IV_StationsFragment(stationId), IR_LoopOverFragments.defIt),
      IR_Assignment(IR_IV_StationsId(stationId, 0), IR_LoopOverDimensions.defIt(numDims)(0)),
      IR_Assignment(IR_IV_StationsId(stationId, 1), IR_LoopOverDimensions.defIt(numDims)(1)),
      IR_Assignment(IR_IV_StationsIsLower(stationId), true)
    ))

    // upper (2,3,1)
    stationStmts += IR_Comment("upper triangle (2,3,1)")
    stationStmts += IR_IfCondition(isInTriangle(stationX, stationY,
      linVertArray(2, 0), linVertArray(2, 1),
      linVertArray(3, 0), linVertArray(3, 1),
      linVertArray(1, 0), linVertArray(1, 1)), ListBuffer[IR_Statement](
      IR_Comment("Add to upper triangle array"),
      IR_Assignment(IR_IV_StationsFragment(stationId), IR_LoopOverFragments.defIt),
      IR_Assignment(IR_IV_StationsId(stationId, 0), IR_LoopOverDimensions.defIt(numDims)(0)),
      IR_Assignment(IR_IV_StationsId(stationId, 1), IR_LoopOverDimensions.defIt(numDims)(1)),
      IR_Assignment(IR_IV_StationsIsLower(stationId), false)
    ))

    fragStmts += IR_ForLoop(IR_VariableDeclaration(stationId, 0), IR_Lower(stationId, Knowledge.swe_stationsMax), IR_PreIncrement(stationId), stationStmts)

    val start = IR_ExpressionIndex((0 until numDims).toArray.map { i => 0 })
    val end = IR_ExpressionIndex((0 until numDims).toArray.map { i => resolveIndex("DRE", i) - 1 - resolveIndex("DLB", i) : IR_Expression })

    body += IR_LoopOverFragments(IR_LoopOverDimensions(numDims, IR_ExpressionIndexRange(start, end), fragStmts))
  }

  override def generateFct() = {
    var body = ListBuffer[IR_Statement]()

    if (!Settings.additionalIncludes.contains("iomanip"))
      Settings.additionalIncludes += "iomanip"

    // broadcast stations for every mpi thread besides rank 0
    if (Knowledge.mpi_enabled) {
      body += IR_IfCondition(IR_Neq(MPI_IV_MpiRank, IR_IntegerConstant(0)),
        bcastStations() ++ ListBuffer[IR_Statement](IR_Return())
      )
    }

    body ++= readStationsFromFile()

    // mpi broadcast
    if (Knowledge.mpi_enabled) {
      // broadcast rank 0
      body ++= bcastStations()
    }


    // find station triangles
    body += IR_Comment("Find triangles of stations")

    body ++= findStationsInDomain()

    body += IR_Comment("A check if station is part of several triangles is missing!")

    IR_PlainFunction(name, IR_UnitDatatype, IR_FunctionArgument(fileName), body)
  }
}
