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

package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.domain.ir._
import exastencils.field.ir.IR_FieldCollection
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.parallelization.api.mpi._
import exastencils.util.ir._

case class IR_ReadStations() extends IR_FuturePlainFunction {
  override var name = "readStations"
  override def prettyprint_decl() = prettyprint

  def fileName = IR_VariableAccess("fileName", IR_StringDatatype)

  val nStations = IR_VariableAccess("nStations", IR_IntegerDatatype)

  def loopOverNumFragments(body : ListBuffer[IR_Statement]) = {
    def fragmentIdx = IR_LoopOverFragments.defIt

    IR_ForLoop(
      IR_VariableDeclaration(fragmentIdx, 0),
      IR_Lower(fragmentIdx, IR_IV_Nfragments()),
      IR_PreIncrement(fragmentIdx),
      body
    )
  }

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
    val stationX = IR_VariableAccess("x", IR_RealDatatype)
    val stationY = IR_VariableAccess("y", IR_RealDatatype)
    body += IR_VariableDeclaration(nStations, 0)
    body += IR_VariableDeclaration(stationX)
    body += IR_VariableDeclaration(stationY)

    body += IR_WhileLoop(
      IR_FunctionCall(IR_ReadLineFromFile.name, file, iss),
      ListBuffer[IR_Statement](
        IR_Read(iss, stationX, stationY),
        IR_Assignment(IR_IV_Stations(nStations, 0), stationX),
        IR_Assignment(IR_IV_Stations(nStations, 1), stationY),
        IR_PreIncrement(nStations)
      )
    )
  }

  def findStationsInDomain() = {
    var body = ListBuffer[IR_Statement]()

    def numDims = Knowledge.dimensionality

    //  v3 -- v2
    //  |     |
    //  v0 -- v1
    val vPos = IR_VariableAccess("vPos", IR_ArrayDatatype(IR_RealDatatype, 8))
    body += IR_VariableDeclaration(vPos)

    def linVertArray(vid : Int, dim : Int) = IR_ArrayAccess(vPos, 2 * vid + dim)

    def nodePosition(dim : Int, offset : IR_ExpressionIndex = IR_ExpressionIndex(0, 0)) = IR_VF_NodePositionPerDim(Knowledge.maxLevel, IR_DomainCollection.objects.head, dim).resolve(IR_LoopOverDimensions.defIt(numDims) + offset)

    var fragStmts = ListBuffer[IR_Statement]()
    fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(0, i), nodePosition(i, IR_ExpressionIndex(0, 0))) }
    fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(1, i), nodePosition(i, IR_ExpressionIndex(1, 0))) }
    fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(2, i), nodePosition(i, IR_ExpressionIndex(1, 1))) }
    fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(3, i), nodePosition(i, IR_ExpressionIndex(0, 1))) }

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

    val bath = IR_FieldCollection.getByIdentifier("bath", Knowledge.maxLevel).get

    def resolveIndex(indexId : String, dim : Int) = bath.layout.idxById(indexId, dim)

    val start = IR_ExpressionIndex((0 until numDims).toArray.map { i => 0 })
    val end = IR_ExpressionIndex((0 until numDims).toArray.map { i => resolveIndex("DRE", i) - 1 - resolveIndex("DLB", i) : IR_Expression })

    body += loopOverNumFragments(ListBuffer[IR_Statement](
      IR_LoopOverDimensions(numDims, IR_ExpressionIndexRange(start, end), fragStmts)
    ))
  }

  override def generateFct() = {
    var body = ListBuffer[IR_Statement]()

    if (!Settings.additionalIncludes.contains("iomanip"))
      Settings.additionalIncludes += "iomanip"

    // broadcast stations for every mpi thread besides rank 0
    if (Knowledge.mpi_enabled) {
      body += IR_IfCondition(IR_Neq(MPI_IV_MpiRank, IR_IntegerConstant(0)),
        bcastStations(),
        readStationsFromFile() ++ bcastStations()
      )
    } else {
      body ++= readStationsFromFile()
    }

    // find station triangles
    body += IR_Comment("Find triangles of stations")

    body ++= findStationsInDomain()

    val iter = IR_VariableAccess("i", IR_IntegerDatatype)
    val localStationCheck = IR_VariableAccess("localStationCheck", IR_ArrayDatatype(IR_IntegerDatatype, Knowledge.swe_stationsMax))
    val globalStationCheck = IR_VariableAccess("globalStationCheck", IR_ArrayDatatype(IR_IntegerDatatype, Knowledge.swe_stationsMax))
    body += IR_VariableDeclaration(localStationCheck)
    body += IR_ForLoop(IR_VariableDeclaration(iter, 0), IR_Lower(iter, Knowledge.swe_stationsMax), IR_PreIncrement(iter), ListBuffer[IR_Statement](
      IR_IfCondition(IR_Neq(IR_IV_StationsFragment(iter), IR_IV_StationsFragment(0).resolveDefValue().get),
        IR_Assignment(IR_ArrayAccess(localStationCheck, iter), 1),
        IR_Assignment(IR_ArrayAccess(localStationCheck, iter), 0))
    ))
    body += IR_VariableDeclaration(globalStationCheck)
    if (Knowledge.mpi_enabled && Knowledge.mpi_numThreads > 1)
      body += MPI_Reduce(0, IR_AddressOf(IR_ArrayAccess(localStationCheck, 0)), IR_AddressOf(IR_ArrayAccess(globalStationCheck, 0)), IR_IV_StationsFragment(0).datatype.resolveBaseDatatype, Knowledge.swe_stationsMax, "+")
    else
      body += IR_ForLoop(IR_VariableDeclaration(iter, 0), IR_Lower(iter, Knowledge.swe_stationsMax), IR_PreIncrement(iter), ListBuffer[IR_Statement](
        IR_Assignment(IR_ArrayAccess(globalStationCheck, iter), IR_ArrayAccess(localStationCheck, iter))
      ))

    body += IR_IfCondition(IR_EqEq(MPI_IV_MpiRank, 0), IR_ForLoop(IR_VariableDeclaration(iter, 0), IR_Lower(iter, Knowledge.swe_stationsMax), IR_PreIncrement(iter), ListBuffer[IR_Statement](
      IR_IfCondition(IR_EqEq(IR_IV_Stations(iter, 0), IR_IV_Stations(0, 0).resolveDefValue().get), IR_Break()),
      IR_IfCondition(IR_EqEq(IR_ArrayAccess(globalStationCheck, iter), 0), ListBuffer[IR_Statement](
        IR_RawPrint(ListBuffer[IR_Expression](
          IR_StringConstant("Station with id"), iter,
          IR_StringConstant("at position x ="), IR_IV_Stations(iter, 0), IR_StringConstant(", y ="), IR_IV_Stations(iter, 1),
          IR_StringConstant("not found in domain. Station will be ignored.")
        ))
      )),
      IR_IfCondition(IR_Greater(IR_ArrayAccess(globalStationCheck, iter), 1), ListBuffer[IR_Statement](
        IR_RawPrint(ListBuffer[IR_Expression](
          IR_StringConstant("Station with id"), iter,
          IR_StringConstant("at position x ="), IR_IV_Stations(iter, 0), IR_StringConstant(", y ="), IR_IV_Stations(iter, 1),
          IR_StringConstant("was found several times! Output might be unusable!")
        ))
      ))
    )))

    IR_PlainFunction(name, IR_UnitDatatype, IR_FunctionArgument(fileName), body)
  }
}
