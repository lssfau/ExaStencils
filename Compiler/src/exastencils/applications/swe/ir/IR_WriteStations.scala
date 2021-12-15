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
import exastencils.domain.ir.IR_DomainCollection
import exastencils.field.ir.IR_FieldAccess
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print

object IR_WriteStations {
  val basename = "writeStations"
  def getNewName(id : Int) = basename + id
}

case class IR_WriteStations(var resolveId : Int, var arguments : ListBuffer[IR_Expression]) extends IR_FuturePlainFunction {
  override var name = IR_WriteStations.getNewName(resolveId)
  override def prettyprint_decl() = prettyprint

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

  override def generateFct() = {
    if (!Settings.additionalIncludes.contains("iomanip"))
      Settings.additionalIncludes += "iomanip"

    if (!Settings.additionalIncludes.contains("set"))
      Settings.additionalIncludes += "set"

    var body = ListBuffer[IR_Statement]()

    val coeffs = arguments.tail

    for (c <- coeffs) {
      if (!c.isInstanceOf[IR_FieldAccess]) Logger.error("The coefficient " + c.toString + " is not a FieldAccess.")
    }

    def numDims = Knowledge.dimensionality

    def coeffsFieldAccess(index : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(numDims), fragIdx : IR_Expression = IR_LoopOverFragments.defIt) = coeffs.map { c =>
      val field = c.asInstanceOf[IR_FieldAccess].field
      IR_FieldAccess(field, 0, fragIdx, index)
    }

    def nodePositions(dim : Int, offset : IR_ExpressionIndex = IR_ExpressionIndex(0, 0)) = IR_VF_NodePositionPerDim(Knowledge.maxLevel, IR_DomainCollection.objects.head, dim).resolve(IR_LoopOverDimensions.defIt(numDims) + offset)

    def coeffsLower(index : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(numDims), fragIdx : IR_Expression = IR_LoopOverFragments.defIt) = coeffsFieldAccess(index, fragIdx).zipWithIndex.collect { case (e, i) if i % 2 == 0 => e }

    def coeffsUpper(index : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(numDims), fragIdx : IR_Expression = IR_LoopOverFragments.defIt) = coeffsFieldAccess(index, fragIdx).zipWithIndex.collect { case (e, i) if i % 2 == 1 => e }

    def fileNames = IR_VariableAccess("fileNames", IR_SpecialDatatype("static std::set<std::string>"))

    body += IR_VariableDeclaration(fileNames)

    val stationId = IR_VariableAccess("stationId", IR_IntegerDatatype)
    val stationStmts = ListBuffer[IR_Statement]()

    val vPos = IR_VariableAccess("vPos", IR_ArrayDatatype(IR_RealDatatype, 8))

    val quantity = IR_VariableAccess("quantity", IR_RealDatatype)

    def linVertArray(vid : Int, dim : Int) = IR_ArrayAccess(vPos, 2 * vid + dim)

    val fragId = IR_LoopOverFragments.defIt
    val i0 = IR_VariableAccess("i0", IR_IntegerDatatype)
    val i1 = IR_VariableAccess("i1", IR_IntegerDatatype)
    val lowerLeftIdx = IR_ExpressionIndex(i0, i1)
    val linIndex = IR_VariableAccess("linInxed", IR_RealDatatype)

    stationStmts += IR_VariableDeclaration(vPos)
    stationStmts += IR_VariableDeclaration(quantity)

    stationStmts += IR_IfCondition(IR_EqEq(IR_IV_StationsFragment(stationId), IR_IV_StationsFragment(stationId).resolveDefValue().get), IR_Break())

    stationStmts += IR_VariableDeclaration(fragId, IR_IV_StationsFragment(stationId))
    stationStmts += IR_VariableDeclaration(i0, IR_IV_StationsId(stationId, 0))
    stationStmts += IR_VariableDeclaration(i1, IR_IV_StationsId(stationId, 1))
    stationStmts ++= (0 until 2).toArray.map(i => IR_Assignment(linVertArray(0, i), nodePositions(i, IR_ExpressionIndex(0, 0))))
    stationStmts ++= (0 until 2).toArray.map(i => IR_Assignment(linVertArray(1, i), nodePositions(i, IR_ExpressionIndex(1, 0))))
    stationStmts ++= (0 until 2).toArray.map(i => IR_Assignment(linVertArray(2, i), nodePositions(i, IR_ExpressionIndex(1, 1))))
    stationStmts ++= (0 until 2).toArray.map(i => IR_Assignment(linVertArray(3, i), nodePositions(i, IR_ExpressionIndex(0, 1))))
    stationStmts += IR_IfCondition(IR_IV_StationsIsLower(stationId),
      IR_Assignment(quantity, IR_FunctionCall(IR_PlainInternalFunctionReference("evalQuantity", IR_UnitDatatype),
        ListBuffer[IR_Expression](
          IR_IV_Stations(stationId, 0), IR_IV_Stations(stationId, 1),
          linVertArray(0, 0), linVertArray(0, 1),
          linVertArray(1, 0), linVertArray(1, 1),
          linVertArray(3, 0), linVertArray(3, 1)
        ) ++ coeffsLower(lowerLeftIdx, fragId))),
      IR_Assignment(quantity, IR_FunctionCall(IR_PlainInternalFunctionReference("evalQuantity", IR_UnitDatatype),
        ListBuffer[IR_Expression](
          IR_IV_Stations(stationId, 0), IR_IV_Stations(stationId, 1),
          linVertArray(2, 0), linVertArray(2, 1),
          linVertArray(3, 0), linVertArray(3, 1),
          linVertArray(1, 0), linVertArray(1, 1)
        ) ++ coeffsUpper(lowerLeftIdx, fragId))))

    val file = IR_VariableAccess("file", IR_SpecialDatatype("std::ofstream"))

    // write quantity to file
    def fileName = IR_VariableAccess("fileName", IR_StringDatatype)

    def fileNameWithId = IR_VariableAccess("fileNameWithId", IR_StringDatatype)

    def strToFind = IR_VariableAccess("strToFind", IR_StringDatatype)

    stationStmts += IR_VariableDeclaration(fileNameWithId, fileName)
    stationStmts += IR_VariableDeclaration(strToFind, IR_StringConstant("$stationId"))
    stationStmts += IR_IfCondition(IR_Neq(IR_MemberFunctionCall(fileNameWithId, "find", strToFind), "std::string::npos"),
      IR_MemberFunctionCall(fileNameWithId, "replace",
        IR_MemberFunctionCall(fileNameWithId, "find", strToFind),
        IR_MemberFunctionCall(strToFind, "length"),
        IR_FunctionCall("std::to_string", stationId)
      ))

    stationStmts += IR_VariableDeclaration(file)

    stationStmts += IR_IfCondition(IR_EqEq(IR_MemberFunctionCall(fileNames, "find", fileNameWithId), IR_MemberFunctionCall(fileNames, "end")), ListBuffer[IR_Statement](
      IR_Comment("reset file"),
      IR_MemberFunctionCall(file, "open", ListBuffer[IR_Expression](fileNameWithId, "std::ios::trunc")),
      IR_Print(file, IR_StringConstant("x = "), IR_IV_Stations(stationId, 0), IR_StringConstant("\\t y = "), IR_IV_Stations(stationId, 1), IR_Print.endl),
      IR_MemberFunctionCall(fileNames, "insert", fileNameWithId)
    ),
      IR_MemberFunctionCall(file, "open", ListBuffer[IR_Expression](fileNameWithId, "std::ios::app"))
    )

    val t = IR_GlobalCollection.get.variables.find(_.name == "t") match {
      case None    => Logger.error("Time variable 't' was not found in writeStations().")
      case Some(v) => IR_VariableAccess(v)
    }

    stationStmts += IR_Assert(IR_MemberFunctionCall(file, "is_open"), ListBuffer("\"Unable to open file \"", fileNameWithId), IR_FunctionCall("exit", 1))
    stationStmts += IR_Print(file, "std::scientific << std::setprecision(10)")
    stationStmts += IR_Print(file, t, IR_StringConstant("\\t"), quantity, IR_Print.endl)
    stationStmts += IR_MemberFunctionCall(file, "close")

    body += IR_ForLoop(IR_VariableDeclaration(stationId, 0), IR_Lower(stationId, Knowledge.swe_stationsMax), IR_PreIncrement(stationId), stationStmts)

    IR_PlainFunction(name, IR_UnitDatatype, IR_FunctionArgument(fileName), body)
  }
}
