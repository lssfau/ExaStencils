package exastencils.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir
import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_FieldCollection
import exastencils.field.ir.IR_LinearizedFieldAccess
import exastencils.grid.ir.IR_VF_NodePositionAsVec
import exastencils.logger.Logger
import exastencils.util.ir.IR_BuildString
import exastencils.util.ir.IR_Print

case class IR_WriteStations(var arguments : ListBuffer[IR_Expression]) extends IR_FuturePlainFunction {
  //TODO change arguments, cut out filename. Use station name as file name. Add ".txt" or something similar.
  //TODO add in ReadStations an optional output folder
  override var name = "writeStations"
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

    var body = ListBuffer[IR_Statement]()

    // get all fields which are in arguments
    //val coeffNames = arguments.tail
    //
    //val coeffs = coeffNames.map(c => {
    //  IR_FieldCollection.getByIdentifier(c.asInstanceOf[IR_StringConstant].value, Knowledge.maxLevel).get
    //})

    val coeffs = arguments.tail

    val coeffsField = coeffs.map(c => {
      IR_FieldCollection.getByIdentifier(c.asInstanceOf[IR_StringConstant].value, Knowledge.maxLevel) match {
        case None    => Logger.error("The coefficient " + c.asInstanceOf[IR_StringConstant] + "is not an existing field.")
        case Some(f) => f
      }
    })

    //for (c <- coeffs) {
    //  if (!c.isInstanceOf[IR_FieldAccess]) Logger.error("The coefficient " + c.toString + "Is not a FieldAccess.")
    //}

    //val coeffsFieldAccess = coeffs.map(_.asInstanceOf[IR_FieldAccess])

    // find quad

    //def nodePositions(dim : Int) = IR_VF_NodePositionPerDim(field.level, field.domain, dim).resolve(IR_LoopOverDimensions.defIt(numDims))
    val field = IR_VF_NodePositionAsVec.find(Knowledge.maxLevel).associatedField

    def fieldSelection = IR_FieldSelection(field, field.level, 0)

    def numDims = field.fieldLayout.numDimsGrid

    def resolveIndex(indexId : String, dim : Int) = field.fieldLayout.idxById(indexId, dim)

    def nodePositions(dim : Int, index : IR_ExpressionIndex, offset : IR_ExpressionIndex = IR_ExpressionIndex(0, 0), fragIdx : IR_Expression = IR_LoopOverFragments.defIt) = {
      val hdIndex = index + offset
      hdIndex.indices :+= (dim : IR_Expression)
      hdIndex.indices :+= (0 : IR_Expression) // matrix dt...
      IR_FieldAccess(IR_FieldSelection(IR_VF_NodePositionAsVec.find(field.level).associatedField, field.level, 0, fragIdx), hdIndex)
    }

    def coeffsFieldAccess(index : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(numDims), fragIdx : IR_Expression = IR_LoopOverFragments.defIt) = coeffsField.map { case f : IR_Field => IR_FieldAccess(IR_FieldSelection(f, f.level, 0, fragIdx), index) }

    def coeffsLower(index : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(numDims), fragIdx : IR_Expression = IR_LoopOverFragments.defIt) = coeffsFieldAccess(index, fragIdx).zipWithIndex.collect { case (e, i) if i % 2 == 0 => e }

    def coeffsUpper(index : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(numDims), fragIdx : IR_Expression = IR_LoopOverFragments.defIt) = coeffsFieldAccess(index, fragIdx).zipWithIndex.collect { case (e, i) if i % 2 == 1 => e }

    //val stationX = IR_IV_Stations(0, 0)
    //val stationY = IR_IV_Stations(0, 1)
    ////  v3 -- v2
    ////  |     |
    ////  v0 -- v1
    //val vPos = IR_VariableAccess("vPos", IR_ArrayDatatype(IR_DoubleDatatype, 8))
    //body += IR_VariableDeclaration(vPos)
    //val quantity = IR_VariableAccess("quantity", IR_DoubleDatatype)
    //body += IR_VariableDeclaration(quantity, 9999)
    //val hasStation = IR_VariableAccess("hasStation", IR_BooleanDatatype)
    //body += IR_VariableDeclaration(hasStation, false)
    //
    //def linVertArray(vid : Int, dim : Int) = IR_ArrayAccess(vPos, 2 * vid + dim)
    //
    //var fragStmts = ListBuffer[IR_Statement]()
    //fragStmts += IR_IfCondition(hasStation, IR_Break())
    //fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(0, i), nodePositions(i, IR_ExpressionIndex(0, 0))) }
    //fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(1, i), nodePositions(i, IR_ExpressionIndex(1, 0))) }
    //fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(2, i), nodePositions(i, IR_ExpressionIndex(0, 1))) }
    //fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(3, i), nodePositions(i, IR_ExpressionIndex(1, 1))) }
    //
    //// find triangle (lower or upper)
    //// lower (0,1,3)
    //fragStmts += IR_Comment("lower triangle (0,1,3)")
    //fragStmts += IR_IfCondition(isInTriangle(stationX, stationY,
    //  linVertArray(0, 0), linVertArray(0, 1),
    //  linVertArray(1, 0), linVertArray(1, 1),
    //  linVertArray(3, 0), linVertArray(3, 1)), ListBuffer[IR_Statement](
    //  // eval quantity
    //  IR_Assignment(hasStation, true),
    //  IR_Assignment(quantity, IR_FunctionCall(IR_PlainInternalFunctionReference("evalQuantity", IR_UnitDatatype),
    //    ListBuffer[IR_Expression](
    //      stationX, stationY,
    //      linVertArray(0, 0), linVertArray(0, 1),
    //      linVertArray(1, 0), linVertArray(1, 1),
    //      linVertArray(3, 0), linVertArray(3, 1)
    //    ) ++ coeffsLower))
    //))
    //
    //// upper (2,3,1)
    //fragStmts += IR_Comment("upper triangle (2,3,1)")
    //fragStmts += IR_IfCondition(isInTriangle(stationX, stationY,
    //  linVertArray(2, 0), linVertArray(2, 1),
    //  linVertArray(3, 0), linVertArray(3, 1),
    //  linVertArray(1, 0), linVertArray(1, 1)), ListBuffer[IR_Statement](
    //  // eval quantity
    //  IR_Assignment(hasStation, true),
    //  IR_Assignment(quantity, IR_FunctionCall(IR_PlainInternalFunctionReference("evalQuantity", IR_UnitDatatype),
    //    ListBuffer[IR_Expression](
    //      stationX, stationY,
    //      linVertArray(2, 0), linVertArray(2, 1),
    //      linVertArray(3, 0), linVertArray(3, 1),
    //      linVertArray(1, 0), linVertArray(1, 1)
    //    ) ++ coeffsUpper))
    //))
    //
    //val start = IR_ExpressionIndex((0 until numDims).toArray.map { i => resolveIndex("DLB", i) })
    //val end = IR_ExpressionIndex((0 until numDims).toArray.map { i => resolveIndex("DRE", i) - 1 : IR_Expression })
    //
    //body += IR_LoopOverFragments(IR_LoopOverDimensions(numDims, IR_ExpressionIndexRange(start, end), fragStmts))
    //
    //body += IR_IfCondition(IR_Negation(hasStation), IR_Return())

    val stationId = IR_VariableAccess("stationId", IR_IntegerDatatype)
    val stationStmts = ListBuffer[IR_Statement]()

    val vPos = IR_VariableAccess("vPos", IR_ArrayDatatype(IR_DoubleDatatype, 8))

    val quantity = IR_VariableAccess("quantity", IR_DoubleDatatype)

    def linVertArray(vid : Int, dim : Int) = IR_ArrayAccess(vPos, 2 * vid + dim)

    //stationStmts += IR_IfCondition(IR_Neq(IR_IV_StationsLower(stationId), IR_IV_StationsLower(stationId).resolveDefValue().get), ListBuffer[IR_Statement](
    //  IR_Comment("station found, add quantity to file"),
    //  IR_VariableDeclaration(vPos),
    //  IR_VariableDeclaration(quantity),
    //  IR_Assignment(linVertArray(0, 0), linearizedNodePositions(0, IR_IV_StationsLower(stationId), IR_ExpressionIndex(0, 0))),
    //  IR_Assignment(linVertArray(0, 1), linearizedNodePositions(1, IR_IV_StationsLower(stationId), IR_ExpressionIndex(0, 0))),
    //  IR_Assignment(linVertArray(1, 0), linearizedNodePositions(0, IR_IV_StationsLower(stationId), IR_ExpressionIndex(1, 0))),
    //  IR_Assignment(linVertArray(1, 1), linearizedNodePositions(1, IR_IV_StationsLower(stationId), IR_ExpressionIndex(1, 0))),
    //  IR_Assignment(linVertArray(2, 0), linearizedNodePositions(0, IR_IV_StationsLower(stationId), IR_ExpressionIndex(1, 1))),
    //  IR_Assignment(linVertArray(2, 1), linearizedNodePositions(1, IR_IV_StationsLower(stationId), IR_ExpressionIndex(1, 1))),
    //  IR_Assignment(linVertArray(3, 0), linearizedNodePositions(0, IR_IV_StationsLower(stationId), IR_ExpressionIndex(0, 1))),
    //  IR_Assignment(linVertArray(3, 1), linearizedNodePositions(1, IR_IV_StationsLower(stationId), IR_ExpressionIndex(0, 1))),
    //  IR_Assignment(quantity, IR_FunctionCall(IR_PlainInternalFunctionReference("evalQuantity", IR_UnitDatatype),
    //    ListBuffer[IR_Expression](
    //      IR_IV_Stations(stationId, 0), IR_IV_Stations(stationId, 1),
    //      linVertArray(0, 0), linVertArray(0, 1),
    //      linVertArray(1, 0), linVertArray(1, 1),
    //      linVertArray(3, 0), linVertArray(3, 1)
    //    ) ++ coeffsLower))
    //))

    //body += IR_LoopOverFragments(IR_ForLoop(IR_VariableDeclaration(stationId, 0), IR_Lower(stationId, Knowledge.swe_stationsMax), IR_PreIncrement(stationId), stationStmts))

    val fragId = IR_VariableAccess("fragId", IR_IntegerDatatype)
    val i0 = IR_VariableAccess("i0", IR_IntegerDatatype)
    val i1 = IR_VariableAccess("i1", IR_IntegerDatatype)
    val lowerLeftIdx = IR_ExpressionIndex(i0, i1)
    val linIndex = IR_VariableAccess("linInxed", IR_DoubleDatatype)

    stationStmts += IR_VariableDeclaration(vPos)
    stationStmts += IR_VariableDeclaration(quantity)

    stationStmts += IR_IfCondition(IR_EqEq(IR_IV_StationsFragment(stationId), IR_IV_StationsFragment(stationId).resolveDefValue().get), IR_Break())

    stationStmts += IR_VariableDeclaration(fragId, IR_IV_StationsFragment(stationId))
    stationStmts += IR_VariableDeclaration(i0, IR_IV_StationsId(stationId, 0))
    stationStmts += IR_VariableDeclaration(i1, IR_IV_StationsId(stationId, 1))
    stationStmts += IR_Assignment(linVertArray(0, 0), nodePositions(0, lowerLeftIdx, IR_ExpressionIndex(0, 0), fragId))
    stationStmts += IR_Assignment(linVertArray(0, 1), nodePositions(1, lowerLeftIdx, IR_ExpressionIndex(0, 0), fragId))
    stationStmts += IR_Assignment(linVertArray(1, 0), nodePositions(0, lowerLeftIdx, IR_ExpressionIndex(1, 0), fragId))
    stationStmts += IR_Assignment(linVertArray(1, 1), nodePositions(1, lowerLeftIdx, IR_ExpressionIndex(1, 0), fragId))
    stationStmts += IR_Assignment(linVertArray(2, 0), nodePositions(0, lowerLeftIdx, IR_ExpressionIndex(1, 1), fragId))
    stationStmts += IR_Assignment(linVertArray(2, 1), nodePositions(1, lowerLeftIdx, IR_ExpressionIndex(1, 1), fragId))
    stationStmts += IR_Assignment(linVertArray(3, 0), nodePositions(0, lowerLeftIdx, IR_ExpressionIndex(0, 1), fragId))
    stationStmts += IR_Assignment(linVertArray(3, 1), nodePositions(1, lowerLeftIdx, IR_ExpressionIndex(0, 1), fragId))
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
          linVertArray(0, 0), linVertArray(0, 1),
          linVertArray(1, 0), linVertArray(1, 1),
          linVertArray(3, 0), linVertArray(3, 1)
        ) ++ coeffsUpper(lowerLeftIdx, fragId))))

    val file = IR_VariableAccess("file", IR_SpecialDatatype("std::ofstream"))

    // write quantity to file
    def fileName = IR_VariableAccess("fileName", IR_StringDatatype)

    stationStmts += IR_VariableDeclaration(fileName)
    stationStmts += IR_BuildString(fileName, ListBuffer[IR_Expression](IR_IV_StationNames(stationId), IR_StringConstant(".txt")))

    stationStmts += IR_VariableDeclaration(file)
    stationStmts += IR_MemberFunctionCall(file, "open", ListBuffer[IR_Expression](fileName, "std::ios::app"))
    stationStmts += IR_Assert(IR_MemberFunctionCall(file, "is_open"), ListBuffer("\"Unable to open file \"", fileName), IR_FunctionCall("exit", 1))
    stationStmts += IR_Print(file, "std::scientific << std::setprecision(10)")
    stationStmts += IR_Print(file, quantity, IR_Print.endl)
    stationStmts += IR_MemberFunctionCall(file, "close")

    body += IR_ForLoop(IR_VariableDeclaration(stationId, 0), IR_Lower(stationId, Knowledge.swe_stationsMax), IR_PreIncrement(stationId), stationStmts)

    IR_PlainFunction(name, IR_UnitDatatype, body)
  }
}
