package exastencils.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_FieldCollection
import exastencils.grid.ir.IR_VF_NodePositionAsVec
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print

case class IR_WriteStations(var arguments : ListBuffer[IR_Expression]) extends IR_FuturePlainFunction {
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

    IR_OrOr(IR_GreaterEqual(det1, 0), IR_OrOr(IR_GreaterEqual(det2, 0), IR_GreaterEqual(det3, 0)))
  }

  override def generateFct() = {
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

    def nodePositions(dim : Int, offset : IR_ExpressionIndex = IR_ExpressionIndex(0, 0)) = IR_VF_NodePositionPerDim(field.level, field.domain, dim).resolve(IR_LoopOverDimensions.defIt(numDims) + offset)

    val coeffsFieldAccess = coeffsField.map { case f : IR_Field => IR_FieldAccess(IR_FieldSelection(f, f.level, 0), IR_LoopOverDimensions.defIt(numDims)) }

    val coeffsLower = coeffsFieldAccess.zipWithIndex.collect { case (e, i) if i % 2 == 0 => e }
    val coeffsUpper = coeffsFieldAccess.zipWithIndex.collect { case (e, i) if i % 2 == 1 => e }

    val stationX = IR_IV_Stations(0, 0)
    val stationY = IR_IV_Stations(0, 1)
    //  v3 -- v2
    //  |     |
    //  v0 -- v1
    val vPos = IR_VariableAccess("vPos", IR_ArrayDatatype(IR_DoubleDatatype, 8))
    body += IR_VariableDeclaration(vPos)
    val quantity = IR_VariableAccess("quantity", IR_DoubleDatatype)
    body += IR_VariableDeclaration(quantity, 9999)
    val hasStation = IR_VariableAccess("hasStation", IR_BooleanDatatype)
    body += IR_VariableDeclaration(hasStation, false)

    def linVertArray(vid : Int, dim : Int) = IR_ArrayAccess(vPos, 2 * vid + dim)

    var fragStmts = ListBuffer[IR_Statement]()
    fragStmts += IR_IfCondition(hasStation, IR_Break())
    fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(0, i), nodePositions(i, IR_ExpressionIndex(0, 0))) }
    fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(1, i), nodePositions(i, IR_ExpressionIndex(1, 0))) }
    fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(2, i), nodePositions(i, IR_ExpressionIndex(0, 1))) }
    fragStmts ++= (0 until numDims).toArray.map { i => IR_Assignment(linVertArray(3, i), nodePositions(i, IR_ExpressionIndex(1, 1))) }

    // find triangle (lower or upper)
    // lower (0,1,3)
    fragStmts += IR_Comment("lower triangle (0,1,3)")
    fragStmts += IR_IfCondition(isInTriangle(stationX, stationY,
      linVertArray(0, 0), linVertArray(0, 1),
      linVertArray(1, 0), linVertArray(1, 1),
      linVertArray(3, 0), linVertArray(3, 1)), ListBuffer[IR_Statement](
      // eval quantity
      IR_Assignment(hasStation, true),
      IR_Assignment(quantity, IR_FunctionCall(IR_PlainInternalFunctionReference("evalQuantity", IR_UnitDatatype),
        ListBuffer[IR_Expression](
          stationX, stationY,
          linVertArray(0, 0), linVertArray(0, 1),
          linVertArray(1, 0), linVertArray(1, 1),
          linVertArray(3, 0), linVertArray(3, 1)
        ) ++ coeffsLower))
    ))

    // upper (2,3,1)
    fragStmts += IR_Comment("upper triangle (2,3,1)")
    fragStmts += IR_IfCondition(isInTriangle(stationX, stationY,
      linVertArray(2, 0), linVertArray(2, 1),
      linVertArray(3, 0), linVertArray(3, 1),
      linVertArray(1, 0), linVertArray(1, 1)), ListBuffer[IR_Statement](
      // eval quantity
      IR_Assignment(hasStation, true),
      IR_Assignment(quantity, IR_FunctionCall(IR_PlainInternalFunctionReference("evalQuantity", IR_UnitDatatype),
        ListBuffer[IR_Expression](
          stationX, stationY,
          linVertArray(2, 0), linVertArray(2, 1),
          linVertArray(3, 0), linVertArray(3, 1),
          linVertArray(1, 0), linVertArray(1, 1)
        ) ++ coeffsUpper))
    ))

    val start = IR_ExpressionIndex((0 until numDims).toArray.map { i => resolveIndex("DLB", i) })
    val end = IR_ExpressionIndex((0 until numDims).toArray.map { i => resolveIndex("DRE", i) - 1 : IR_Expression })

    body += IR_LoopOverFragments(IR_LoopOverDimensions(numDims, IR_ExpressionIndexRange(start, end), fragStmts))

    val file = IR_VariableAccess("file", IR_SpecialDatatype("std::ofstream"))

    // write quantity to file

    def fileName = IR_VariableAccess("fileName", IR_StringDatatype)

    body += IR_VariableDeclaration(file)
    body += IR_MemberFunctionCall(file, "open", ListBuffer[IR_Expression](fileName, "std::ios::app"))
    body += IR_Assert(IR_MemberFunctionCall(file, "is_open"), ListBuffer("\"Unable to open file \"", fileName), IR_FunctionCall("exit", 1))

    body += IR_Print(file, quantity, IR_Print.endl)
    body += IR_MemberFunctionCall(file, "close")

    IR_PlainFunction(name, IR_UnitDatatype, IR_FunctionArgument(fileName), body)
    //IR_PlainFunction(name, IR_UnitDatatype, , body)
  }
}
