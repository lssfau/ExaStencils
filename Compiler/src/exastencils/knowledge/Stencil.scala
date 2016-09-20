package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.logger._
import exastencils.stencil.ir._
import exastencils.util._
import exastencils.util.ir.IR_ResultingDatatype

case class StencilEntry(var offset : IR_ExpressionIndex, var coefficient : IR_Expression) {
  def datatype : IR_Datatype = coefficient.datatype
}

case class Stencil(var identifier : String, var level : Int, var entries : ListBuffer[StencilEntry] = new ListBuffer) extends IR_KnowledgeObject {
  def datatype = {
    var ret = entries(0).datatype
    entries.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
    ret
  }
  def getReach(dim : Int) : Int = {
    var reach : Int = 0
    // get max reach
    for (e <- entries) {
      reach = reach max SimplifyExpression.evalIntegral(e.offset(dim)).toInt
    }
    reach
  }

  def findStencilEntry(offset : IR_ExpressionIndex) : Option[StencilEntry] = {
    val index = findStencilEntryIndex(offset)
    if (index.isDefined)
      Some(entries(index.get))
    else
      None
  }

  def findStencilEntryIndex(offset : IR_ExpressionIndex) : Option[Int] = {
    for (i <- 0 until entries.size) {
      var ret = true
      for (dim <- 0 until Knowledge.dimensionality)
        ret &= (offset(dim) == entries(i).offset(dim))

      if (ret) return Some(i)
    }

    Logger.warn(s"Trying to find stencil entry for invalid offset ${ offset.prettyprint() } in stencil:\n" +
      entries.map(e => s"\t${ e.offset.prettyprint : String } -> ${ e.coefficient.prettyprint : String }").mkString("\n"))

    None
  }

  def printStencilToStr() : String = {
    var s : String = ""

    s += s"Stencil $identifier:\n\n"

    for (z <- -getReach(2) to getReach(2)) {
      for (y <- -getReach(1) to getReach(1)) {
        for (x <- -getReach(0) to getReach(0))
          s += "\t" +
            entries.find(
              e => e.offset match {
                case index : IR_ExpressionIndex if index.length >= 3 => (
                  (index(0) match { case IR_IntegerConstant(xOff) if x == xOff => true; case _ => false })
                    && (index(1) match { case IR_IntegerConstant(yOff) if y == yOff => true; case _ => false })
                    && (index(2) match { case IR_IntegerConstant(zOff) if z == zOff => true; case _ => false }))
                case _                                               => false
              }).getOrElse(StencilEntry(IR_ExpressionIndex(), 0)).coefficient.prettyprint
        s += "\n"
      }
      s += "\n\n"
    }

    s
  }
}

object StencilCollection {
  var stencils : ListBuffer[Stencil] = ListBuffer()

  def getStencilByIdentifier(identifier : String, level : Int) : Option[Stencil] = {
    val ret = stencils.find(s => s.identifier == identifier && s.level == level)
    if (ret.isEmpty) Logger.warn(s"Stencil $identifier on level $level was not found")
    ret
  }
}

case class StencilField(var identifier : String, var field : Field, var stencil : Stencil) extends IR_KnowledgeObject {
  // TODO: add level
}

object StencilFieldCollection {
  var stencilFields : ListBuffer[StencilField] = ListBuffer()

  def getStencilFieldByIdentifier(identifier : String, level : Int) : Option[StencilField] = {
    val ret = stencilFields.find(s => s.identifier == identifier && s.field.level == level)
    if (ret.isEmpty) Logger.warn(s"StencilField $identifier on level $level was not found")
    ret
  }
}

case class StencilFieldSelection(
    var stencilField : StencilField,
    var level : IR_Expression,
    var slot : IR_Expression,
    var arrayIndex : Option[Int],
    var fragIdx : IR_Expression = IR_LoopOverFragments.defIt) extends Node {

  def toFieldSelection = {
    new FieldSelection(field, level, slot, arrayIndex, fragIdx)
  }

  // shortcuts to stencilField members
  def field = stencilField.field
  def stencil = stencilField.stencil

  // shortcuts to Field members
  def codeName = field.codeName
  def fieldLayout = field.fieldLayout
  def referenceOffset = field.referenceOffset
}

object FindStencilConvolutions extends DefaultStrategy("FindStencilConvolutions") {
  var changed : Boolean = false

  def transformMultiplication(exp : IR_MultiplicationExpression) : IR_MultiplicationExpression = {
    val facts : ListBuffer[IR_Expression] = exp.factors
    var result = new ListBuffer[IR_Expression]()
    var prev : IR_Expression = null

    // check for StencilLike * FieldLike
    result = ListBuffer[IR_Expression]()
    prev = null
    for (f <- facts)
      (prev, f) match {
        case (IR_StencilAccess(stencil), fieldAccess : IR_FieldAccess)                  =>
          result += IR_StencilConvolution(stencil, fieldAccess)
          prev = null
        case (stencilFieldAccess : IR_StencilFieldAccess, fieldAccess : IR_FieldAccess) =>
          result += IR_StencilFieldConvolution(stencilFieldAccess, fieldAccess)
          prev = null
        case _                                                                          =>
          if (prev != null) result += prev
          prev = f
      }
    if (prev != null)
      result += prev
    changed |= facts.length != result.length
    if (facts.length != result.length)
      return IR_MultiplicationExpression(result)

    // check for StencilLike * Stencil
    result = ListBuffer[IR_Expression]()
    prev = null
    for (f <- facts)
      (prev, f) match {
        case (IR_StencilAccess(stencilLeft), IR_StencilAccess(stencilRight))       =>
          result += IR_StencilStencilConvolution(stencilLeft, stencilRight)
          prev = null
        case (stencilLeft : IR_StencilFieldAccess, IR_StencilAccess(stencilRight)) =>
          result += IR_StencilFieldStencilConvolution(stencilLeft, stencilRight)
          prev = null
        case _                                                                     =>
          if (prev != null) result += prev
          prev = f
      }
    if (prev != null)
      result += prev
    changed |= facts.length != result.length
    if (facts.length != result.length)
      return IR_MultiplicationExpression(result)

    // check for other convolutions
    result = ListBuffer[IR_Expression]()
    prev = null
    for (f <- facts)
      (prev, f) match {
        case (IR_StencilAccess(stencilLeft), stencilRight : IR_StencilFieldAccess)       =>
          ??? // TODO
        case (stencilLeft : IR_StencilFieldAccess, stencilRight : IR_StencilFieldAccess) =>
          ??? // TODO
        case _                                                                           =>
          if (prev != null) result += prev
          prev = f
      }
    if (prev != null)
      result += prev
    changed |= facts.length != result.length
    if (facts.length != result.length)
      return IR_MultiplicationExpression(result)

    exp
  }

  this += new Transformation("SearchAndMark", {
    case exp : IR_MultiplicationExpression => {
      val newMult = transformMultiplication(exp)
      newMult.factors.size match {
        case 0 => IR_NullExpression
        case 1 => newMult.factors(0)
        case _ => newMult
      }
    }
  })
}

object MapStencilAssignments extends DefaultStrategy("MapStencilAssignments") {
  this += new Transformation("SearchAndMark", {
    case IR_Assignment(stencilFieldAccess : IR_StencilFieldAccess, IR_StencilAccess(stencil), op) => {
      var statements : ListBuffer[IR_Statement] = ListBuffer()

      val stencilRight = stencil
      val stencilLeft = stencilFieldAccess.stencilFieldSelection.stencil

      val flipEntries = false

      for (idx <- 0 until stencilLeft.entries.size) {
        var fieldSelection = stencilFieldAccess.stencilFieldSelection.toFieldSelection
        fieldSelection.arrayIndex = Some(idx)
        var fieldIndex = Duplicate(stencilFieldAccess.index)
        fieldIndex(Knowledge.dimensionality) = idx
        var coeff : IR_Expression = 0
        for (e <- stencilRight.entries) {
          if (flipEntries) {
            if ((0 until Knowledge.dimensionality).map(dim =>
              (SimplifyExpression.evalIntegral(e.offset(dim)) == -SimplifyExpression.evalIntegral(stencilLeft.entries(idx).offset(dim))))
              .reduceLeft((a, b) => a && b))
              coeff += e.coefficient
          } else {
            if (e.offset == stencilLeft.entries(idx).offset)
              coeff += e.coefficient
          }
        }

        if (flipEntries)
          for (dim <- 0 until Knowledge.dimensionality)
            fieldIndex(dim) -= stencilLeft.entries(idx).offset(dim)

        statements += new IR_Assignment(new IR_FieldAccess(fieldSelection, fieldIndex), coeff, op)
      }

      statements
    }
  })
}
