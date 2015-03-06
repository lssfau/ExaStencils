package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.logger._
import exastencils.util._

case class StencilEntry(var offset : MultiIndex, var coefficient : Expression) {}

case class Stencil(var identifier : String, var level : Int, var entries : ListBuffer[StencilEntry] = new ListBuffer) {
  def getReach(dim : Int) : Int = {
    var reach : Int = 0
    // get max reach
    for (e <- entries) {
      reach = reach max SimplifyExpression.evalIntegral(e.offset(dim)).toInt
    }
    reach
  }

  def findStencilEntry(offset : MultiIndex) : Option[StencilEntry] = {
    val index = findStencilEntryIndex(offset)
    if (index.isDefined)
      Some(entries(index.get))
    else
      None
  }

  def findStencilEntryIndex(offset : MultiIndex) : Option[Int] = {
    for (i <- 0 until entries.size) {
      var ret = true
      for (dim <- 0 until Knowledge.dimensionality)
        ret &= (offset(dim) == entries(i).offset(dim))

      if (ret) return Some(i)
    }

    Logger.warn(s"Trying to find stencil entry for invalid offset ${offset.prettyprint()} in stencil:\n" +
      entries.map(e => s"\t${e.offset.prettyprint()} -> ${e.coefficient.prettyprint()}").mkString("\n"))

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
                case MultiIndex(IntegerConstant(xOff), IntegerConstant(yOff), IntegerConstant(zOff), _) if (x == xOff && y == yOff && z == zOff) => true
                case _ => false
              }).getOrElse(StencilEntry(new MultiIndex, 0)).coefficient.prettyprint
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

case class StencilField(var identifier : String, var field : Field, var stencil : Stencil) {}

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
    var level : Expression,
    var slot : Expression,
    var arrayIndex : Option[Int],
    var fragIdx : Expression = LoopOverFragments.defIt) extends Node {

  def toFieldSelection = {
    new FieldSelection(field, level, slot, arrayIndex, fragIdx)
  }

  // shortcuts to stencilField members
  def field = stencilField.field
  def stencil = stencilField.stencil

  // shortcuts to Field members
  def codeName = field.codeName
  def dataType = field.dataType
  def fieldLayout = field.fieldLayout
  def referenceOffset = field.referenceOffset
}

object FindStencilConvolutions extends DefaultStrategy("FindStencilConvolutions") {
  this += new Transformation("SearchAndMark", {
    case MultiplicationExpression(StencilAccess(stencil), fieldAccess : FieldAccess) =>
      StencilConvolution(stencil, fieldAccess)
    case MultiplicationExpression(stencilFieldAccess : StencilFieldAccess, fieldAccess : FieldAccess) =>
      StencilFieldConvolution(stencilFieldAccess, fieldAccess)
    case MultiplicationExpression(StencilAccess(stencilLeft), StencilAccess(stencilRight)) =>
      StencilStencilConvolution(stencilLeft, stencilRight)
    case MultiplicationExpression(stencilLeft : StencilFieldAccess, StencilAccess(stencilRight)) =>
      StencilFieldStencilConvolution(stencilLeft, stencilRight)
    case MultiplicationExpression(StencilAccess(stencilLeft), stencilRight : StencilFieldAccess)       => ??? // TODO
    case MultiplicationExpression(stencilLeft : StencilFieldAccess, stencilRight : StencilFieldAccess) => ??? // TODO
  })
}

object MapStencilAssignments extends DefaultStrategy("MapStencilAssignments") {
  this += new Transformation("SearchAndMark", {
    case AssignmentStatement(stencilFieldAccess : StencilFieldAccess, StencilAccess(stencil), op) => {
      var statements : ListBuffer[Statement] = ListBuffer()

      val stencilRight = stencil
      val stencilLeft = stencilFieldAccess.stencilFieldSelection.stencil

      val flipEntries = false

      for (idx <- 0 until stencilLeft.entries.size) {
        var fieldSelection = stencilFieldAccess.stencilFieldSelection.toFieldSelection
        fieldSelection.arrayIndex = Some(idx)
        var fieldIndex = Duplicate(stencilFieldAccess.index)
        fieldIndex(Knowledge.dimensionality) = idx
        var coeff : Expression = 0
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

        statements += new AssignmentStatement(new FieldAccess(fieldSelection, fieldIndex), coeff, op)
      }

      statements
    }
  })
}
