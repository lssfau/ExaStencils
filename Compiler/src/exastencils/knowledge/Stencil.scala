package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.Logger._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.strategies._
import exastencils.util._

case class StencilEntry(var offset : MultiIndex, var weight : Expression) {}

case class Stencil(var identifier : String, var level : Int, var entries : ListBuffer[StencilEntry] = new ListBuffer) {
  def getReach(dim : Int) : Int = {
    var reach : Int = 0
    // get max reach
    for (e <- entries) {
      reach = reach max SimplifyExpression.evalIntegral(e.offset(dim)).toInt
    }
    reach
  }

  def printStencil() : Unit = {
    println(s"Stencil $identifier:")
    println

    for (z <- -getReach(2) to getReach(2)) {
      for (y <- -getReach(1) to getReach(1)) {
        for (x <- -getReach(0) to getReach(0))
          print("\t" +
            entries.find(
              e => e.offset match {
                case MultiIndex(IntegerConstant(xOff), IntegerConstant(yOff), IntegerConstant(zOff), _) if (x == xOff && y == yOff && z == zOff) => true
                case _ => false
              }).getOrElse(StencilEntry(new MultiIndex, 0)).weight.cpp)
        println
      }
      println
      println
    }
  }
}

object StencilCollection {
  var stencils : ListBuffer[Stencil] = ListBuffer()

  def getStencilByIdentifier(identifier : String, level : Int) : Option[Stencil] = {
    val ret = stencils.find(s => s.identifier == identifier && s.level == level)
    if (ret.isEmpty) warn(s"Stencil $identifier on level $level was not found")
    ret
  }
}

case class StencilField(var identifier : String, var field : Field, var stencil : Stencil) {}

object StencilFieldCollection {
  var stencilFields : ListBuffer[StencilField] = ListBuffer()

  def getStencilFieldByIdentifier(identifier : String, level : Int) : Option[StencilField] = {
    val ret = stencilFields.find(s => s.identifier == identifier && s.field.level == level)
    if (ret.isEmpty) warn(s"StencilField $identifier on level $level was not found")
    ret
  }
}

case class StencilFieldSelection(
    var stencilField : StencilField,
    var level : Expression,
    var slot : Expression,
    var arrayIndex : Int,
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
  def layout = field.layout
  def referenceOffset = field.referenceOffset
}

case class StencilConvolution(var stencil : Stencil, var fieldAccess : FieldAccess) extends Expression with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = StencilConvolution\n"

  def resolveEntry(idx : Int) : Expression = {
    stencil.entries(idx).weight * new FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + stencil.entries(idx).offset)
  }

  def expand : Output[Expression] = {
    var ret : Expression = (0 until stencil.entries.size).toArray.map(idx => resolveEntry(idx)).toArray[Expression].reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

case class StencilFieldConvolution(var stencilFieldAccess : StencilFieldAccess, var fieldAccess : FieldAccess) extends Expression with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = StencilConvolution\n"

  def resolveEntry(idx : Int) : Expression = {
    var stencilFieldIdx = Duplicate(stencilFieldAccess.index)
    stencilFieldIdx(Knowledge.dimensionality) = idx

    FieldAccess(stencilFieldAccess.stencilFieldSelection.toFieldSelection, stencilFieldIdx) *
      new FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + stencilFieldAccess.stencilFieldSelection.stencil.entries(idx).offset)
  }

  def expand : Output[Expression] = {
    var ret : Expression = (0 until stencilFieldAccess.stencilFieldSelection.stencil.entries.size).toArray.map(idx => resolveEntry(idx)).toArray[Expression].reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

case class StencilStencilConvolution(var stencilLeft : Stencil, var stencilRight : Stencil) extends Expression with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = StencilStencilConvolution\n"

  def expand : Output[StencilAccess] = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()

    for (re <- stencilRight.entries) {
      for (le <- stencilLeft.entries) {
        var rightOffset = Duplicate(re.offset)
        //            if (stencilRight.level < stencilLeft.level) {
        //              for (d <- 0 until Knowledge.dimensionality)
        //                rightOffset(d) = (dimToString(d) : Expression) * 2 + rightOffset(d)
        //            }

        var leftOffset = Duplicate(le.offset)
        if (stencilRight.level > stencilLeft.level) {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : Expression) / 2 + leftOffset(d)
        } else {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : Expression) + leftOffset(d)
        }

        var combOff = leftOffset
        ResolveCoordinates.replacement = rightOffset
        ResolveCoordinates.doUntilDoneStandalone(combOff)

        var combCoeff : Expression = (re.weight * le.weight)
        SimplifyStrategy.doUntilDoneStandalone(combOff)
        SimplifyStrategy.doUntilDoneStandalone(combCoeff)
        var addToEntry = entries.find(e => e.offset match { case o if (combOff == o) => true; case _ => false })
        if (addToEntry.isDefined) {
          combCoeff += addToEntry.get.weight
          SimplifyStrategy.doUntilDoneStandalone(combCoeff)
          addToEntry.get.weight = combCoeff
        } else entries += new StencilEntry(combOff, combCoeff)
      }
    }

    StencilAccess(Stencil(stencilLeft.identifier + "_" + stencilRight.identifier, stencilLeft.level, entries))
  }
}

case class StencilFieldStencilConvolution(var stencilLeft : StencilFieldAccess, var stencilRight : Stencil) extends Expression with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = StencilFieldStencilConvolution\n"

  def expand : Output[StencilAccess] = {
    var entries : ListBuffer[StencilEntry] = ListBuffer()

    for (re <- stencilRight.entries) {
      for (e <- 0 until stencilLeft.stencilFieldSelection.stencil.entries.size) {
        var stencilFieldIdx = Duplicate(stencilLeft.index)
        stencilFieldIdx(Knowledge.dimensionality) = e
        for (dim <- 0 until Knowledge.dimensionality)
          stencilFieldIdx(dim) += re.offset(dim)
        var fieldSel = stencilLeft.stencilFieldSelection.toFieldSelection
        fieldSel.arrayIndex = e

        var rightOffset = Duplicate(re.offset)

        var leftOffset = Duplicate(stencilLeft.stencilFieldSelection.stencil.entries(e).offset)
        if (stencilRight.level > stencilLeft.stencilFieldSelection.stencil.level) {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : Expression) / 2 + leftOffset(d)
        } else {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (dimToString(d) : Expression) + leftOffset(d)
        }

        var combOff = leftOffset
        ResolveCoordinates.replacement = rightOffset
        ResolveCoordinates.doUntilDoneStandalone(combOff)

        var combCoeff : Expression = (re.weight * new FieldAccess(fieldSel, stencilFieldIdx))
        SimplifyStrategy.doUntilDoneStandalone(combOff)
        SimplifyStrategy.doUntilDoneStandalone(combCoeff)
        var addToEntry = entries.find(e => e.offset match { case o if (combOff == o) => true; case _ => false })
        if (addToEntry.isDefined) {
          combCoeff += addToEntry.get.weight
          SimplifyStrategy.doUntilDoneStandalone(combCoeff)
          addToEntry.get.weight = combCoeff
        } else entries += new StencilEntry(combOff, combCoeff)
      }
    }

    StencilAccess(Stencil(stencilLeft.stencilFieldSelection.stencil.identifier + "_" + stencilRight.identifier, stencilLeft.stencilFieldSelection.stencil.level, entries))
  }
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
    //    case MultiplicationExpression(StencilAccess(stencilLeft), stencilRight : StencilFieldAccess) => TODO
    //    case MultiplicationExpression(stencilLeft : StencilFieldAccess, stencilRight : StencilFieldAccess) => TODO
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
        fieldSelection.arrayIndex = idx
        var fieldIndex = Duplicate(stencilFieldAccess.index)
        fieldIndex(Knowledge.dimensionality) = idx
        var coeff : Expression = 0
        for (e <- stencilRight.entries) {
          if (flipEntries) {
            if ((0 until Knowledge.dimensionality).map(dim =>
              (SimplifyExpression.evalIntegral(e.offset(dim)) == -SimplifyExpression.evalIntegral(stencilLeft.entries(idx).offset(dim))))
              .reduceLeft((a, b) => a && b))
              coeff += e.weight
          } else {
            if (e.offset == stencilLeft.entries(idx).offset)
              coeff += e.weight
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
