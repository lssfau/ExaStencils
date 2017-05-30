package exastencils.operator.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FieldIteratorAccess
import exastencils.core.Duplicate
import exastencils.knowledge.ir.IR_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.util.ir._

/// IR_Stencil

case class IR_Stencil(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var numDims : Int, // number of dimensions in the coefficients
    var colStride : Array[Double], // strides of the entries per dimension; 1 means stencil represents a square matrix, >1 means stride*n x n, <1 means n x n/stride
    var entries : ListBuffer[IR_StencilMappingEntry]) extends IR_LeveledKnowledgeObject {

  def datatype = entries.foldLeft(entries.head.datatype)((dt, entry) => IR_ResultingDatatype(dt, entry.datatype))

  def numCases(d : Int) : Int = if (colStride(d) >= 1) 1/*colStride(d).toInt*/ else (1.0 / colStride(d)).toInt

  def assembleOffsetMap() : Map[IR_Expression, ListBuffer[IR_ConstIndex]] = {
    val map = HashMap[IR_Expression, ListBuffer[IR_ConstIndex]]().withDefaultValue(ListBuffer())

    assembleCases().foreach(c => {
      val cond = (0 until numDims).map(d => IR_EqEq(c(d), IR_Modulo(IR_FieldIteratorAccess(d), numCases(d)))).reduce(IR_AndAnd)
      val offsets = Duplicate(entries).filter(entry => {
        for (d <- 0 until numDims) {
          IR_ReplaceExpressions.toReplace = entry.row.indices(d)
          IR_ReplaceExpressions.replacement = c(d)
          IR_ReplaceExpressions.applyStandalone(entry.col)
        }

        entry.col.indices.map(IR_SimplifyExpression.simplifyFloatingExpr(_) match {
          case IR_RealConstant(v) => v.isValidInt
          case other              => Logger.warn(other); false
        }).reduce(_ && _)
      })

      map(cond) ++= offsets.map(_.asStencilOffsetEntry.offset)
    })

    map
  }

  def getReach(dim : Int) = assembleOffsetMap().values.map(_.map(_ (dim)).max).max

  def findStencilEntry(offset : IR_ConstIndex) : Option[IR_StencilEntry] = {
    val index = findStencilEntryIndex(offset)
    if (index.isDefined)
      Some(entries(index.get))
    else
      None
  }

  def findStencilEntryIndex(offset : IR_ConstIndex) : Option[Int] = {
    if (colStride.exists(_ != 1.0))
      Logger.warn(s"Trying to access a specific offset for stencil with unsupported stride(s) [ ${ colStride.mkString(", ") } ]")

    val entry = entries.zipWithIndex.find(_._1.asStencilOffsetEntry.offset == offset)
    if (entry.isDefined) {
      Some(entry.get._2)
    } else {
      Logger.warn(s"Trying to find stencil entry for invalid offset ${ offset.prettyprint() } in stencil:\n" +
        entries.map(_.prettyprint()).mkString("\n"))
      None
    }
  }

  def printStencilToStr() : String = {
    ???
//    var s : String = ""
//
//    s += s"Stencil $name:\n\n"
//
//    for (z <- -getReach(2) to getReach(2)) {
//      for (y <- -getReach(1) to getReach(1)) {
//        for (x <- -getReach(0) to getReach(0))
//          s += "\t" +
//            entries.find(
//              e => e.offset match {
//                case index : IR_ExpressionIndex if index.length >= 3 => (
//                  (index(0) match { case IR_IntegerConstant(xOff) if x == xOff => true; case _ => false })
//                    && (index(1) match { case IR_IntegerConstant(yOff) if y == yOff => true; case _ => false })
//                    && (index(2) match { case IR_IntegerConstant(zOff) if z == zOff => true; case _ => false }))
//                case _                                               => false
//              }).getOrElse(IR_StencilEntry(IR_ExpressionIndex(), 0)).coefficient.prettyprint
//        s += "\n"
//      }
//      s += "\n\n"
//    }
//
//    s
  }

  def squash() = {
    case class Mapping(var row : IR_ExpressionIndex, var col : IR_ExpressionIndex)

    val newEntries = HashMap[Mapping, IR_Expression]()

    entries.foreach(_.row.indices.transform(IR_SimplifyExpression.simplifyFloatingExpr))
    entries.foreach(_.col.indices.transform(IR_SimplifyExpression.simplifyFloatingExpr))

    for (entry <- entries) {
      val id = Mapping(entry.row, entry.col)
      if (newEntries.contains(id))
        newEntries(id) += entry.coefficient
      else
        newEntries += ((id, entry.coefficient))
    }

    entries = newEntries.to[ListBuffer].sortBy(_._1.col.prettyprint()).map {
      case (mapping, coeff) => IR_StencilMappingEntry(mapping.row, mapping.col, coeff)
    }

    entries.foreach(IR_GeneralSimplify.doUntilDoneStandalone(_))
  }

  def assembleCases() : ListBuffer[ListBuffer[Int]] = {
    var cases = ListBuffer.range(0, numCases(0)).map(i => ListBuffer(i))
    for (d <- 1 until numDims)
      cases = ListBuffer.range(0, numCases(d)).flatMap(i => cases.map(_ :+ i))

    cases
  }

  def filter() = {
    // remove entries with zero coefficients
    entries = entries.filter(entry => {
      try {
        val simplified = IR_SimplifyExpression.simplifyFloatingExpr(entry.coefficient)
        //entry.coefficient = simplified

        simplified match {
          case IR_RealConstant(0.0) => false
          case _                    => true
        }
      } catch {
        // keep entry if eval is not possible
        case _ : EvaluationException => true
      }
    })

    // remove entries with invalid row/column pairs
    entries = entries.filter(entry => {
      // filter entries with invalid indices
      val cases = assembleCases()

      // check if at least one case exists that emits valid indices
      cases.map(curCase => {
        val newIndex = Duplicate(entry.col)
        for (d <- 0 until numDims) {
          IR_ReplaceExpressions.toReplace = entry.row.indices(d)
          IR_ReplaceExpressions.replacement = curCase(d)
          IR_ReplaceExpressions.applyStandalone(newIndex)
        }
        newIndex.indices.map(IR_SimplifyExpression.simplifyFloatingExpr(_) match {
          case IR_RealConstant(v) => v.isValidInt
          case other              => Logger.warn(other); false
        }).reduce(_ && _)
      }).reduce(_ || _)
    })
  }
}
