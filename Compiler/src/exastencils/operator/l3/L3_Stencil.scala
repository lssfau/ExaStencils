package exastencils.operator.l3

import scala.collection.mutable._

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_FieldIteratorAccess
import exastencils.core._
import exastencils.knowledge.l3.L3_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.operator.l4._
import exastencils.optimization.l3._
import exastencils.prettyprinting._
import exastencils.util.l3.L3_ReplaceExpressions

/// L3_Stencil

case class L3_Stencil(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var numDims : Int, // number of dimensions in the coefficients
    var colStride : Array[Double], // strides of the entries per dimension; 1 means stencil represents a square matrix, >1 means stride*n x n, <1 means n x n/stride
    var entries : ListBuffer[L3_StencilMappingEntry]) extends L3_LeveledKnowledgeObject[L4_Stencil] {

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "Operator " << name << "@" << level << " from Stencil {\n"
    out <<< (entries, "\n")
    out << "\n}"
  }

  override def progressImpl() = L4_Stencil(name, level, numDims, colStride, entries.map(_.progress))

  def numCases(d : Int) : Int = if (colStride(d) >= 1) 1/*colStride(d).toInt*/ else (1.0 / colStride(d)).toInt

  def assembleOffsetMap() : Map[L3_Expression, ListBuffer[L3_ConstIndex]] = {
    val map = HashMap[L3_Expression, ListBuffer[L3_ConstIndex]]().withDefaultValue(ListBuffer())

    assembleCases().foreach(c => {
      val cond = (0 until numDims).map(d => L3_EqEq(c(d), L3_Modulo(L3_FieldIteratorAccess(d), numCases(d)))).reduce(L3_AndAnd)
      val offsets = Duplicate(entries).filter(entry => {
        for (d <- 0 until numDims) {
          L3_ReplaceExpressions.toReplace = entry.row.indices(d)
          L3_ReplaceExpressions.replacement = c(d)
          L3_ReplaceExpressions.applyStandalone(entry.col)
        }

        entry.col.indices.map(L3_SimplifyExpression.simplifyFloatingExpr(_) match {
          case L3_RealConstant(v) => v.isValidInt
          case other              => Logger.warn(other); false
        }).reduce(_ && _)
      })

      map(cond) ++= offsets.map(_.asStencilOffsetEntry.offset)
    })

    map
  }

  def getReach(dim : Int) = assembleOffsetMap().values.map(_.map(_ (dim)).max).max

  def findStencilEntry(offset : L3_ConstIndex) : Option[L3_StencilEntry] = {
    val index = findStencilEntryIndex(offset)
    if (index.isDefined)
      Some(entries(index.get))
    else
      None
  }

  def findStencilEntryIndex(offset : L3_ConstIndex) : Option[Int] = {
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
//                case index : L3_ExpressionIndex if index.length >= 3 => (
//                  (index(0) match { case L3_IntegerConstant(xOff) if x == xOff => true; case _ => false })
//                    && (index(1) match { case L3_IntegerConstant(yOff) if y == yOff => true; case _ => false })
//                    && (index(2) match { case L3_IntegerConstant(zOff) if z == zOff => true; case _ => false }))
//                case _                                               => false
//              }).getOrElse(L3_StencilEntry(L3_ExpressionIndex(), 0)).coefficient.prettyprint
//        s += "\n"
//      }
//      s += "\n\n"
//    }
//
//    s
  }

  def squash() = {
    case class Mapping(var row : L3_ExpressionIndex, var col : L3_ExpressionIndex)

    val newEntries = HashMap[Mapping, L3_Expression]()

    entries.foreach(_.row.indices.transform(L3_SimplifyExpression.simplifyFloatingExpr))
    entries.foreach(_.col.indices.transform(L3_SimplifyExpression.simplifyFloatingExpr))

    for (entry <- entries) {
      val id = Mapping(entry.row, entry.col)
      if (newEntries.contains(id))
        newEntries(id) += entry.coefficient
      else
        newEntries += ((id, entry.coefficient))
    }

    entries = newEntries.to[ListBuffer].sortBy(_._1.col.prettyprint()).map {
      case (mapping, coeff) => L3_StencilMappingEntry(mapping.row, mapping.col, coeff)
    }

    entries.foreach(L3_GeneralSimplify.doUntilDoneStandalone(_))
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
        val simplified = L3_SimplifyExpression.simplifyFloatingExpr(entry.coefficient)
        //entry.coefficient = simplified

        simplified match {
          case L3_RealConstant(0.0) => false
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
          L3_ReplaceExpressions.toReplace = entry.row.indices(d)
          L3_ReplaceExpressions.replacement = curCase(d)
          L3_ReplaceExpressions.applyStandalone(newIndex)
        }
        newIndex.indices.map(L3_SimplifyExpression.simplifyFloatingExpr(_) match {
          case L3_RealConstant(v) => v.isValidInt
          case other              => Logger.warn(other); false
        }).reduce(_ && _)
      }).reduce(_ || _)
    })
  }
}
