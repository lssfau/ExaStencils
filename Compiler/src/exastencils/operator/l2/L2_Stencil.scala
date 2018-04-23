package exastencils.operator.l2

import scala.collection.mutable._

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.baseExt.l2.L2_FieldIteratorAccess
import exastencils.core._
import exastencils.knowledge.l2.L2_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.operator.l3._
import exastencils.optimization.l2._
import exastencils.prettyprinting._
import exastencils.util.l2.L2_ReplaceExpressions

/// L2_Stencil

case class L2_Stencil(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var numDims : Int, // number of dimensions in the coefficients
    var colStride : Array[Double], // strides of the entries per dimension; 1 means stencil represents a square matrix, >1 means stride*n x n, <1 means n x n/stride
    var entries : ListBuffer[L2_StencilMappingEntry]) extends L2_LeveledKnowledgeObject[L3_Stencil] {

  override def createDuplicate() : L2_Stencil = {
    L2_Stencil.tupled(Duplicate(L2_Stencil.unapply(this).get))
  }

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "Operator " << name << "@" << level << " from Stencil {\n"
    out <<< (entries, "\n")
    out << "\n}"
  }

  override def progressImpl() = L3_Stencil(name, level, numDims, colStride, entries.map(_.progress))

  def numCases(d : Int) : Int = if (colStride(d) >= 1) 1 /*colStride(d).toInt*/ else (1.0 / colStride(d)).toInt

  def assembleOffsetMap() : Map[L2_Expression, ListBuffer[L2_ConstIndex]] = {
    val map = HashMap[L2_Expression, ListBuffer[L2_ConstIndex]]()

    assembleCases().foreach[Unit](c => {
      val cond = (0 until numDims).map(d => L2_EqEq(c(d), L2_Modulo(L2_FieldIteratorAccess(d), numCases(d)))).reduce(L2_AndAnd)
      val offsets = Duplicate(entries).filter(entry => {
        for (d <- 0 until numDims) {
          L2_ReplaceExpressions.toReplace = entry.row.indices(d)
          L2_ReplaceExpressions.replacement = c(d)
          L2_ReplaceExpressions.applyStandalone(entry.col)
        }

        entry.col.indices.map(L2_SimplifyExpression.simplifyFloatingExpr(_) match {
          case L2_RealConstant(v) => v.isValidInt
          case other              => Logger.warn(other); false
        }).reduce(_ && _)
      })

      if (map.contains(cond))
        map(cond) ++= offsets.map(_.asStencilOffsetEntry.offset)
      else
        map += (cond -> offsets.map(_.asStencilOffsetEntry.offset))
    })

    map
  }

  def getReach(dim : Int) = assembleOffsetMap().values.map(_.map(_ (dim)).max).max

  def findStencilEntry(offset : L2_ConstIndex) : Option[L2_StencilEntry] = {
    val index = findStencilEntryIndex(offset)
    if (index.isDefined)
      Some(entries(index.get))
    else
      None
  }

  def findStencilEntryIndex(offset : L2_ConstIndex) : Option[Int] = {
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

  def printStencilToStr(noStructure : Boolean = false) : String = {
    var s : String = s"Stencil $name@$level:\n\n"

    if ((2 == numDims || 3 == numDims) && !colStride.exists(_ != 1) && !noStructure) {
      // special handling for 2D and 3D intra-level stencils

      val entriesAsOffset = entries.map(Duplicate(_).asStencilOffsetEntry)

      def printEntry(it : Array[Int]) = {
        s += "\t" +
          entriesAsOffset.find(
            e => e.offset match {
              case index if index.length >= numDims =>
                (0 until numDims).map(d => index(d) == it(d)).reduce(_ && _)
              case _                                => false
            }).getOrElse(L2_StencilOffsetEntry(L2_ConstIndex(), 0)).coefficient.prettyprint
      }

      numDims match {
        case 2 =>
          for (y <- getReach(1) to -getReach(1) by -1) {
            for (x <- -getReach(0) to getReach(0))
              printEntry(Array(x, y))
            s += "\n"
          }
        case 3 =>
          for (z <- -getReach(2) to getReach(2)) {
            for (y <- getReach(1) to -getReach(1) by -1) {
              for (x <- -getReach(0) to getReach(0))
                printEntry(Array(x, y, z))
              s += "\n"
            }
            s += "\n\n"
          }
      }
    } else {
      // fallback
      s += entries.map(_.prettyprint()).mkString("\n")
    }

    s
  }

  def squash() = {
    case class Mapping(var row : L2_ExpressionIndex, var col : L2_ExpressionIndex)

    val newEntries = HashMap[Mapping, L2_Expression]()

    entries.foreach(_.row.indices.transform(L2_SimplifyExpression.simplifyFloatingExpr))
    entries.foreach(_.col.indices.transform(L2_SimplifyExpression.simplifyFloatingExpr))

    for (entry <- entries) {
      val id = Mapping(entry.row, entry.col)
      if (newEntries.contains(id))
        newEntries(id) += entry.coefficient
      else
        newEntries += ((id, entry.coefficient))
    }

    entries = newEntries.to[ListBuffer].sortBy(_._1.col.prettyprint()).map {
      case (mapping, coeff) => L2_StencilMappingEntry(mapping.row, mapping.col, coeff)
    }

    entries.foreach(L2_GeneralSimplify.doUntilDoneStandalone(_))
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
        val simplified = L2_SimplifyExpression.simplifyFloatingExpr(entry.coefficient)
        //entry.coefficient = simplified

        simplified match {
          case L2_RealConstant(0.0) => false
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
          L2_ReplaceExpressions.toReplace = entry.row.indices(d)
          L2_ReplaceExpressions.replacement = curCase(d)
          L2_ReplaceExpressions.applyStandalone(newIndex)
        }
        newIndex.indices.map(L2_SimplifyExpression.simplifyFloatingExpr(_) match {
          case L2_RealConstant(v) => v.isValidInt
          case other              => Logger.warn(other); false
        }).reduce(_ && _)
      }).reduce(_ || _)
    })
  }
}
