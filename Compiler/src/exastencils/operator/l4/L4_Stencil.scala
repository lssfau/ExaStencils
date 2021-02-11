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

package exastencils.operator.l4

import scala.collection.mutable._

import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_FieldIteratorAccess
import exastencils.core._
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.operator.ir._
import exastencils.optimization.l4._
import exastencils.prettyprinting._
import exastencils.util.l4.L4_ReplaceExpressions

/// L4_Stencil

case class L4_Stencil(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var numDims : Int, // number of dimensions in the coefficients
    var colStride : Array[Double], // strides of the entries per dimension; 1 means stencil represents a square matrix, >1 means stride*n x n, <1 means n x n/stride
    var entries : ListBuffer[L4_StencilMappingEntry]) extends L4_LeveledKnowledgeObject[IR_Stencil] {

  override def createDuplicate() : L4_Stencil = {
    L4_Stencil.tupled(Duplicate(L4_Stencil.unapply(this).get))
  }

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "Stencil " << name << "@" << level << "{\n"
    out <<< (entries, ",\n")
    out << "\n}"
  }

  override def progressImpl() = IR_Stencil(name, level, numDims, colStride, entries.map(_.progress))

  def numCases(d : Int) : Int = if (colStride(d) >= 1) 1 /*colStride(d).toInt*/ else (1.0 / colStride(d)).toInt

  def assembleOffsetMap() : Map[L4_Expression, ListBuffer[L4_ConstIndex]] = {
    val map = HashMap[L4_Expression, ListBuffer[L4_ConstIndex]]()

    assembleCases().foreach[Unit](c => {
      val cond = (0 until numDims).map(d => L4_EqEq(c(d), L4_Modulo(L4_FieldIteratorAccess(d), numCases(d)))).reduce(L4_AndAnd)
      val offsets = Duplicate(entries).filter(entry => {
        for (d <- 0 until numDims) {
          L4_ReplaceExpressions.toReplace = entry.row.indices(d)
          L4_ReplaceExpressions.replacement = c(d)
          L4_ReplaceExpressions.applyStandalone(entry.col)
        }

        entry.col.indices.map(L4_SimplifyExpression.simplifyFloatingExpr(_) match {
          case L4_RealConstant(v) => v.isValidInt
          case other              => Logger.warn(s"Found $other in stencil $name@$level"); false
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

  def findStencilEntry(offset : L4_ConstIndex) : Option[L4_StencilEntry] = {
    val index = findStencilEntryIndex(offset)
    if (index.isDefined)
      Some(entries(index.get))
    else
      None
  }

  def findStencilEntryIndex(offset : L4_ConstIndex) : Option[Int] = {
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
            }).getOrElse(L4_StencilOffsetEntry(L4_ConstIndex(), 0)).coefficient.prettyprint
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
    case class Mapping(var row : L4_ExpressionIndex, var col : L4_ExpressionIndex)

    val newEntries = HashMap[Mapping, L4_Expression]()

    entries.foreach(_.row.indices.transform(L4_SimplifyExpression.simplifyFloatingExpr))
    entries.foreach(_.col.indices.transform(L4_SimplifyExpression.simplifyFloatingExpr))

    for (entry <- entries) {
      val id = Mapping(entry.row, entry.col)
      if (newEntries.contains(id))
        newEntries(id) += entry.coefficient
      else
        newEntries += ((id, entry.coefficient))
    }

    entries = newEntries.to[ListBuffer].sortBy(_._1.col.prettyprint()).map {
      case (mapping, coeff) => L4_StencilMappingEntry(mapping.row, mapping.col, coeff)
    }

    entries.foreach(L4_GeneralSimplify.doUntilDoneStandalone(_))
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
        val simplified = L4_SimplifyExpression.simplifyFloatingExpr(entry.coefficient)
        //entry.coefficient = simplified

        simplified match {
          case L4_RealConstant(0.0) => false
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
          L4_ReplaceExpressions.toReplace = entry.row.indices(d)
          L4_ReplaceExpressions.replacement = curCase(d)
          L4_ReplaceExpressions.applyStandalone(newIndex)
        }
        newIndex.indices.map(L4_SimplifyExpression.simplifyFloatingExpr(_) match {
          case L4_RealConstant(v) => v.isValidInt
          case other              => Logger.warn(other); false
        }).reduce(_ && _)
      }).reduce(_ || _)
    })
  }
}
