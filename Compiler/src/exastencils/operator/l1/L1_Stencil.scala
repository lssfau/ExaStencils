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

package exastencils.operator.l1

import scala.collection.mutable._

import exastencils.base.l1.L1_ImplicitConversion._
import exastencils.base.l1._
import exastencils.core.Duplicate
import exastencils.knowledge.l1.L1_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.operator.l2._
import exastencils.optimization.l1._
import exastencils.prettyprinting._

/// L1_Stencil

case class L1_Stencil(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var numDims : Int, // number of dimensions in the coefficients
    var entries : ListBuffer[L1_StencilEntry]) extends L1_LeveledKnowledgeObject[L2_Stencil] {

  override def createDuplicate() : L1_Stencil = {
    L1_Stencil.tupled(Duplicate(L1_Stencil.unapply(this).get))
  }

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "Operator " << name << "@" << level << " from Stencil {\n"
    out <<< (entries, "\n")
    out << "\n}"
  }

  override def progressImpl() = L2_Stencil(name, level, numDims, Array.fill(numDims)(1.0), entries.map(_.progress.asStencilMappingEntry))
  def assembleOffsetMap() = entries.map(_.offset)
  def getReach(dim : Int) = assembleOffsetMap().map(_.indices(dim)).max

  def findStencilEntry(offset : L1_ConstIndex) : Option[L1_StencilEntry] = {
    val index = findStencilEntryIndex(offset)
    if (index.isDefined)
      Some(entries(index.get))
    else
      None
  }

  def findStencilEntryIndex(offset : L1_ConstIndex) : Option[Int] = {
    val entry = entries.zipWithIndex.find(_._1.offset == offset)
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

    if ((2 == numDims || 3 == numDims) && !noStructure) {
      // special handling for 2D and 3D intra-level stencils

      val entriesAsOffset = entries.map(Duplicate(_))

      def printEntry(it : Array[Int]) = {
        s += "\t" +
          entriesAsOffset.find(
            e => e.offset match {
              case index if index.length >= numDims =>
                (0 until numDims).map(d => index(d) == it(d)).reduce(_ && _)
              case _                                => false
            }).getOrElse(L1_StencilEntry(L1_ConstIndex(), 0)).coefficient.prettyprint
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
    val newEntries = ListBuffer[L1_StencilEntry]()

    for (entry <- entries) {
      val dup = newEntries.find(_.offset == entry.offset)

      if (dup.isDefined)
        dup.get.coefficient += entry.coefficient
      else
        newEntries += entry
    }

    entries = newEntries

    entries.foreach(L1_GeneralSimplify.doUntilDoneStandalone(_))
  }

  def filter() = {
    // remove entries with zero coefficients
    entries = entries.filter(entry => {
      try {
        val simplified = L1_SimplifyExpression.simplifyFloatingExpr(entry.coefficient)
        //entry.coefficient = simplified

        simplified match {
          case L1_RealConstant(0.0) => false
          case _                    => true
        }
      } catch {
        // keep entry if eval is not possible
        case _ : EvaluationException => true
      }
    })
  }
}
