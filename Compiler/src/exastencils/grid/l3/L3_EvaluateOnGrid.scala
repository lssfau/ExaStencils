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

package exastencils.grid.l3

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_FieldIteratorAccess
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.l3.L3_FieldAccess
import exastencils.grid.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_EvaluateOnGrid

object L3_EvaluateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[L3_Expression], offset : Option[L3_ConstIndex]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      L3_RealConstant(0.0)
    } else {
      if (args.length > 2) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      args match {
        case ListBuffer(access : L3_Expression) =>
          new L3_EvaluateOnGrid(name, level, access, "default", offset)

        case ListBuffer(access : L3_Expression, interpolation : L3_StringConstant) =>
          new L3_EvaluateOnGrid(name, level, access, interpolation.value, offset)

        case _ =>
          Logger.warn(s"Arguments (${ args.map(_.prettyprint).mkString(", ") }) are currently not supported for function $name")
          args(0)
      }
    }
  }

  def apply(stagDim : Option[Int], faceDim : Int, level : Int, expression : L3_Expression, interpolation : String = "default") =
    new L3_EvaluateOnGrid(s"evalAt${ L3_GridUtil.dimsToFace(stagDim, faceDim) }", level, expression, interpolation, None)
}

case class L3_EvaluateOnGrid(
    var name : String,
    var level : Int,
    var expression : L3_Expression,
    var interpolation : String,
    var offset : Option[L3_ConstIndex]) extends L3_Expression with L3_CanBeOffset with L3_MayBlockResolution {

  allDone = !Knowledge.experimental_l3_resolveVirtualFields

  def numDims = /*FIXME*/ Knowledge.dimensionality
  def stagDim = L3_GridUtil.faceToDims(name.replace("evalAt", ""))._1
  def faceDim = L3_GridUtil.faceToDims(name.replace("evalAt", ""))._2

  def fieldAccess() : L3_FieldAccess = {
    expression match {
      case fieldAccess : L3_FieldAccess => fieldAccess
      case other                        => Logger.error(s"$other in evaluate is not of type L3_FieldAccess")
    }
  }

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << offset.get
    out << " ( " << expression
    if ("default" != interpolation) out << ", " << interpolation
    out << " ) "
  }

  def getEffectiveOffset = {
    val effectiveOffset = offset.getOrElse(L3_ConstIndex(Array.fill(numDims)(0)))

    // take into account that right-hand interfaces can be targets too
    val faceOffset = L3_GridUtil.offsetForFace(name.replace("evalAt", ""))
    L3_GridUtil.offsetIndex(effectiveOffset, faceOffset, faceDim)
  }

  def resolve() = {
    expression match {
      case fieldAccess : L3_FieldAccess  => resolveForFieldAccess(fieldAccess, interpolation)
      case const : L3_ConstantExpression => const // no interpolation needed
      case variable : L3_VariableAccess  => variable // no interpolation needed
      case other                         => Logger.error(s"Evaluation is not supported yet for $other")
    }
  }

  def resolveForFieldAccess(fieldAccess : L3_FieldAccess, interpolation : String) : L3_Expression = {
    val field = fieldAccess.target
    val (stagDim, faceDim) = L3_GridUtil.faceToDims(name.replace("evalAt", ""))

    // construct index
    var index = L3_FieldIteratorAccess.fullIndex(field.numDimsGrid)
    index += getEffectiveOffset

    // take into account that right-hand interfaces can be targets too
    val faceOffset = L3_GridUtil.offsetForFace(name.replace("evalAt", ""))
    if (0 != faceOffset) L3_GridUtil.offsetIndex(index, faceOffset, faceDim)

    // placeholder to interpolation weights
    var a0 : (() => L3_Expression) = () => { L3_NullExpression }
    var a1 : (() => L3_Expression) = () => { L3_NullExpression }

    // placeholder for interpolation values
    def x0() = { val access = Duplicate(fieldAccess); access.offsetWith(getEffectiveOffset); access }
    def x1() = { val access = L3_GridUtil.offsetAccess(fieldAccess, -1, faceDim); access.offsetWith(getEffectiveOffset); access }

    // special handling for different localizations
    field.localization match {
      case L3_AtNode => // evaluation of node localized quantities is always ambiguous
        Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ L3_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")

      case L3_AtCellCenter => stagDim match {
        case None => // cell localized quantities are evaluated at (regular) cell interfaces -> use distance between cell center and face
          a0 = () => { 0.5 * L3_VF_CellWidthPerDim.access(level, faceDim, Duplicate(index)) }
          a1 = () => { 0.5 * L3_VF_CellWidthPerDim.access(level, faceDim, L3_GridUtil.offsetIndex(index, -1, faceDim)) }

        case Some(`faceDim`) => // cell localized quantities are evaluated at staggered cell interfaces -> no interpolation necessary
          val ret = L3_GridUtil.offsetAccess(fieldAccess, -1, faceDim)
          ret.offsetWith(getEffectiveOffset)
          return ret

        case Some(_) => // cell localized quantities are evaluated at staggered cell interface that align with regular cell interfaces -> ambiguous
          Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ L3_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")
      }

      case L3_AtFaceCenter(fd) => stagDim match {
        case None if fd == faceDim => // face localized quantities align with cell interfaces -> no interpolation necessary
          val ret = Duplicate(fieldAccess)
          ret.offsetWith(getEffectiveOffset)
          return ret

        case None => // face localized quantities are evaluated at cell interfaces that don't align with the face dimension -> ambiguous
          Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ L3_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")

        case Some(`fd`) => // face localization and control volume staggering match -> use distance between (stag) cell center and (stag) face
          a0 = () => 0.5
          a1 = () => 0.5

        case Some(_) => // face localization and control volume staggering don't match -> ambiguous
          Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ L3_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")
      }

      case other => Logger.error(s"Unsupported localization in evaluate: ${ other.name }")
    }

    // compile evaluation
    interpolation match {
      case "linear" | "default" => (a1() * x0() + a0() * x1()) / (a0() + a1())
      case "harmonicMean"       => ((a0() + a1()) * (x0() * x1())) / (a1() * x0() + a0() * x1())
      case _                    =>
        Logger.warn(s"Trying to use interpolation scheme $interpolation which is unknown - falling back to default scheme")
        resolveForFieldAccess(fieldAccess, "default")
    }
  }

  override def progress = ProgressLocation(L4_EvaluateOnGrid(name, level, expression.progress, interpolation, L3_ProgressOption(offset)(_.progress)))
}

/// L3_ResolveEvaluateOnGrid

object L3_ResolveEvaluateOnGrid extends DefaultStrategy("Resolve grid evaluations") {
  this += new Transformation("Resolve", {
    case evaluate : L3_EvaluateOnGrid if L3_MayBlockResolution.isDone(evaluate) => evaluate.resolve()
  })
}
