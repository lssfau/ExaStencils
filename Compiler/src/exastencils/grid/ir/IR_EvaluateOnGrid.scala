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

package exastencils.grid.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FieldIteratorAccess
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// IR_EvaluateOnGrid

object IR_EvaluateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[IR_Expression], offset : Option[IR_ConstIndex]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      IR_RealConstant(0.0)
    } else {
      if (args.length > 2) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      args match {
        case ListBuffer(access : IR_Expression) =>
          new IR_EvaluateOnGrid(name, level, access, "default", offset)

        case ListBuffer(access : IR_Expression, interpolation : IR_StringConstant) =>
          new IR_EvaluateOnGrid(name, level, access, interpolation.value, offset)

        case _ =>
          Logger.warn(s"Arguments (${ args.map(_.prettyprint).mkString(", ") }) are currently not supported for function $name")
          args(0)
      }
    }
  }

  def apply(stagDim : Option[Int], faceDim : Int, level : Int, expression : IR_Expression, interpolation : String = "default") =
    new IR_EvaluateOnGrid(s"evalAt${ IR_GridUtil.dimsToFace(stagDim, faceDim) }", level, expression, interpolation, None)
}

case class IR_EvaluateOnGrid(
    var name : String,
    var level : Int,
    var expression : IR_Expression,
    var interpolation : String,
    var offset : Option[IR_ConstIndex]) extends IR_Expression with IR_CanBeOffset {

  override def datatype = expression.datatype

  def numDims = /*FIXME*/ Knowledge.dimensionality
  def stagDim = IR_GridUtil.faceToDims(name.replace("evalAt", ""))._1
  def faceDim = IR_GridUtil.faceToDims(name.replace("evalAt", ""))._2

  def fieldAccess() : IR_FieldLikeAccess = {
    expression match {
      case fieldAccess : IR_FieldLikeAccess => fieldAccess
      case other                            => Logger.error(s"$other in evaluate is not of type IR_FieldLikeAccess")
    }
  }

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << offset
    out << " ( " << expression
    if ("default" != interpolation) out << ", " << interpolation
    out << " ) "
  }

  def getEffectiveOffset = {
    val effectiveOffset = offset.getOrElse(IR_ConstIndex(Array.fill(numDims)(0)))

    // take into account that right-hand interfaces can be targets too
    val faceOffset = IR_GridUtil.offsetForFace(name.replace("evalAt", ""))
    IR_GridUtil.offsetIndex(effectiveOffset, faceOffset, faceDim)
  }

  def resolve() = {
    expression match {
      case fieldAccess : IR_FieldLikeAccess => resolveForFieldAccess(fieldAccess, interpolation)
      case const : IR_ConstantExpression    => const // no interpolation needed
      case variable : IR_VariableAccess     => variable // no interpolation needed
      case other                            => Logger.error(s"Evaluation is not supported yet for $other")
    }
  }

  def resolveForFieldAccess(fieldAccess : IR_FieldLikeAccess, interpolation : String) : IR_Expression = {
    val field = fieldAccess.field
    val (stagDim, faceDim) = IR_GridUtil.faceToDims(name.replace("evalAt", ""))

    // construct index
    var index = IR_FieldIteratorAccess.fullIndex(field.numDimsGrid)
    index += getEffectiveOffset

    // take into account that right-hand interfaces can be targets too
    val faceOffset = IR_GridUtil.offsetForFace(name.replace("evalAt", ""))
    if (0 != faceOffset) IR_GridUtil.offsetIndex(index, faceOffset, faceDim)

    // placeholder to interpolation weights
    var a0 : (() => IR_Expression) = () => { IR_NullExpression }
    var a1 : (() => IR_Expression) = () => { IR_NullExpression }

    // placeholder for interpolation values
    def x0() = { val access = Duplicate(fieldAccess); access.offsetWith(getEffectiveOffset); access }

    def x1() = { val access = IR_GridUtil.offsetAccess(fieldAccess, -1, faceDim); access.offsetWith(getEffectiveOffset); access }

    // special handling for different localizations
    field.localization match {
      case IR_AtNode => // evaluation of node localized quantities is always ambiguous
        Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ IR_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")

      case IR_AtCellCenter => stagDim match {
        case None => // cell localized quantities are evaluated at (regular) cell interfaces -> use distance between cell center and face
          a0 = () => { 0.5 * IR_VF_CellWidthPerDim.access(level, faceDim, Duplicate(index)) }
          a1 = () => { 0.5 * IR_VF_CellWidthPerDim.access(level, faceDim, IR_GridUtil.offsetIndex(index, -1, faceDim)) }

        case Some(`faceDim`) => // cell localized quantities are evaluated at staggered cell interfaces -> no interpolation necessary
          val ret = IR_GridUtil.offsetAccess(fieldAccess, -1, faceDim)
          ret.offsetWith(getEffectiveOffset)
          return ret

        case Some(_) => // cell localized quantities are evaluated at staggered cell interface that align with regular cell interfaces -> ambiguous
          Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ IR_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")
      }

      case IR_AtFaceCenter(fd) => stagDim match {
        case None if fd == faceDim => // face localized quantities align with cell interfaces -> no interpolation necessary
          val ret = Duplicate(fieldAccess)
          ret.offsetWith(getEffectiveOffset)
          return ret

        case None => // face localized quantities are evaluated at cell interfaces that don't align with the face dimension -> ambiguous
          Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ IR_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")

        case Some(`fd`) => // face localization and control volume staggering match -> use distance between (stag) cell center and (stag) face
          a0 = () => 0.5
          a1 = () => 0.5

        case Some(_) => // face localization and control volume staggering don't match -> ambiguous
          Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ IR_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")
      }
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
}

/// IR_ResolveEvaluateOnGrid

object IR_ResolveEvaluateOnGrid extends DefaultStrategy("Resolve grid evaluations") {
  this += new Transformation("Resolve", {
    case evaluate : IR_EvaluateOnGrid => evaluate.resolve()
  })
}
