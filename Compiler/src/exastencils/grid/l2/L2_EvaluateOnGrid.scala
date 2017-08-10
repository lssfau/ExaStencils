package exastencils.grid.l2

import scala.collection.mutable._

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2.{ L2_ConstantExpression, _ }
import exastencils.baseExt.l2.L2_FieldIteratorAccess
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.l2.L2_FieldAccess
import exastencils.grid.l3._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_EvaluateOnGrid

object L2_EvaluateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[L2_Expression], offset : Option[L2_ConstIndex]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      L2_RealConstant(0.0)
    } else {
      if (args.length > 2) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      args match {
        case ListBuffer(access : L2_Expression) =>
          new L2_EvaluateOnGrid(name, level, access, "default", offset)

        case ListBuffer(access : L2_Expression, interpolation : L2_StringConstant) =>
          new L2_EvaluateOnGrid(name, level, access, interpolation.value, offset)

        case _ =>
          Logger.warn(s"Arguments (${ args.map(_.prettyprint).mkString(", ") }) are currently not supported for function $name")
          args(0)
      }
    }
  }

  def apply(stagDim : Option[Int], faceDim : Int, level : Int, expression : L2_Expression, interpolation : String = "default") =
    new L2_EvaluateOnGrid(s"evaluateAt${ L2_GridUtil.dimsToFace(stagDim, faceDim) }", level, expression, interpolation, None)
}

case class L2_EvaluateOnGrid(
    var name : String,
    var level : Int,
    var expression : L2_Expression,
    var interpolation : String,
    var offset : Option[L2_ConstIndex]) extends L2_Expression with L2_CanBeOffset with L2_MayBlockResolution {

  allDone = !Knowledge.experimental_l2_resolveVirtualFields

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << offset
    out << " ( " << expression
    if ("default" != interpolation) out << ", " << interpolation
    out << " ) "
  }

  override def offsetWith(newOffset : L2_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }

  def resolve() = {
    expression match {
      case fieldAccess : L2_FieldAccess  => resolveForFieldAccess(fieldAccess, interpolation)
      case const : L2_ConstantExpression => const // no interpolation needed
      case other                         => Logger.error(s"Evaluation is not supported yet for $other")
    }
  }

  def resolveForFieldAccess(fieldAccess : L2_FieldAccess, interpolation : String) : L2_Expression = {
    val field = fieldAccess.target
    val (stagDim, faceDim) = L2_GridUtil.faceToDims(name)

    // construct index
    var index = L2_FieldIteratorAccess.fullIndex(field.numDimsGrid)
    if (offset.isDefined)
      index += offset.get

    // take into account that right-hand interfaces can be targets too
    val faceOffset = L2_GridUtil.offsetForFace(name)
    if (0 != faceOffset) L2_GridUtil.offsetIndex(index, faceOffset, faceDim)

    // placeholder to interpolation weights
    var a0 : (() => L2_Expression) = () => { L2_NullExpression }
    var a1 : (() => L2_Expression) = () => { L2_NullExpression }

    // placeholder for interpolation values
    def x0 = Duplicate(fieldAccess)
    def x1 = L2_GridUtil.offsetAccess(fieldAccess, -1, faceDim)

    // special handling for different localizations
    field.localization match {
      case L2_AtNode => // evaluation of node localized quantities is always ambiguous
        Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ L2_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")

      case L2_AtCellCenter => stagDim match {
        case None => // cell localized quantities are evaluated at (regular) cell interfaces -> use distance between cell center and face
          a0 = () => { 0.5 * L2_VF_CellWidthPerDim.access(level, faceDim, Duplicate(index)) }
          a1 = () => { 0.5 * L2_VF_CellWidthPerDim.access(level, faceDim, L2_GridUtil.offsetIndex(index, -1, faceDim)) }

        case Some(`faceDim`) => // cell localized quantities are evaluated at staggered cell interfaces -> no interpolation necessary
          return L2_GridUtil.offsetAccess(fieldAccess, -1, faceDim)

        case Some(_) => // cell localized quantities are evaluated at staggered cell interface that align with regular cell interfaces -> ambiguous
          Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ L2_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")
      }

      case L2_AtFaceCenter(fd) => stagDim match {
        case None if fd == faceDim => // face localized quantities align with cell interfaces -> no interpolation necessary
          return fieldAccess

        case None => // face localized quantities are evaluated at cell interfaces that don't align with the face dimension -> ambiguous
          Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ L2_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")

        case Some(`fd`) => // face localization and control volume staggering match -> use distance between (stag) cell center and (stag) face
          a0 = () => { 0.5 * L2_VF_StagCellWidthPerDim.access(level, stagDim.get, faceDim, Duplicate(index)) }
          a1 = () => { 0.5 * L2_VF_StagCellWidthPerDim.access(level, stagDim.get, faceDim, L2_GridUtil.offsetIndex(index, -1, faceDim)) }

        case Some(_) => // face localization and control volume staggering don't match -> ambiguous
          Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ L2_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")
      }
    }

    // compile evaluation
    interpolation match {
      case "linear" | "default" => (a1() * x0 + a0() * x1) / (a0() + a1())
      case "harmonicMean"       => ((a0() + a1()) * (x0 * x1)) / (a1() * x0 + a0() * x1)
      case _                    =>
        Logger.warn(s"Trying to use interpolation scheme $interpolation which is unknown - falling back to default scheme")
        resolveForFieldAccess(fieldAccess, "default")
    }
  }

  override def progress = L3_EvaluateOnGrid(name, level, expression.progress, interpolation, L2_ProgressOption(offset)(_.progress))
}

/// L2_ResolveEvaluateOnGrid

object L2_ResolveEvaluateOnGrid extends DefaultStrategy("Resolve grid evaluations") {
  this += new Transformation("Resolve", {
    case evaluate : L2_EvaluateOnGrid if L2_MayBlockResolution.isDone(evaluate) => evaluate.resolve()
  })
}
