package exastencils.grid.l4

import scala.collection.mutable._

import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_FieldIteratorAccess
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.l4.L4_FieldAccess
import exastencils.grid.ir._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_EvaluateOnGrid

object L4_EvaluateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[L4_Expression], offset : Option[L4_ConstIndex]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      L4_RealConstant(0.0)
    } else {
      if (args.length > 2) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      args match {
        case ListBuffer(access : L4_Expression) =>
          new L4_EvaluateOnGrid(name, level, access, "default", offset)

        case ListBuffer(access : L4_Expression, interpolation : L4_StringConstant) =>
          new L4_EvaluateOnGrid(name, level, access, interpolation.value, offset)

        case _ =>
          Logger.warn(s"Arguments (${ args.map(_.prettyprint).mkString(", ") }) are currently not supported for function $name")
          args(0)
      }
    }
  }

  def apply(stagDim : Option[Int], faceDim : Int, level : Int, expression : L4_Expression, interpolation : String = "default") =
    new L4_EvaluateOnGrid(s"evalAt${ L4_GridUtil.dimsToFace(stagDim, faceDim) }", level, expression, interpolation, None)
}

case class L4_EvaluateOnGrid(
    var name : String,
    var level : Int,
    var expression : L4_Expression,
    var interpolation : String,
    var offset : Option[L4_ConstIndex]) extends L4_Expression with L4_CanBeOffset with L4_MayBlockResolution {

  allDone = !Knowledge.experimental_l4_resolveVirtualFields

  def numDims = /*FIXME*/ Knowledge.dimensionality
  def stagDim = L4_GridUtil.faceToDims(name.replace("evalAt", ""))._1
  def faceDim = L4_GridUtil.faceToDims(name.replace("evalAt", ""))._2

  def fieldAccess() : L4_FieldAccess = {
    expression match {
      case fieldAccess : L4_FieldAccess => fieldAccess
      case other                        => Logger.error(s"$other in evaluate is not of type L4_FieldAccess")
    }
  }

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << offset
    out << " ( " << expression
    if ("default" != interpolation) out << ", " << interpolation
    out << " ) "
  }

  override def offsetWith(newOffset : L4_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }

  def getEffectiveOffset = {
    val effectiveOffset = offset.getOrElse(L4_ConstIndex(Array.fill(numDims)(0)))

    // take into account that right-hand interfaces can be targets too
    val faceOffset = L4_GridUtil.offsetForFace(name.replace("evalAt", ""))
    L4_GridUtil.offsetIndex(effectiveOffset, faceOffset, faceDim)
  }

  def resolve() = {
    expression match {
      case fieldAccess : L4_FieldAccess  => resolveForFieldAccess(fieldAccess, interpolation)
      case const : L4_ConstantExpression => const // no interpolation needed
      case other                         => Logger.error(s"Evaluation is not supported yet for $other")
    }
  }

  def resolveForFieldAccess(fieldAccess : L4_FieldAccess, interpolation : String) : L4_Expression = {
    val field = fieldAccess.target
    val (stagDim, faceDim) = L4_GridUtil.faceToDims(name.replace("evalAt", ""))

    // construct index
    var index = L4_FieldIteratorAccess.fullIndex(field.numDimsGrid)
    index += getEffectiveOffset

    // take into account that right-hand interfaces can be targets too
    val faceOffset = L4_GridUtil.offsetForFace(name.replace("evalAt", ""))
    if (0 != faceOffset) L4_GridUtil.offsetIndex(index, faceOffset, faceDim)

    // placeholder to interpolation weights
    var a0 : (() => L4_Expression) = () => { L4_NullExpression }
    var a1 : (() => L4_Expression) = () => { L4_NullExpression }

    // placeholder for interpolation values
    def x0() = { val access = Duplicate(fieldAccess); access.offsetWith(getEffectiveOffset); access }
    def x1() = { val access = L4_GridUtil.offsetAccess(fieldAccess, -1, faceDim); access.offsetWith(getEffectiveOffset); access }

    // special handling for different localizations
    field.localization match {
      case L4_AtNode => // evaluation of node localized quantities is always ambiguous
        Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ L4_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")

      case L4_AtCellCenter => stagDim match {
        case None => // cell localized quantities are evaluated at (regular) cell interfaces -> use distance between cell center and face
          a0 = () => { 0.5 * L4_VF_CellWidthPerDim.access(level, faceDim, Duplicate(index)) }
          a1 = () => { 0.5 * L4_VF_CellWidthPerDim.access(level, faceDim, L4_GridUtil.offsetIndex(index, -1, faceDim)) }

        case Some(`faceDim`) => // cell localized quantities are evaluated at staggered cell interfaces -> no interpolation necessary
          val ret = L4_GridUtil.offsetAccess(fieldAccess, -1, faceDim)
          ret.offsetWith(getEffectiveOffset)
          return ret

        case Some(_) => // cell localized quantities are evaluated at staggered cell interface that align with regular cell interfaces -> ambiguous
          Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ L4_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")
      }

      case L4_AtFaceCenter(fd) => stagDim match {
        case None if fd == faceDim => // face localized quantities align with cell interfaces -> no interpolation necessary
          val ret = Duplicate(fieldAccess)
          ret.offsetWith(getEffectiveOffset)
          return ret

        case None => // face localized quantities are evaluated at cell interfaces that don't align with the face dimension -> ambiguous
          Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ L4_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")

        case Some(`fd`) => // face localization and control volume staggering match -> use distance between (stag) cell center and (stag) face
          a0 = () => 0.5
          a1 = () => 0.5

        case Some(_) => // face localization and control volume staggering don't match -> ambiguous
          Logger.error(s"Trying to evaluate field ${ fieldAccess.prettyprint() } on face ${ L4_GridUtil.dimsToFace(stagDim, faceDim) }; this is not unique")
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

  override def progress = IR_EvaluateOnGrid(name, level, expression.progress, interpolation, L4_ProgressOption(offset)(_.progress))
}

/// L4_ResolveEvaluateOnGrid

object L4_ResolveEvaluateOnGrid extends DefaultStrategy("Resolve grid evaluations") {
  this += new Transformation("Resolve", {
    case evaluate : L4_EvaluateOnGrid if L4_MayBlockResolution.isDone(evaluate) => evaluate.resolve()
  })
}
