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

package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.communication.l4.L4_Communicate
import exastencils.field.l4.L4_FieldAccess
import exastencils.logger.Logger
import exastencils.operator.l4.L4_StencilFieldAccess
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.prettyprinting._

/// L4_RegionSpecification

case class L4_RegionSpecification(var region : String, var dir : L4_ConstIndex, var onlyOnBoundary : Boolean) extends L4_Node with L4_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = {
    out << region << ' ' << dir
    if (onlyOnBoundary) out << " on boundary"
  }

  override def progress = ProgressLocation(IR_RegionSpecification(region, dir.progress, onlyOnBoundary))
}

/// L4_LoopOverField

object L4_LoopOverField {
  def apply(field : L4_Access, body : L4_Statement*) =
    new L4_LoopOverField(field, None, false, None, None, None, None, body.to[ListBuffer], None, ListBuffer(), ListBuffer())

  def apply(field : L4_Access, modifiers : List[(String, Any)], body : List[L4_Statement]) = {
    val loop = new L4_LoopOverField(field, None, false, None, None, None, None, body.to[ListBuffer], None, ListBuffer(), ListBuffer())

    // apply loop modifiers
    modifiers.foreach { case (modName, modVal) =>
      modName match {
        case "only"         => loop.region = Some(modVal.asInstanceOf[L4_RegionSpecification])
        case "sequentially" => loop.seq = true
        case "where"        => loop.condition = Some(modVal.asInstanceOf[L4_Expression])
        case "starting"     => loop.startOffset = Some(modVal.asInstanceOf[L4_ExpressionIndex])
        case "ending"       => loop.endOffset = Some(modVal.asInstanceOf[L4_ExpressionIndex])
        case "stepping"     => loop.increment = Some(modVal.asInstanceOf[L4_ExpressionIndex])
        case "with"         => loop.reduction = Some(modVal.asInstanceOf[L4_Reduction])
        case "precomm"      => loop.preComms :+= modVal.asInstanceOf[L4_Communicate]
        case "postcomm"     => loop.postComms :+= modVal.asInstanceOf[L4_Communicate]
      }
    }

    loop
  }

  def apply(field : L4_Access, region : Option[L4_RegionSpecification], seq : Boolean, condition : Option[L4_Expression], startOffset : Option[L4_ExpressionIndex], endOffset : Option[L4_ExpressionIndex],
      increment : Option[L4_ExpressionIndex], statements : List[L4_Statement], reduction : Option[L4_Reduction], preComms : List[L4_Communicate], postComms : List[L4_Communicate]) =
    new L4_LoopOverField(field, region, seq, condition, startOffset, endOffset,
      increment, statements.to[ListBuffer], reduction, preComms.to[ListBuffer], postComms.to[ListBuffer])
}

// TODO: refactor -> less options/knowledge in loop nodes
case class L4_LoopOverField(
    var field : L4_Access,
    var region : Option[L4_RegionSpecification],
    var seq : Boolean, // FIXME: seq HACK
    var condition : Option[L4_Expression],
    var startOffset : Option[L4_ExpressionIndex],
    var endOffset : Option[L4_ExpressionIndex],
    var increment : Option[L4_ExpressionIndex],
    var body : ListBuffer[L4_Statement],
    var reduction : Option[L4_Reduction],
    var preComms : ListBuffer[L4_Communicate],
    var postComms : ListBuffer[L4_Communicate]) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "loop over " << field << ' '
    if (region.isDefined) out << "only " << region.get << ' '
    if (seq) out << "sequentially "
    if (condition.isDefined) out << "where " << condition.get << ' '
    if (startOffset.isDefined) out << "starting " << startOffset.get << ' '
    if (endOffset.isDefined) out << "ending " << endOffset.get << ' '
    if (increment.isDefined) out << "stepping " << increment.get << ' '
    if (reduction.isDefined) out << "with " << reduction.get << ' '
    for (cs <- preComms) { out << "precomm " <<< (cs.targets, " ") << (if (cs.targets.isEmpty) "" else " of ") << cs.field << ' ' }
    for (cs <- postComms) { out << "postcomm " <<< (cs.targets, " ") << (if (cs.targets.isEmpty) "" else " of ") << cs.field << ' ' }
    out << "{\n" <<< (body, "\n") << "\n}"
  }

  override def progress : IR_LoopOverPoints = ProgressLocation {
    val resolvedField = field match {
      case access : L4_FieldAccess        => access.target.getProgressedObj()
      case access : L4_StencilFieldAccess => access.target.getProgressedObj().field
      case _                              => Logger.error(s"Trying to loop over $field - has to be of type FieldAccess or StencilFieldAccess")
    }

    val numDims = resolvedField.layout.numDimsGrid
    val procStartOffset = IR_ExpressionIndex(Array.fill(numDims)(0))
    val procEndOffset = IR_ExpressionIndex(Array.fill(numDims)(0))
    val procIncrement = IR_ExpressionIndex(Array.fill(numDims)(1))
    if (startOffset.isDefined) {
      val newOffset = startOffset.get.progress
      for (i <- 0 until Math.min(numDims, newOffset.length)) procStartOffset(i) = newOffset(i)
    }
    if (endOffset.isDefined) {
      val newOffset = endOffset.get.progress
      for (i <- 0 until Math.min(numDims, newOffset.length)) procEndOffset(i) = newOffset(i)
    }
    if (increment.isDefined) {
      val newIncrement = increment.get.progress
      for (i <- 0 until Math.min(numDims, newIncrement.length)) procIncrement(i) = newIncrement(i)
    }

    // TODO: introduce L4_ParallelizationInfo
    val parallelization = IR_ParallelizationInfo()
    parallelization.potentiallyParallel = !seq
    parallelization.reduction = reduction.map(_.progress)

    val loop = IR_LoopOverPoints(resolvedField,
      if (region.isDefined) Some(region.get.progress) else None,
      procStartOffset,
      procEndOffset,
      procIncrement,
      body.map(_.progress),
      preComms.map(_.progress),
      postComms.map(_.progress),
      parallelization,
      L4_ProgressOption(condition)(_.progress))

    loop.annotate("l4_fromDSL") // experimental annotation -> if successful and performance impacts are ok annotate all l4 statements
    loop
  }
}
