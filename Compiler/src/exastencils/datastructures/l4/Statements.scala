package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils._
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.field.l4.L4_FieldAccess
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.stencil.l4.L4_StencilFieldAccess

abstract class SpecialStatement /*TODO: think about an appropriate name*/ extends Node /*with L4_Progressable*/ with PrettyPrintable {
  def progress : Any
}

trait HasIdentifier {
  var identifier : Identifier
}

trait ExternalDeclarationStatement extends SpecialStatement

case class AssignmentStatement(var dest : Access, var src : L4_Expression, var op : String) extends L4_Statement {
  override def prettyprint(out : PpStream) = { out << dest << ' ' << op << ' ' << src << '\n' }

  override def progress : IR_Assignment = {
    IR_Assignment(dest.progress, src.progress, op)
  }
}

case class LoopOverPointsStatement(
    var field : Access,
    var region : Option[RegionSpecification],
    var seq : Boolean, // FIXME: seq HACK
    var condition : Option[L4_Expression],
    var startOffset : Option[L4_ExpressionIndex],
    var endOffset : Option[L4_ExpressionIndex],
    var increment : Option[L4_ExpressionIndex],
    var statements : List[L4_Statement],
    var reduction : Option[ReductionStatement],
    var preComms : List[CommunicateStatement],
    var postComms : List[CommunicateStatement]) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "loop over " << field << ' '
    if (region.isDefined) out << "only " << region.get
    if (seq) out << "sequentially "
    if (condition.isDefined) out << "where " << condition.get
    if (startOffset.isDefined) out << "starting " << startOffset.get << ' '
    if (endOffset.isDefined) out << "ending " << endOffset.get << ' '
    if (increment.isDefined) out << "stepping " << increment.get << ' '
    if (reduction.isDefined) out << "with " << reduction.get << ' '
    for (cs <- preComms) { out << "precomm " <<< (cs.targets, " ") << (if (cs.targets.isEmpty) "" else " of ") << cs.field << ' ' }
    for (cs <- postComms) { out << "postcomm " <<< (cs.targets, " ") << (if (cs.targets.isEmpty) "" else " of ") << cs.field << ' ' }
    out << "{\n" <<< statements << "}\n"
  }

  override def progress : IR_LoopOverPoints = {
    val resolvedField = field match {
      case access : L4_FieldAccess        => access.target.getProgressedObject
      case access : L4_StencilFieldAccess => access.target.getProgressedObject.field
      case _                              => Logger.error(s"Trying to loop over $field - has to be of type FieldAccess or StencilFieldAccess")
    }

    val numDims = resolvedField.fieldLayout.numDimsGrid
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

    val loop = IR_LoopOverPoints(resolvedField,
      if (region.isDefined) Some(region.get.progress) else None,
      seq,
      procStartOffset,
      procEndOffset,
      procIncrement,
      statements.map(_.progress).to[ListBuffer], // FIXME: .to[ListBuffer]
      preComms.map(_.progress).to[ListBuffer],
      postComms.map(_.progress).to[ListBuffer],
      if (reduction.isDefined) Some(reduction.get.progress) else None,
      if (condition.isDefined) Some(condition.get.progress) else None)

    loop.annotate("l4_fromDSL") // experimental annotation -> if successful and performance impacts are ok annotate all l4 statements
    loop
  }
}

case class LoopOverFragmentsStatement(var statements : List[L4_Statement], var reduction : Option[ReductionStatement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "loop over fragments "
    if (reduction.isDefined) out << "with " << reduction.get
    out << "{\n" <<< statements << "}\n"
  }

  override def progress : IR_LoopOverFragments = {
    new IR_LoopOverFragments(statements.map(s => s.progress).to[ListBuffer],
      if (reduction.isDefined) Some(reduction.get.progress) else None) with omp.OMP_PotentiallyParallel
  }
}

case class ColorWithStatement(var colors : List[L4_Expression], var loop : LoopOverPointsStatement) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "color with {\n"
    out <<< (colors, ",\n") << ",\n"
    out << loop
    out << "}\n"
  }

  override def progress : IR_Scope = {
    // TODO: think about extracting loop duplication to separate transformation
    var loops = colors.map(color => {
      var newLoop = Duplicate(loop)
      if (newLoop.condition.isDefined)
        newLoop.condition = Some(L4_AndAndExpression(newLoop.condition.get, color))
      else
        newLoop.condition = Some(color)
      newLoop
    })

    IR_Scope(loops.map(_.progress : IR_Statement).to[ListBuffer])
  }
}

case class ContractionSpecification(var posExt : L4_ConstIndex, var negExt : Option[L4_ConstIndex]) extends SpecialStatement {
  override def prettyprint(out : PpStream) : Unit = {
    out << posExt
    if (negExt.isDefined)
      out << ", " << negExt
  }

  override def progress : IR_ContractionSpecification = {
    IR_ContractionSpecification(posExt.progress, negExt.getOrElse(posExt).progress)
  }
}

case class RepeatTimesStatement(var number : Int,
    var iterator : Option[Access],
    var contraction : Option[ContractionSpecification],
    var statements : List[L4_Statement]) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "repeat " << number << " times"
    if (iterator.isDefined) out << " count " << iterator.get
    if (contraction.isDefined) out << " with contraction " << contraction.get
    out << " {\n" <<< statements << "}\n"
  }

  override def progress : IR_Statement = {
    if (contraction.isDefined)
    // FIXME: to[ListBuffer]
      return IR_ContractingLoop(number, iterator.map(i => i.progress), statements.map(s => s.progress).to[ListBuffer], contraction.get.progress)

    val (loopVar, begin) =
      if (iterator.isDefined) {
        val lv = iterator.get.progress
        (lv, IR_Assignment(lv, IR_IntegerConstant(0)))
      } else {
        val lv = "someRandomIndexVar" // FIXME: someRandomIndexVar
        (IR_StringLiteral(lv), IR_VariableDeclaration(IR_IntegerDatatype, lv, Some(IR_IntegerConstant(0))))
      }

    val ret = IR_ForLoop(
      begin,
      loopVar < IR_IntegerConstant(number),
      IR_Assignment(loopVar, IR_IntegerConstant(1), "+="),
      statements.map(s => s.progress).to[ListBuffer], // FIXME: to[ListBuffer]
      None)

    ret.annotate("numLoopIterations", number)

    ret
  }
}

case class ReductionStatement(var op : String, var target : String) extends SpecialStatement {
  override def prettyprint(out : PpStream) = out << "reduction ( " << op << " : " << target << " )"

  override def progress : IR_Reduction = {
    IR_Reduction(op, IR_VariableAccess(target, None))
  }
}

case class RegionSpecification(var region : String, var dir : L4_ConstIndex, var onlyOnBoundary : Boolean) extends SpecialStatement {
  override def prettyprint(out : PpStream) = out << region << ' ' << dir

  override def progress : IR_RegionSpecification = {
    IR_RegionSpecification(region, L4_ConstIndex(dir.indices ++ Array.fill(3 - dir.indices.length)(0)).progress, onlyOnBoundary)
  }
}

case class BreakStatement() extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "break\n"
  }

  override def progress : IR_Break = {
    IR_Break()
  }
}

case class AdvanceStatement(var field : Access) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "advance "
    field.prettyprint(out)
    out << '\n'
  }

  override def progress = {
    data.AdvanceSlotStatement(iv.CurrentSlot(field.asInstanceOf[L4_FieldAccess].progress.fieldSelection.field,
      IR_StringLiteral(IR_LoopOverFragments.defIt)))
  }
}

case class LeveledScopeStatement(var level : LevelSpecification, var statements : List[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << level << " {\n"
    statements.foreach(_.prettyprint(out))
    out << "\n}\n"
  }
  override def progress = {
    Logger.error("cannot progress LeveledScopeStatement to IR")
  }
}
