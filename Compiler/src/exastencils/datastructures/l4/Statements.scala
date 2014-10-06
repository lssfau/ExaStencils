package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.globals._
import exastencils.knowledge
import exastencils.omp
import exastencils.util._

abstract class Statement extends Node with ProgressableToIr {
  def progressToIr : ir.Statement
}

abstract class SpecialStatement /*TODO: think about an appropriate name*/ extends Node with ProgressableToIr {
  def progressToIr : Any
}

trait HasIdentifier {
  var identifier : Identifier
}

case class DomainDeclarationStatement(var name : String, var lower : RealIndex, var upper : RealIndex, var index : Int = 0) extends SpecialStatement {
  def progressToIr : knowledge.Domain = {
    (lower, upper) match {
      case (l : RealIndex2D, u : RealIndex2D) => new knowledge.Domain(name, index, new AABB(l.x, u.x, l.y, u.y, 0.0, 0.0))
      case (l : RealIndex3D, u : RealIndex3D) => new knowledge.Domain(name, index, new AABB(l.x, u.x, l.y, u.y, l.z, u.z))
      case _                                  => new knowledge.Domain(name, index, new AABB())
    }
  }
}

case class IterationSetDeclarationStatement(var identifier : Identifier,
    var begin : ExpressionIndex, var end : Option[ExpressionIndex],
    var increment : Option[ExpressionIndex],
    var condition : Option[Expression]) extends SpecialStatement {
  def progressToIr : knowledge.IterationSet = {
    knowledge.IterationSet(identifier.asInstanceOf[BasicIdentifier].progressToIr.value,
      begin.progressToIr,
      if (end.isDefined) end.get.progressToIr else begin.progressToIr,
      (if (increment.isDefined) increment.get.progressToIr else new ir.MultiIndex(Array.fill(knowledge.Knowledge.dimensionality)(1))),
      (if (condition.isDefined) Some(condition.get.progressToIr) else None))
  }
}

case class StencilEntry(var offset : ExpressionIndex, var weight : Expression) extends SpecialStatement {
  def progressToIr : knowledge.StencilEntry = {
    var off = offset.progressToIr
    if (off(knowledge.Knowledge.dimensionality) == null) off(knowledge.Knowledge.dimensionality) = ir.IntegerConstant(0)
    knowledge.StencilEntry(off, weight.progressToIr)
  }
}

case class StencilDeclarationStatement(var identifier : Identifier,
    var entries : List[StencilEntry]) extends SpecialStatement with HasIdentifier {
  def progressToIr : knowledge.Stencil = {
    knowledge.Stencil(identifier.name, identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level, entries.map(e => e.progressToIr).to[ListBuffer])
  }
}

case class GlobalDeclarationStatement(var values : List[ValueDeclarationStatement], var variables : List[VariableDeclarationStatement]) extends SpecialStatement {
  def this(entries : List[Statement]) = this(entries.filter(_ match {
    case x : ValueDeclarationStatement => true
    case _                             => false
  }).asInstanceOf[List[ValueDeclarationStatement]],
    entries.filter(_ match {
      case x : VariableDeclarationStatement => true
      case _                                => false
    }).asInstanceOf[List[VariableDeclarationStatement]])

  def progressToIr : Globals = {
    new Globals(variables.to[ListBuffer].map(e => e.progressToIr))
  }
}

case class VariableDeclarationStatement(var identifier : Identifier, var datatype : Datatype, var expression : Option[Expression] = None) extends Statement with HasIdentifier {
  def progressToIr : ir.VariableDeclarationStatement = {
    ir.VariableDeclarationStatement(datatype.progressToIr,
      identifier.progressToIr.asInstanceOf[ir.StringConstant].value,
      if (expression.isDefined) Some(expression.get.progressToIr) else None)
  }
}

case class ValueDeclarationStatement(var identifier : Identifier, var datatype : Datatype, var expression : Expression) extends Statement with HasIdentifier {
  //  def progressToIr : ir.ValueDeclarationStatement = {
  //    ir.ValueDeclarationStatement(datatype.progressToIr,
  //      identifier.progressToIr.asInstanceOf[ir.StringConstant].value,
  //      expression.get.progressToIr
  //  }
  def progressToIr : ir.Statement = ir.NullStatement
}

case class AssignmentStatement(var dest : Access, var src : Expression, var op : String) extends Statement {
  def progressToIr : ir.AssignmentStatement = {
    ir.AssignmentStatement(dest.progressToIr, src.progressToIr, op)
  }
}

case class LoopOverPointsStatement(
    var field : FieldAccess, var condition : Option[Expression],
    var startOffset : Option[ExpressionIndex],
    var endOffset : Option[ExpressionIndex],
    var increment : Option[ExpressionIndex],
    var statements : List[Statement],
    var reduction : Option[ReductionStatement]) extends Statement {
  def progressToIr : ir.LoopOverPoints = {
    ir.LoopOverPoints(field.resolveField,
      if (startOffset.isDefined) startOffset.get.progressToIr else new ir.MultiIndex(Array.fill(knowledge.Knowledge.dimensionality)(0)),
      if (endOffset.isDefined) endOffset.get.progressToIr else new ir.MultiIndex(Array.fill(knowledge.Knowledge.dimensionality)(0)),
      if (increment.isDefined) increment.get.progressToIr else new ir.MultiIndex(Array.fill(knowledge.Knowledge.dimensionality)(1)),
      statements.map(s => s.progressToIr).to[ListBuffer], // FIXME: .to[ListBuffer]
      if (reduction.isDefined) Some(reduction.get.progressToIr) else None,
      if (condition.isDefined) Some(condition.get.progressToIr) else None)
  }
}

case class LoopOverFragmentsStatement(var statements : List[Statement], var reduction : Option[ReductionStatement]) extends Statement {
  def progressToIr : ir.LoopOverFragments = {
    new ir.LoopOverFragments(statements.map(s => s.progressToIr).to[ListBuffer],
      if (reduction.isDefined) Some(reduction.get.progressToIr) else None) with omp.OMP_PotentiallyParallel
  }
}

case class FunctionStatement(var identifier : Identifier,
    var returntype : Datatype,
    var arguments : List[Variable],
    var statements : List[Statement]) extends Statement with HasIdentifier {
  def progressToIr : ir.AbstractFunctionStatement = {
    ir.FunctionStatement(
      returntype.progressToIr,
      identifier.progressToIr.asInstanceOf[ir.StringConstant].value,
      arguments.map(s => s.progressToIr).to[ListBuffer], // FIXME: .to[ListBuffer] 
      statements.map(s => s.progressToIr).to[ListBuffer]) // FIXME: .to[ListBuffer]
  }
}

case class RepeatUpStatement(var number : Int,
    var iterator : Option[Access],
    var contraction : Boolean,
    var statements : List[Statement]) extends Statement {

  def progressToIr : ir.Statement = {
    if (contraction)
      // FIXME: to[ListBuffer]
      return new ir.ContractingLoop(number, iterator.map(i => i.progressToIr), statements.map(s => s.progressToIr).to[ListBuffer])

    val (loopVar, begin) =
      if (iterator.isDefined) {
        val lv = iterator.get.progressToIr
        (lv, ir.AssignmentStatement(lv, ir.IntegerConstant(0)))
      } else {
        val lv = "someRandomIndexVar" // FIXME: someRandomIndexVar
        (ir.StringConstant(lv), ir.VariableDeclarationStatement(ir.IntegerDatatype(), lv, Some(ir.IntegerConstant(0))))
      }

    return ir.ForLoopStatement(
      begin,
      loopVar < ir.IntegerConstant(number),
      ir.AssignmentStatement(loopVar, ir.IntegerConstant(1), "+="),
      statements.map(s => s.progressToIr).to[ListBuffer], // FIXME: to[ListBuffer]
      None)
  }
}

case class RepeatUntilStatement(var comparison : BooleanExpression, var statements : List[Statement]) extends Statement {
  def progressToIr : ir.WhileLoopStatement = {
    ir.WhileLoopStatement(ir.UnaryExpression(ir.UnaryOperators.Not, comparison.progressToIr), statements.map(s => s.progressToIr).to[ListBuffer])
  }
}

case class ReductionStatement(var op : String, var target : String) extends SpecialStatement {
  def progressToIr : ir.Reduction = {
    ir.Reduction(op, ir.StringConstant(target))
  }
}

case class FunctionCallStatement(var call : FunctionCallExpression) extends Statement {
  def progressToIr : ir.ExpressionStatement = {
    ir.ExpressionStatement(call.progressToIr)
  }
}

case class ConditionalStatement(var expression : Expression, var statements : List[Statement]) extends Statement {
  def progressToIr : ir.ConditionStatement = {
    new ir.ConditionStatement(expression.progressToIr, statements.map(s => s.progressToIr).to[ListBuffer])
  }
}

trait ExternalDeclarationStatement extends SpecialStatement
