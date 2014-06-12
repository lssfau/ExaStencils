package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.knowledge
import exastencils.knowledge.Knowledge
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives
import exastencils.primitives._
import exastencils.languageprocessing.l4.ProgressToIr
import exastencils.util._
import exastencils.globals._
import exastencils.knowledge.IterationSetCollection

abstract class Statement extends Node with ProgressableToIr {
  def progressToIr : ir.Statement
}

abstract class SpecialStatement /*TODO: think about an appropriate name*/ extends Node with ProgressableToIr {
  def progressToIr : Any
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

case class IterationSetDeclarationStatement(var identifier : Identifier, var begin : ExpressionIndex, var end : ExpressionIndex, var increment : Option[ExpressionIndex], var condition : Option[BooleanExpression]) extends SpecialStatement {
  def progressToIr : knowledge.IterationSet = {
    knowledge.IterationSet(identifier.asInstanceOf[BasicIdentifier].progressToIr.value,
      begin.progressToIr,
      end.progressToIr,
      (if (increment.isDefined) increment.get.progressToIr else new ir.MultiIndex(Array.fill(Knowledge.dimensionality)(1))),
      (if (condition.isDefined) Some(condition.get.progressToIr) else None))
  }
}

case class StencilEntry(var offset : ExpressionIndex, var weight : Expression) extends SpecialStatement {
  def progressToIr : knowledge.StencilEntry = {
    var off = offset.progressToIr
    if (off(Knowledge.dimensionality).isInstanceOf[ir.NullExpression]) off(Knowledge.dimensionality) = 0
    knowledge.StencilEntry(off, weight.progressToIr)
  }
}

case class StencilDeclarationStatement(var name : String, var entries : List[StencilEntry], var level : Option[LevelSpecification]) extends SpecialStatement {
  def progressToIr : knowledge.Stencil = {
    knowledge.Stencil(name, level.get.asInstanceOf[SingleLevelSpecification].level, entries.map(e => e.progressToIr).to[ListBuffer])
  }
}

case class GlobalDeclarationStatement(var entries : List[VariableDeclarationStatement]) extends SpecialStatement {
  def progressToIr : Globals = {
    new Globals(entries.to[ListBuffer].map(e => e.progressToIr))
  }
}

case class VariableDeclarationStatement(var identifier : Identifier, var datatype : Datatype, var expression : Option[Expression] = None) extends Statement {
  def progressToIr : ir.VariableDeclarationStatement = {
    ir.VariableDeclarationStatement(datatype.progressToIr,
      identifier.progressToIr.asInstanceOf[ir.StringConstant].value,
      if (expression.isDefined) Some(expression.get.progressToIr) else None)
  }
}

case class AssignmentStatement(var identifier : Identifier, var expression : Expression) extends Statement {
  def progressToIr : ir.AssignmentStatement = {
    ir.AssignmentStatement(identifier.progressToIr, expression.progressToIr)
  }
}

case class LoopOverDomainStatement(var iterationSet : String, var field : FieldIdentifier, var statements : List[Statement], var reduction : Option[ReductionStatement]) extends Statement {
  def progressToIr : LoopOverDomain = {
    LoopOverDomain(IterationSetCollection.getIterationSetByIdentifier(iterationSet).get,
      field.resolveField,
      statements.map(s => s.progressToIr).to[ListBuffer], // FIXME: .to[ListBuffer]
      if (reduction.isDefined) Some(reduction.get.progressToIr) else None)
  }
}

case class FunctionStatement(var identifier : Identifier, var returntype : Datatype, var arguments : List[Variable], var statements : List[Statement]) extends Statement {
  def progressToIr : ir.AbstractFunctionStatement = {
    ir.FunctionStatement(
      returntype.progressToIr,
      identifier.progressToIr.asInstanceOf[ir.StringConstant].value,
      arguments.map(s => s.progressToIr).to[ListBuffer], // FIXME: .to[ListBuffer] 
      statements.map(s => s.progressToIr).to[ListBuffer]) // FIXME: .to[ListBuffer]
  }
}

case class RepeatUpStatement(var number : Int, var statements : List[Statement]) extends Statement {
  def progressToIr : ir.ForLoopStatement = {
    ir.ForLoopStatement( // FIXME: de-stringify
      "unsigned int someRandomIndexVar = 0", // FIXME: someRandomIndexVar
      ("someRandomIndexVar" : ir.Expression) < number,
      "++someRandomIndexVar",
      statements.map(s => s.progressToIr).to[ListBuffer]) // FIXME: to[ListBuffer]
  }
}

case class RepeatUntilStatement(var comparison : BooleanExpression, var statements : List[Statement]) extends Statement {
  def progressToIr = "FIXME: implement"
}

case class ReductionStatement(var op : String, var target : String) extends SpecialStatement {
  def progressToIr : ir.Reduction = {
    ir.Reduction(ir.BinaryOperators.str2op(op), target)
  }
}

case class FunctionCallStatement(var identifier : Identifier, var arguments : List[Expression]) extends Statement {
  def progressToIr : ir.ExpressionStatement = {
    ir.ExpressionStatement(ir.FunctionCallExpression(identifier.progressToIr.asInstanceOf[ir.StringConstant].value,
      arguments.map(s => s.progressToIr).to[ListBuffer]))
  }
}

case class ConditionalStatement(var expression : BooleanExpression, var statements : List[Statement]) extends Statement {
  def progressToIr : ir.ConditionStatement = {
    new ir.ConditionStatement(expression.progressToIr, statements.map(s => s.progressToIr).to[ListBuffer])
  }
}

case class CommunicateStatement(var identifier : Identifier) extends Statement {
  def progressToIr : primitives.CommunicateStatement = {
    primitives.CommunicateStatement(identifier.asInstanceOf[FieldIdentifier].name,
      identifier.asInstanceOf[FieldIdentifier].level.asInstanceOf[SingleLevelSpecification].level)
  }
}

trait ExternalDeclarationStatement extends SpecialStatement
