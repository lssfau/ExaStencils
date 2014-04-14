package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._

abstract class Statement extends Node with ProgressableToIr {
  def progressToIr : ir.Statement
}

abstract class SpecialStatement /*TODO: think about an appropriate name*/ extends Node with ProgressableToIr {
  def progressToIr : Node
}

case class DomainDeclarationStatement(var name : String) extends SpecialStatement {
  def progressToIr : ir.Expression = "FIXME: implement"
}

case class FieldDeclarationStatement(var name : String, var datatype : Datatype, var offset : Index, var level : Option[LevelSpecification]) extends SpecialStatement {
  var communicate = false
  var ghostlayers = 0
  var padding = 0
  var slots = 1
  var bcDir0 = false
  def set(t : TempOption) { // FIXME hack
    t.key match {
      case "communicate" => communicate = t.value.toBoolean
      case "ghostlayers" => ghostlayers = t.value.toInt
      case "padding"     => padding = t.value.toInt
      case "slots"       => slots = t.value.toInt
      case "bcDir"       => bcDir0 = t.value.toBoolean
      case _             => exastencils.core.ERROR(s"Unknown option ${t.key} = ${t.value}")
    }
  }

  def progressToIr : Field = {
    val layout = DimArray().map(dim => new FieldLayoutPerDim(if (0 == dim) 1 else 0, Knowledge.data_numGhostLayers, 1, ((Knowledge.domain_fragLengthPerDim(dim) * (1 << level.get.asInstanceOf[SingleLevelSpecification].level)) + 1) - 2 /*dup*/ , 1, Knowledge.data_numGhostLayers, 0)) ++
      (Knowledge.dimensionality until 3).toArray.map(dim => new FieldLayoutPerDim(0, 0, 0, 1, 0, 0, 0))
    new Field(
      name,
      0, // FIXME: domain
      name.substring(0, 3).toLowerCase + "Data", // HACK
      "double", // FIXME: datatype,
      layout, // FIXME: get this info from the DSL
      level.get.asInstanceOf[SingleLevelSpecification].level,
      slots,
      new ir.MultiIndex(layout.map(l => l.idxDupLeftBegin)), // FIXME: offset
      bcDir0);
  }
}

case class IterationSetDeclarationStatement(var identifier : Identifier, var begin : Index, var end : Index, var increment : Option[Index]) extends SpecialStatement {
  def progressToIr : IterationSet = {
    IterationSet(identifier.asInstanceOf[BasicIdentifier].progressToIr.value,
      begin.progressToIr,
      end.progressToIr,
      (if (increment.isDefined) increment.get.progressToIr else new ir.MultiIndex(Array.fill(Knowledge.dimensionality)(1))))
  }
}

case class VariableDeclarationStatement(var identifier : Identifier, var datatype : Datatype, var expression : Option[Expression] = None) extends Statement {
  def progressToIr : ir.VariableDeclarationStatement = {
    ir.VariableDeclarationStatement(
      ir.VariableAccess(identifier.progressToIr.asInstanceOf[ir.StringConstant].value, Some(datatype.progressToIr)),
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
    LoopOverDomain(iterationSet,
      field.name,
      field.level.asInstanceOf[SingleLevelSpecification].level,
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

case class ReductionStatement(var op : String, var target : Identifier) extends SpecialStatement {
  def progressToIr : ir.Reduction = {
    ir.Reduction(ir.BinaryOperators.str2op(op), target.progressToIr)
  }
}

case class FunctionCallStatement(var identifier : Identifier, var arguments : List[Expression]) extends Statement {
  def progressToIr : ir.Statement /*FIXME: FunctionCallExpression*/ = {
    ir.FunctionCallExpression(identifier.progressToIr.asInstanceOf[ir.StringConstant].value,
      arguments.map(s => s.progressToIr).to[ListBuffer])
  }
}

case class ConditionalStatement(var expression : BooleanExpression, var statements : List[Statement]) extends Statement {
  def progressToIr = "FIXME: implement"
}

