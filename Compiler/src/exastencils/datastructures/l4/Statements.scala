package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.data
import exastencils.datastructures._
import exastencils.knowledge
import exastencils.omp
import exastencils.prettyprinting._
import exastencils.util._

abstract class Statement extends Node with ProgressableToIr with PrettyPrintable {
  override def progressToIr : ir.Statement
}

abstract class SpecialStatement /*TODO: think about an appropriate name*/ extends Node with ProgressableToIr with PrettyPrintable {
  override def progressToIr : Any
}

trait HasIdentifier {
  var identifier : Identifier
}

trait ExternalDeclarationStatement extends SpecialStatement

case class DomainDeclarationStatement(var name : String, var lower : RealIndex, var upper : RealIndex, var index : Int = 0) extends SpecialStatement {
  override def prettyprint(out : PpStream) = { out << "Domain " << name << "< " << lower << " to " << upper << " >\n" }

  override def progressToIr : knowledge.Domain = {
    (lower, upper) match {
      case (l : RealIndex2D, u : RealIndex2D) => new knowledge.Domain(name, index, new AABB(l.x, u.x, l.y, u.y, 0.0, 0.0))
      case (l : RealIndex3D, u : RealIndex3D) => new knowledge.Domain(name, index, new AABB(l.x, u.x, l.y, u.y, l.z, u.z))
      case _                                  => new knowledge.Domain(name, index, new AABB())
    }
  }
}

case class StencilEntry(var offset : ExpressionIndex, var coeff : Expression) extends SpecialStatement {
  override def prettyprint(out : PpStream) = { out << offset << " => " << coeff }

  override def progressToIr : knowledge.StencilEntry = {
    var off = offset.progressToIr
    if (off(knowledge.Knowledge.dimensionality) == null) off(knowledge.Knowledge.dimensionality) = ir.IntegerConstant(0)
    knowledge.StencilEntry(off, coeff.progressToIr)
  }
}

case class StencilDeclarationStatement(var identifier : Identifier, var entries : List[StencilEntry]) extends SpecialStatement with HasIdentifier {
  override def prettyprint(out : PpStream) = {
    out << "Stencil " << identifier.name << '@' << identifier.asInstanceOf[LeveledIdentifier].level << " {\n"
    out <<< (entries, "\n") << '\n'
    out << "}\n"
  }

  override def progressToIr : knowledge.Stencil = {
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

  override def prettyprint(out : PpStream) = { out << "Globals {\n" <<< (values, "\n") <<< (variables, "\n") << "}\n" }

  override def progressToIr : ListBuffer[ir.VariableDeclarationStatement] = {
    variables.to[ListBuffer].map(e => e.progressToIr)
  }
}

case class VariableDeclarationStatement(var identifier : Identifier, var datatype : Datatype, var expression : Option[Expression] = None) extends Statement with HasIdentifier {
  override def prettyprint(out : PpStream) = {
    out << "Variable " << identifier << " : " << datatype
    if (expression.isDefined)
      out << " = " << expression.get
    out << '\n'
  }

  override def progressToIr : ir.VariableDeclarationStatement = {
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
  override def prettyprint(out : PpStream) = { out << "Value " << identifier << " : " << datatype << " = " << expression << '\n' }

  override def progressToIr : ir.Statement = ir.NullStatement
}

case class AssignmentStatement(var dest : Access, var src : Expression, var op : String) extends Statement {
  override def prettyprint(out : PpStream) = { out << dest << ' ' << op << ' ' << src << '\n' }

  override def progressToIr : ir.AssignmentStatement = {
    ir.AssignmentStatement(dest.progressToIr, src.progressToIr, op)
  }
}

case class LoopOverPointsStatement(
    var field : FieldAccess,
    var seq : Boolean, // FIXME: seq HACK
    var condition : Option[Expression],
    var startOffset : Option[ExpressionIndex],
    var endOffset : Option[ExpressionIndex],
    var increment : Option[ExpressionIndex],
    var statements : List[Statement],
    var reduction : Option[ReductionStatement]) extends Statement {

  override def prettyprint(out : PpStream) = {
    out << "loop over " << field << ' '
    if (seq) out << "sequentially "
    if (condition.isDefined) out << "where " << condition.get
    if (startOffset.isDefined) out << "starting " << startOffset.get << ' '
    if (endOffset.isDefined) out << "ending " << endOffset.get << ' '
    if (increment.isDefined) out << "stepping " << increment.get << ' '
    if (reduction.isDefined) out << "with " << reduction.get
    out << "{\n" <<< statements << "}\n"
  }

  override def progressToIr : ir.LoopOverPoints = {
    ir.LoopOverPoints(field.resolveField,
      seq,
      if (startOffset.isDefined) startOffset.get.progressToIr else new ir.MultiIndex(Array.fill(knowledge.Knowledge.dimensionality)(0)),
      if (endOffset.isDefined) endOffset.get.progressToIr else new ir.MultiIndex(Array.fill(knowledge.Knowledge.dimensionality)(0)),
      if (increment.isDefined) increment.get.progressToIr else new ir.MultiIndex(Array.fill(knowledge.Knowledge.dimensionality)(1)),
      statements.map(s => s.progressToIr).to[ListBuffer], // FIXME: .to[ListBuffer]
      if (reduction.isDefined) Some(reduction.get.progressToIr) else None,
      if (condition.isDefined) Some(condition.get.progressToIr) else None)
  }
}

case class LoopOverFragmentsStatement(var statements : List[Statement], var reduction : Option[ReductionStatement]) extends Statement {
  override def prettyprint(out : PpStream) = {
    out << "loop over fragments "
    if (reduction.isDefined) out << "with " << reduction.get
    out << "{\n" <<< statements << "}\n"
  }

  override def progressToIr : ir.LoopOverFragments = {
    new ir.LoopOverFragments(statements.map(s => s.progressToIr).to[ListBuffer],
      if (reduction.isDefined) Some(reduction.get.progressToIr) else None) with omp.OMP_PotentiallyParallel
  }
}

case class FunctionStatement(var identifier : Identifier,
    var returntype : Datatype,
    var arguments : List[Variable],
    var statements : List[Statement]) extends Statement with HasIdentifier {

  override def prettyprint(out : PpStream) = {
    out << "Function " << identifier << " ("
    if (!arguments.isEmpty) {
      for (arg <- arguments) { out << arg.identifier << " : " << arg.datatype << ", " }
      out.removeLast(2)
    }
    out << " )" << " : " << returntype << " {\n"
    out <<< statements
    out << "}\n"
  }

  override def progressToIr : ir.AbstractFunctionStatement = {
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

  override def prettyprint(out : PpStream) = {
    out << "repeat " << number << " times"
    if (iterator.isDefined) out << " count " << iterator.get
    if (contraction) out << " with contraction"
    out << " {\n" <<< statements << "}\n"
  }

  override def progressToIr : ir.Statement = {
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
  override def prettyprint(out : PpStream) = { out << "repeat until " << comparison << "{\n" <<< statements << "}\n" }

  override def progressToIr : ir.WhileLoopStatement = {
    ir.WhileLoopStatement(ir.UnaryExpression(ir.UnaryOperators.Not, comparison.progressToIr), statements.map(s => s.progressToIr).to[ListBuffer])
  }
}

case class ReductionStatement(var op : String, var target : String) extends SpecialStatement {
  override def prettyprint(out : PpStream) = out << "reduction ( " << op << " : " << target << " )"

  override def progressToIr : ir.Reduction = {
    ir.Reduction(op, ir.StringConstant(target))
  }
}

case class FunctionCallStatement(var call : FunctionCallExpression) extends Statement {
  override def prettyprint(out : PpStream) = { out << call << '\n' }

  override def progressToIr : ir.ExpressionStatement = {
    ir.ExpressionStatement(call.progressToIr)
  }
}

case class ReturnStatement(var expr : Option[Expression]) extends Statement {
  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined) out << ' ' << expr.get.prettyprint()
    out << '\n'
  }

  override def progressToIr : ir.ReturnStatement = {
    ir.ReturnStatement(expr.map(_.progressToIr))
  }
}

case class BreakStatement() extends Statement {
  override def prettyprint(out : PpStream) = {
    out << "break\n"
  }

  override def progressToIr : ir.BreakStatement = {
    ir.BreakStatement()
  }
}

case class ConditionalStatement(var expression : Expression, var statements : List[Statement], var elsestatements : List[Statement]) extends Statement {
  override def prettyprint(out : PpStream) = {
    out << "if ( " << expression << " )" << " {\n" <<< statements << '}'
    if (!elsestatements.isEmpty) out << " else {\n" <<< elsestatements << '}'
    out << '\n'
  }

  override def progressToIr : ir.ConditionStatement = {
    new ir.ConditionStatement(expression.progressToIr, statements.map(s => s.progressToIr).to[ListBuffer], elsestatements.map(s => s.progressToIr).to[ListBuffer])
  }
}

case class AdvanceStatement(var field : Access) extends Statement {
  override def prettyprint(out : PpStream) = {
    out << "advance "
    field.prettyprint(out)
    out << '\n'
  }

  override def progressToIr = {
    data.AdvanceSlotStatement(ir.iv.CurrentSlot(field.asInstanceOf[FieldAccess].progressToIr.fieldSelection.field,
      ir.StringConstant(ir.LoopOverFragments.defIt)))
  }
}