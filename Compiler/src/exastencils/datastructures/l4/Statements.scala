package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.data
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.domain._
import exastencils.knowledge
import exastencils.logger._
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

case class DomainDeclarationStatement(var name : String, var lower : Any, var upper : Any, var index : Int = 0) extends SpecialStatement {
  override def prettyprint(out : PpStream) = {
    (lower, upper) match {
      case (null, null)                   => out << s"Domain = fromFile($name) \n"
      case (l : RealIndex, u : RealIndex) => out << "Domain " << name << "< " << l << " to " << u << " >\n"
      case (lo : List[_], up : List[_]) => {
        (lo.head, up.head) match {
          case (_ : RealIndex, _ : RealIndex) => {
            val sep = lo.map(m => ", ").dropRight(1) :+ " >\n"
            out << "Domain " << name << "< "
            for (i <- lo.indices) { out << lo(i) << " to " << up(i) << sep(i) }
            //out << "Domain " << name << "< " << l(0) << " to " << u(0) << ", " << l(1) << " to " << u(1) << " ," << l(2) << " to " << u(2) << " >\n"
          }
        }
      }
    }
  }
  override def progressToIr : knowledge.Domain = {
    (lower, upper) match {
      case (null, null) => {
        new knowledge.FileInputGlobalDomain("global", index, DomainFileHeader.domainIdentifier.zipWithIndex.map {
          case (identifier, index) => new knowledge.FileInputDomain(identifier, index, new FileInputDomainShape(identifier))
        }.toList)
      }
      case (lo : List[_], up : List[_]) =>
        {
          (lo.head, up.head) match {
            case (_ : RealIndex2D, _ : RealIndex2D) => {
              val rectUnionDomains : List[RectangularDomainShape] =
                (lo.zip(up)).map {
                  case (li : RealIndex2D, ui : RealIndex2D) =>
                    new RectangularDomainShape(new AABB(li.x, ui.x, li.y, ui.y, 0.0, 0.0))
                }
              new knowledge.ShapedDomain(name, index, new ShapedDomainShape(rectUnionDomains))
            }
          }
        }
      case (l : RealIndex2D, u : RealIndex2D) => new knowledge.RectangularDomain(name, index, new RectangularDomainShape(new AABB(l.x, u.x, l.y, u.y, 0, 0)))
      case (l : RealIndex3D, u : RealIndex3D) => new knowledge.RectangularDomain(name, index, new RectangularDomainShape(new AABB(l.x, u.x, l.y, u.y, l.z, u.z)))
      case _                                  => new knowledge.RectangularDomain(name, index, new RectangularDomainShape(new AABB()))
    }
  }
}

case class StencilEntry(var offset : ExpressionIndex, var coeff : Expression) extends SpecialStatement {
  override def prettyprint(out : PpStream) = { out << offset << " => " << coeff }

  override def progressToIr : knowledge.StencilEntry = {
    var off = offset.progressToIr
    while (off.length < knowledge.Knowledge.dimensionality + 1) off.indices :+= ir.IntegerConstant(0)
    knowledge.StencilEntry(off, coeff.progressToIr)
  }
}

case class StencilDeclarationStatement(override var identifier : Identifier, var entries : List[StencilEntry]) extends SpecialStatement with HasIdentifier {
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

case class VariableDeclarationStatement(override var identifier : Identifier, var datatype : Datatype, var expression : Option[Expression] = None) extends Statement with HasIdentifier {
  override def prettyprint(out : PpStream) = {
    out << "Variable " << identifier << " : " << datatype
    if (expression.isDefined)
      out << " = " << expression.get
    out << '\n'
  }

  override def progressToIr : ir.VariableDeclarationStatement = {
    ir.VariableDeclarationStatement(datatype.progressToIr,
      identifier.fullName,
      if (expression.isDefined) Some(expression.get.progressToIr) else None)
  }
}

case class ValueDeclarationStatement(override var identifier : Identifier, var datatype : Datatype, var expression : Expression) extends Statement with HasIdentifier {
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
    var field : Access,
    var region : Option[RegionSpecification],
    var seq : Boolean, // FIXME: seq HACK
    var condition : Option[Expression],
    var startOffset : Option[ExpressionIndex],
    var endOffset : Option[ExpressionIndex],
    var increment : Option[ExpressionIndex],
    var statements : List[Statement],
    var reduction : Option[ReductionStatement],
    var preComms : List[CommunicateStatement],
    var postComms : List[CommunicateStatement]) extends Statement {

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

  override def progressToIr : ir.LoopOverPoints = {
    val resolvedField = field match {
      case access : FieldAccess        => access.resolveField
      case access : StencilFieldAccess => access.resolveField
      case _                           => Logger.error(s"Trying to loop over $field - has to be of type FieldAccess or StencilFieldAccess")
    }

    val numDims = resolvedField.fieldLayout.numDimsGrid
    val procStartOffset = new ir.MultiIndex(Array.fill(numDims)(0))
    val procEndOffset = new ir.MultiIndex(Array.fill(numDims)(0))
    val procIncrement = new ir.MultiIndex(Array.fill(numDims)(1))
    if (startOffset.isDefined) {
      val newOffset = startOffset.get.progressToIr
      for (i <- 0 until Math.min(numDims, newOffset.length)) procStartOffset(i) = newOffset(i)
    }
    if (endOffset.isDefined) {
      val newOffset = endOffset.get.progressToIr
      for (i <- 0 until Math.min(numDims, newOffset.length)) procEndOffset(i) = newOffset(i)
    }
    if (increment.isDefined) {
      val newIncrement = increment.get.progressToIr
      for (i <- 0 until Math.min(numDims, newIncrement.length)) procIncrement(i) = newIncrement(i)
    }

    val loop = ir.LoopOverPoints(resolvedField,
      if (region.isDefined) Some(region.get.progressToIr) else None,
      seq,
      procStartOffset,
      procEndOffset,
      procIncrement,
      statements.map(_.progressToIr).to[ListBuffer], // FIXME: .to[ListBuffer]
      preComms.map(_.progressToIr).to[ListBuffer],
      postComms.map(_.progressToIr).to[ListBuffer],
      if (reduction.isDefined) Some(reduction.get.progressToIr) else None,
      if (condition.isDefined) Some(condition.get.progressToIr) else None)

    loop.annotate("l4_fromDSL") // experimental annotation -> if successful and performance impacts are ok annotate all l4 statements
    loop
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

case class ColorWithStatement(var colors : List[Expression], var loop : LoopOverPointsStatement) extends Statement {
  override def prettyprint(out : PpStream) = {
    out << "color with {\n"
    out <<< (colors, ",\n") << ",\n"
    out << loop
    out << "}\n"
  }

  override def progressToIr : ir.Scope = {
    // TODO: think about extracting loop duplication to separate transformation
    var loops = colors.map(color => {
      var newLoop = Duplicate(loop)
      if (newLoop.condition.isDefined)
        newLoop.condition = Some(BooleanExpression("&&", newLoop.condition.get, color)) // TODO: replace with new l4.AndAndExpression later
      else
        newLoop.condition = Some(color)
      newLoop
    })

    ir.Scope(loops.map(_.progressToIr : ir.Statement).to[ListBuffer])
  }
}

case class FunctionStatement(override var identifier : Identifier,
    var returntype : Datatype,
    var arguments : List[VariableAccess],
    var statements : List[Statement],
    var allowInlining : Boolean = true) extends Statement with HasIdentifier {

  override def prettyprint(out : PpStream) = {
    out << "Function " << identifier << " ("
    if (!arguments.isEmpty) {
      for (arg <- arguments) { out << arg.name << " : " << arg.datatype << ", " }
      out.removeLast(2)
    }
    out << " )" << " : " << returntype << " {\n"
    out <<< statements
    out << "}\n"
  }

  override def progressToIr : ir.AbstractFunctionStatement = {
    ir.FunctionStatement(
      returntype.progressToIr,
      identifier.fullName,
      arguments.map(s => s.progressToIr).to[ListBuffer], // FIXME: .to[ListBuffer]
      statements.map(s => s.progressToIr).to[ListBuffer], // FIXME: .to[ListBuffer]
      allowInlining)
  }
}

case class FunctionTemplateStatement(var name : String,
    var templateArgs : List[String],
    var functionArgs : List[VariableAccess],
    var returntype : Datatype,
    var statements : List[Statement]) extends Statement {

  override def prettyprint(out : PpStream) = {
    out << "FunctionTemplate " << name << " < " << templateArgs.mkString(", ") << " > ( "
    if (!functionArgs.isEmpty) {
      for (arg <- functionArgs) { out << arg.name << " : " << arg.datatype << ", " }
      out.removeLast(2)
    }
    out << " )" << " : " << returntype << " {\n"
    out <<< statements
    out << "}\n"
  }

  override def progressToIr : ir.Statement = {
    Logger.warn("Trying to progress FunctionTemplateStatement to ir which is not supported")
    ir.NullStatement
  }
}

case class FunctionInstantiationStatement(var templateName : String,
    args : List[Expression],
    targetFct : Identifier) extends Statement {
  override def prettyprint(out : PpStream) = {
    out << "Instantiate " << templateName << " < "
    if (!args.isEmpty) {
      for (arg <- args) { out << arg << ", " }
      out.removeLast(2)
    }
    out << " > " << " as " << targetFct << "\n"
  }

  override def progressToIr : ir.Statement = {
    Logger.warn("Trying to progress FunctionTemplateStatement to ir which is not supported")
    ir.NullStatement
  }
}

case class ContractionSpecification(var posExt : Index, var negExt : Option[Index]) extends SpecialStatement {
  override def prettyprint(out : PpStream) : Unit = {
    out << posExt
    if (negExt.isDefined)
      out << ", " << negExt
  }

  override def progressToIr : ir.ContractionSpecification = {
    return new ir.ContractionSpecification(posExt.extractArray, negExt.getOrElse(posExt).extractArray)
  }
}

case class RepeatTimesStatement(var number : Int,
    var iterator : Option[Access],
    var contraction : Option[ContractionSpecification],
    var statements : List[Statement]) extends Statement {

  override def prettyprint(out : PpStream) = {
    out << "repeat " << number << " times"
    if (iterator.isDefined) out << " count " << iterator.get
    if (contraction.isDefined) out << " with contraction " << contraction.get
    out << " {\n" <<< statements << "}\n"
  }

  override def progressToIr : ir.Statement = {
    if (contraction.isDefined)
      // FIXME: to[ListBuffer]
      return new ir.ContractingLoop(number, iterator.map(i => i.progressToIr), statements.map(s => s.progressToIr).to[ListBuffer], contraction.get.progressToIr)

    val (loopVar, begin) =
      if (iterator.isDefined) {
        val lv = iterator.get.progressToIr
        (lv, ir.AssignmentStatement(lv, ir.IntegerConstant(0)))
      } else {
        val lv = "someRandomIndexVar" // FIXME: someRandomIndexVar
        (ir.StringLiteral(lv), ir.VariableDeclarationStatement(ir.IntegerDatatype, lv, Some(ir.IntegerConstant(0))))
      }

    val ret = ir.ForLoopStatement(
      begin,
      loopVar < ir.IntegerConstant(number),
      ir.AssignmentStatement(loopVar, ir.IntegerConstant(1), "+="),
      statements.map(s => s.progressToIr).to[ListBuffer], // FIXME: to[ListBuffer]
      None)

    ret.annotate("numLoopIterations", number)

    ret
  }
}

case class RepeatUntilStatement(var comparison : Expression, var statements : List[Statement]) extends Statement {
  override def prettyprint(out : PpStream) = { out << "repeat until " << comparison << "{\n" <<< statements << "}\n" }

  override def progressToIr : ir.WhileLoopStatement = {
    ir.WhileLoopStatement(NegationExpression(comparison.progressToIr), statements.map(s => s.progressToIr).to[ListBuffer])
  }
}

case class ReductionStatement(var op : String, var target : String) extends SpecialStatement {
  override def prettyprint(out : PpStream) = out << "reduction ( " << op << " : " << target << " )"

  override def progressToIr : ir.Reduction = {
    ir.Reduction(op, ir.VariableAccess(target, None))
  }
}

case class RegionSpecification(var region : String, var dir : Index, var onlyOnBoundary : Boolean) extends SpecialStatement {
  override def prettyprint(out : PpStream) = out << region << ' ' << dir

  override def progressToIr : ir.RegionSpecification = {
    ir.RegionSpecification(region, dir.extractArray ++ Array.fill(3 - dir.extractArray.length)(0), onlyOnBoundary)
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
      ir.StringLiteral(ir.LoopOverFragments.defIt)))
  }
}

case class LeveledScopeStatement(var level : LevelSpecification, var statements : List[Statement]) extends Statement {
  override def prettyprint(out : PpStream) = {
    out << level << " {\n"
    statements.foreach(_.prettyprint(out))
    out << "\n}\n"
  }
  override def progressToIr = {
    Logger.error("cannot progress LeveledScopeStatement to IR")
  }
}
