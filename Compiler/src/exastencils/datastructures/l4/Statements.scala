package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils._
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.core._
import exastencils.datastructures._
import exastencils.domain._
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.util._

abstract class SpecialStatement /*TODO: think about an appropriate name*/ extends Node with ProgressableToIr with PrettyPrintable {
  override def progress : Any
}

trait HasIdentifier {
  var identifier : Identifier
}

trait ExternalDeclarationStatement extends SpecialStatement

case class DomainDeclarationStatement(var name : String, var lower : Any, var upper : Any, var index : Int = 0) extends SpecialStatement {
  override def prettyprint(out : PpStream) = {
    (lower, upper) match {
      case (null, null)                 => out << s"Domain = fromFile($name) \n"
      case (l : ConstVec, u : ConstVec) => out << "Domain " << name << "< " << l << " to " << u << " >\n"
      case (lo : List[_], up : List[_]) => {
        (lo.head, up.head) match {
          case (_ : ConstVec, _ : ConstVec) => {
            val sep = lo.map(m => ", ").dropRight(1) :+ " >\n"
            out << "Domain " << name << "< "
            for (i <- lo.indices) { out << lo(i) << " to " << up(i) << sep(i) }
            //out << "Domain " << name << "< " << l(0) << " to " << u(0) << ", " << l(1) << " to " << u(1) << " ," << l(2) << " to " << u(2) << " >\n"
          }
        }
      }
    }
  }
  override def progress : knowledge.Domain = {
    (lower, upper) match {
      case (null, null) => {
        new knowledge.FileInputGlobalDomain("global", index, DomainFileHeader.domainIdentifier.zipWithIndex.map {
          case (identifier, index) => new knowledge.FileInputDomain(identifier, index, new FileInputDomainShape(identifier))
        }.toList)
      }
      case (lo : List[_], up : List[_]) => {
        (lo.head, up.head) match {
          case (_ : ConstVec2D, _ : ConstVec2D) => {
            val rectUnionDomains : List[RectangularDomainShape] =
              (lo.zip(up)).map {
                case (li : ConstVec2D, ui : ConstVec2D) =>
                  new RectangularDomainShape(new AABB(li.x, ui.x, li.y, ui.y, 0.0, 0.0))
              }
            new knowledge.ShapedDomain(name, index, new ShapedDomainShape(rectUnionDomains))
          }
        }
      }
      case (l : ConstVec2D, u : ConstVec2D) => new knowledge.RectangularDomain(name, index, new RectangularDomainShape(new AABB(l.x, u.x, l.y, u.y, 0, 0)))
      case (l : ConstVec3D, u : ConstVec3D) => new knowledge.RectangularDomain(name, index, new RectangularDomainShape(new AABB(l.x, u.x, l.y, u.y, l.z, u.z)))
      case _                                => new knowledge.RectangularDomain(name, index, new RectangularDomainShape(new AABB()))
    }
  }
}

case class StencilEntry(var offset : L4_ExpressionIndex, var coeff : L4_Expression) extends SpecialStatement {
  override def prettyprint(out : PpStream) = { out << offset << " => " << coeff }

  override def progress : knowledge.StencilEntry = {
    var off = offset.progress
    while (off.length < knowledge.Knowledge.dimensionality + 1) off.indices :+= IR_IntegerConstant(0)
    knowledge.StencilEntry(off, coeff.progress)
  }
}

case class StencilDeclarationStatement(override var identifier : Identifier, var entries : List[StencilEntry]) extends SpecialStatement with HasIdentifier {
  override def prettyprint(out : PpStream) = {
    out << "Stencil " << identifier.name << '@' << identifier.asInstanceOf[LeveledIdentifier].level << " {\n"
    out <<< (entries, "\n") << '\n'
    out << "}\n"
  }

  override def progress : knowledge.Stencil = {
    knowledge.Stencil(identifier.name, identifier.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level, entries.map(e => e.progress).to[ListBuffer])
  }
}

case class GlobalDeclarationStatement(var values : List[ValueDeclarationStatement], var variables : List[VariableDeclarationStatement]) extends SpecialStatement {
  def this(entries : List[L4_Statement]) = this(entries.filter(_ match {
    case x : ValueDeclarationStatement => true
    case _                             => false
  }).asInstanceOf[List[ValueDeclarationStatement]],
    entries.filter(_ match {
      case x : VariableDeclarationStatement => true
      case _                                => false
    }).asInstanceOf[List[VariableDeclarationStatement]])

  override def prettyprint(out : PpStream) = { out << "Globals {\n" <<< (values, "\n") <<< (variables, "\n") << "}\n" }

  override def progress : ListBuffer[ir.VariableDeclarationStatement] = {
    variables.to[ListBuffer].map(e => e.progress)
  }
}

case class VariableDeclarationStatement(override var identifier : Identifier, var datatype : L4_Datatype, var expression : Option[L4_Expression] = None) extends L4_Statement with HasIdentifier {
  override def prettyprint(out : PpStream) = {
    out << "Variable " << identifier << " : " << datatype
    if (expression.isDefined)
      out << " = " << expression.get
    out << '\n'
  }

  override def progress : ir.VariableDeclarationStatement = {
    ir.VariableDeclarationStatement(datatype.progress,
      identifier.fullName,
      if (expression.isDefined) Some(expression.get.progress) else None)
  }
}

case class ValueDeclarationStatement(override var identifier : Identifier, var datatype : L4_Datatype, var expression : L4_Expression) extends L4_Statement with HasIdentifier {
  //  def progress : ir.ValueDeclarationStatement = {
  //    ir.ValueDeclarationStatement(datatype.progress,
  //      identifier.progress.asInstanceOf[ir.StringConstant].value,
  //      expression.get.progress
  //  }
  override def prettyprint(out : PpStream) = { out << "Value " << identifier << " : " << datatype << " = " << expression << '\n' }

  override def progress : IR_Statement = IR_NullStatement
}

case class AssignmentStatement(var dest : L4_Access, var src : L4_Expression, var op : String) extends L4_Statement {
  override def prettyprint(out : PpStream) = { out << dest << ' ' << op << ' ' << src << '\n' }

  override def progress : ir.AssignmentStatement = {
    ir.AssignmentStatement(dest.progress, src.progress, op)
  }
}

case class LoopOverPointsStatement(
    var field : L4_Access,
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

  override def progress : ir.LoopOverPoints = {
    val resolvedField = field match {
      case access : FieldAccess        => access.resolveField
      case access : StencilFieldAccess => access.resolveField
      case _                           => Logger.error(s"Trying to loop over $field - has to be of type FieldAccess or StencilFieldAccess")
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

    val loop = ir.LoopOverPoints(resolvedField,
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

  override def progress : ir.LoopOverFragments = {
    new ir.LoopOverFragments(statements.map(s => s.progress).to[ListBuffer],
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

case class FunctionStatement(override var identifier : Identifier,
    var returntype : L4_Datatype,
    var arguments : List[FunctionArgument],
    var statements : List[L4_Statement],
    var allowInlining : Boolean = true) extends L4_Statement with HasIdentifier {

  override def prettyprint(out : PpStream) = {
    out << "Function " << identifier << " ("
    if (!arguments.isEmpty) {
      for (arg <- arguments) { arg.prettyprint(out); out << ", " }
      out.removeLast(2)
    }
    out << " )" << " : " << returntype << " {\n"
    out <<< statements
    out << "}\n"
  }

  override def progress : ir.AbstractFunctionStatement = {
    ir.FunctionStatement(
      returntype.progress,
      identifier.fullName,
      arguments.map(s => s.progress).to[ListBuffer], // FIXME: .to[ListBuffer]
      statements.map(s => s.progress).to[ListBuffer], // FIXME: .to[ListBuffer]
      allowInlining)
  }
}

case class FunctionArgument(override var identifier : Identifier,
    var datatype : L4_Datatype) extends L4_Access with HasIdentifier with ProgressableToIr {
  override def name = identifier.name
  override def prettyprint(out : PpStream) {
    out << identifier.name << " : " << datatype.prettyprint
  }

  override def progress = ir.FunctionArgument(identifier.fullName, datatype.progress)
}

case class FunctionTemplateStatement(var name : String,
    var templateArgs : List[String],
    var functionArgs : List[FunctionArgument],
    var returntype : L4_Datatype,
    var statements : List[L4_Statement]) extends L4_Statement {

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

  override def progress : IR_Statement = {
    Logger.warn("Trying to progress FunctionTemplateStatement to ir which is not supported")
    IR_NullStatement
  }
}

case class FunctionInstantiationStatement(var templateName : String,
    args : List[L4_Expression],
    targetFct : Identifier) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "Instantiate " << templateName << " < "
    if (!args.isEmpty) {
      for (arg <- args) { out << arg << ", " }
      out.removeLast(2)
    }
    out << " > " << " as " << targetFct << "\n"
  }

  override def progress : IR_Statement = {
    Logger.warn("Trying to progress FunctionTemplateStatement to ir which is not supported")
    IR_NullStatement
  }
}

case class ContractionSpecification(var posExt : L4_ConstIndex, var negExt : Option[L4_ConstIndex]) extends SpecialStatement {
  override def prettyprint(out : PpStream) : Unit = {
    out << posExt
    if (negExt.isDefined)
      out << ", " << negExt
  }

  override def progress : ir.ContractionSpecification = {
    return new ir.ContractionSpecification(posExt.progress, negExt.getOrElse(posExt).progress)
  }
}

case class RepeatTimesStatement(var number : Int,
    var iterator : Option[L4_Access],
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
      return new ir.ContractingLoop(number, iterator.map(i => i.progress), statements.map(s => s.progress).to[ListBuffer], contraction.get.progress)

    val (loopVar, begin) =
      if (iterator.isDefined) {
        val lv = iterator.get.progress
        (lv, ir.AssignmentStatement(lv, IR_IntegerConstant(0)))
      } else {
        val lv = "someRandomIndexVar" // FIXME: someRandomIndexVar
        (IR_StringLiteral(lv), ir.VariableDeclarationStatement(IR_IntegerDatatype, lv, Some(IR_IntegerConstant(0))))
      }

    val ret = ir.ForLoopStatement(
      begin,
      loopVar < IR_IntegerConstant(number),
      ir.AssignmentStatement(loopVar, IR_IntegerConstant(1), "+="),
      statements.map(s => s.progress).to[ListBuffer], // FIXME: to[ListBuffer]
      None)

    ret.annotate("numLoopIterations", number)

    ret
  }
}

case class RepeatUntilStatement(var comparison : L4_Expression, var statements : List[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = { out << "repeat until " << comparison << "{\n" <<< statements << "}\n" }

  override def progress : ir.WhileLoopStatement = {
    ir.WhileLoopStatement(IR_NegationExpression(comparison.progress), statements.map(s => s.progress).to[ListBuffer])
  }
}

case class ReductionStatement(var op : String, var target : String) extends SpecialStatement {
  override def prettyprint(out : PpStream) = out << "reduction ( " << op << " : " << target << " )"

  override def progress : ir.Reduction = {
    ir.Reduction(op, IR_VariableAccess(target, None))
  }
}

case class RegionSpecification(var region : String, var dir : L4_ConstIndex, var onlyOnBoundary : Boolean) extends SpecialStatement {
  override def prettyprint(out : PpStream) = out << region << ' ' << dir

  override def progress : ir.RegionSpecification = {
    ir.RegionSpecification(region, L4_ConstIndex(dir.indices ++ Array.fill(3 - dir.indices.length)(0)).progress, onlyOnBoundary)
  }
}

case class FunctionCallStatement(var call : FunctionCallExpression) extends L4_Statement {
  override def prettyprint(out : PpStream) = { out << call << '\n' }

  override def progress : IR_ExpressionStatement = {
    IR_ExpressionStatement(call.progress)
  }
}

case class ReturnStatement(var expr : Option[L4_Expression]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined) out << ' ' << expr.get.prettyprint()
    out << '\n'
  }

  override def progress : ir.ReturnStatement = {
    ir.ReturnStatement(expr.map(_.progress))
  }
}

case class BreakStatement() extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "break\n"
  }

  override def progress : ir.BreakStatement = {
    ir.BreakStatement()
  }
}

case class ConditionalStatement(var expression : L4_Expression, var statements : List[L4_Statement], var elsestatements : List[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "if ( " << expression << " )" << " {\n" <<< statements << '}'
    if (!elsestatements.isEmpty) out << " else {\n" <<< elsestatements << '}'
    out << '\n'
  }

  override def progress : ir.ConditionStatement = {
    new ir.ConditionStatement(expression.progress, statements.map(s => s.progress).to[ListBuffer], elsestatements.map(s => s.progress).to[ListBuffer])
  }
}

case class AdvanceStatement(var field : L4_Access) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "advance "
    field.prettyprint(out)
    out << '\n'
  }

  override def progress = {
    data.AdvanceSlotStatement(ir.iv.CurrentSlot(field.asInstanceOf[FieldAccess].progress.fieldSelection.field,
      IR_StringLiteral(ir.LoopOverFragments.defIt)))
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
