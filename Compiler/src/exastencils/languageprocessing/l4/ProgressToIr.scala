package exastencils.languageprocessing.l4

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.core.ImplicitConversions._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge.Knowledge

object ProgressToIr extends Strategy("ProgressToIr") {
  var collector = new LevelCollector

  override def apply(hackedApplyAt : Option[Node] = None, hackedToken : Option[StateManager.History.TransactionToken] = None) = {
    StateManager.register(collector);
    super.apply(hackedApplyAt, hackedToken);
    StateManager.unregister(collector);
  }

  // resolve grouped level specifications with size one, i.e. groups with only one element

  this += new Transformation("ResolveGroupedLevelSpecifications", {
    case UnresolvedIdentifier(name, Some(ListLevelSpecification(levels))) if (1 == levels.size) =>
      UnresolvedIdentifier(name, Some(levels.head))
    case FieldIdentifier(name, ListLevelSpecification(levels)) if (1 == levels.size) =>
      FieldIdentifier(name, levels.head)
  })

  // resolve raw level specifications

  this += new Transformation("ResolveRawLevelSpecifications", {
    case UnresolvedIdentifier(name, Some(level)) =>
      if (StateManager.root_.asInstanceOf[Root].fields.exists(f => name == f.name))
        FieldIdentifier(name, level)
      else if (StateManager.root_.asInstanceOf[Root].stencils.exists(s => name == s.name))
        StencilIdentifier(name, level)
      else
        LeveledIdentifier(name, level)
    case UnresolvedIdentifier(name, _) => BasicIdentifier(name)
  })

  // resolve level identifiers "coarsest", "finest"
  this += new Transformation("ResolveIdentifierLevels", {
    case x : AllLevelsSpecification     => RangeLevelSpecification(SingleLevelSpecification(0), SingleLevelSpecification(Knowledge.maxLevel))
    case x : CoarsestLevelSpecification => SingleLevelSpecification(0)
    case x : FinestLevelSpecification   => SingleLevelSpecification(Knowledge.maxLevel)
  })

  // resolve relative level identifiers
  this += new Transformation("ResolveRelativeIdentifiers", {
    case x : RelativeLevelSpecification => {
      def calc(a : Int, b : Int) = x.operator match {
        case "+" => a + b
        case "-" => a - b
      };
      x.base match {
        case SingleLevelSpecification(b) => SingleLevelSpecification(calc(b, x.offset))
      }
    }
    //case _ => Log.error("ResolveRelativeIdentifiers: fixme")
  })

  // unfold function declarations and calls
  // FIXME: can this be combined into one more generic transformation?

  this += new Transformation("UnfoldLeveledFunctions", {
    case function : FunctionStatement => function.identifier match {
      case LeveledIdentifier(_, level) => duplicateFunctionDeclaration(function, level)
      case BasicIdentifier(_)          => function
    }
    case function : FunctionCallStatement => function.identifier match {
      case LeveledIdentifier(_, level) => duplicateFunctionCall(function, level)
      case BasicIdentifier(_)          => function
    }
  })

  def duplicateFunctionDeclaration(function : FunctionStatement, level : LevelSpecification) : List[FunctionStatement] = {
    var functions = new ListBuffer[FunctionStatement]()
    level match {
      case level @ (SingleLevelSpecification(_) | CurrentLevelSpecification() | CoarserLevelSpecification() | FinerLevelSpecification()) => {
        var f = Duplicate(function)
        f.identifier = new LeveledIdentifier(f.identifier.name, level)
        functions += f
      }
      case level : ListLevelSpecification =>
        level.levels.foreach(level => functions ++= duplicateFunctionDeclaration(function, level))
      case level : RangeLevelSpecification => // there is no relative (e.g., "current+1") level allowed for function definitions
        for (level <- math.min(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level) to math.max(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level)) {
          var f = Duplicate(function)
          f.identifier = new LeveledIdentifier(f.identifier.name, SingleLevelSpecification(level))
          functions += f
        }
      case _ => Logger.error(s"Invalid level specification for function $function: $level")
    }
    return functions.toList
  }

  def duplicateFunctionCall(function : FunctionCallStatement, level : LevelSpecification) : List[FunctionCallStatement] = {
    var functions = new ListBuffer[FunctionCallStatement]()
    level match {
      case level @ (SingleLevelSpecification(_) | CurrentLevelSpecification() | CoarserLevelSpecification() | FinerLevelSpecification()) => {
        var f = Duplicate(function)
        f.identifier = new LeveledIdentifier(f.identifier.name, level)
        functions += f
      }
      case level : ListLevelSpecification =>
        level.levels.foreach(level => functions ++= duplicateFunctionCall(function, level))
      case level : RangeLevelSpecification => // all relative (e.g., "current+1") levels should have been resolved already
        for (level <- math.min(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level) to math.max(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level)) {
          var f = Duplicate(function)
          f.identifier = new LeveledIdentifier(f.identifier.name, SingleLevelSpecification(level))
          functions += f
        }
      case _ => Logger.error(s"Invalid level specification for function $function: $level")
    }
    return functions.toList
  }

  // unfold field declarations

  this += new Transformation("UnfoldLeveledFieldDeclarations", {
    case field : FieldDeclarationStatement => field.level match {
      case Some(level) => duplicateFields(field, level)
      case _           => field
    }
  })

  def duplicateFields(field : FieldDeclarationStatement, level : LevelSpecification) : List[FieldDeclarationStatement] = {
    var fields = new ListBuffer[FieldDeclarationStatement]()
    level match {
      case level @ (SingleLevelSpecification(_) | CurrentLevelSpecification() | CoarserLevelSpecification() | FinerLevelSpecification()) => {
        var f = Duplicate(field)
        f.level = Some(level)
        fields += f
      }
      case level : ListLevelSpecification =>
        level.levels.foreach(level => fields ++= duplicateFields(field, level))
      case level : RangeLevelSpecification => // there is no relative (e.g., "current+1") level allowed for function definitions
        for (level <- math.min(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level) to math.max(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level)) {
          var f = Duplicate(field)
          f.level = Some(SingleLevelSpecification(level))
          fields += f
        }
      case _ => Logger.error(s"Invalid level specification for field $field: $level")
    }
    return fields.toList
  }

  // unfold stencil declarations
  // FIXME: can this be combined into one more generic transformation?

  this += new Transformation("UnfoldLeveledStencilDeclarations", {
    case stencil : StencilDeclarationStatement => stencil.level match {
      case Some(level) => duplicateStencils(stencil, level)
      case _           => stencil
    }
  })

  def duplicateStencils(stencil : StencilDeclarationStatement, level : LevelSpecification) : List[StencilDeclarationStatement] = {
    var stencils = new ListBuffer[StencilDeclarationStatement]()
    level match {
      case level @ (SingleLevelSpecification(_) | CurrentLevelSpecification() | CoarserLevelSpecification() | FinerLevelSpecification()) => {
        var f = Duplicate(stencil)
        f.level = Some(level)
        stencils += f
      }
      case level : ListLevelSpecification =>
        level.levels.foreach(level => stencils ++= duplicateStencils(stencil, level))
      case level : RangeLevelSpecification => // there is no relative (e.g., "current+1") level allowed for function definitions
        for (level <- math.min(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level) to math.max(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level)) {
          var f = Duplicate(stencil)
          f.level = Some(SingleLevelSpecification(level))
          stencils += f
        }
      case _ => Logger.error(s"Invalid level specification for stencil $stencil: $level")
    }
    return stencils.toList
  }

  // resolve level specifications

  this += new Transformation("ResolveRelativeLevelSpecifications", {
    case level : CurrentLevelSpecification => SingleLevelSpecification(collector.curLevel)
    case level : CoarserLevelSpecification => SingleLevelSpecification(collector.curLevel - 1) // FIXME: coarser and finer are not reliable
    case level : FinerLevelSpecification   => SingleLevelSpecification(collector.curLevel + 1)
  })

  /*  def doTransformToIr(node : l4.Datatype) : ir.Datatype = {
    node match {
      case x : l4.IntegerDatatype => new ir.IntegerDatatype
      case x : l4.StringDatatype  => new ir.StringDatatype
      case x : l4.UnitDatatype    => new ir.StringDatatype
      case x : l4.ArrayDatatype   => new ir.ArrayDatatype(doTransformToIr(x.datatype))
      case x : l4.ComplexDatatype => new ir.ComplexDatatype(doTransformToIr(x.datatype))
    }
  }

  def resolveLeveledName(id : l4.Identifier) : String = s"${id.name}__level_${
    id.level match {
      case Some(x) => x.toString()
      case None    => "_"
    }
  }"

  def doTransformToIr(node : l4.Variable) : ir.VariableAccess = {
    new ir.VariableAccess(resolveLeveledName(node.identifier), Some(doTransformToIr(node.datatype)))
  }

  def doTransformToIr(node : l4.Expression) : ir.Expression = {
    node match {
      case l4.StringConstant(x)  => ir.StringConstant(x)
      case l4.IntegerConstant(x) => ir.IntegerConstant(x)
      case l4.FloatConstant(x)   => ir.FloatConstant(x)
      case l4.BooleanConstant(x) => ir.BooleanConstant(x)
      case _                     => Log.error(s"No rule for progression of L4 node ${node}")
    }
  }

  def doTransformToIr(node : l4.Statement) : ir.Statement = {
    node match {
      case x : l4.FunctionStatement => ir.FunctionStatement(doTransformToIr(x.returntype),
        resolveLeveledName(x.identifier),
        x.arguments.map(doTransformToIr(_)),
        x.statements.map(doTransformToIr(_)))
      case x : l4.RepeatUpStatement => new ir.ForLoopStatement(
        "int" ~ "i" ~ "=" ~ "0", // FIXME
        ir.BinaryExpression(ir.BinaryOperators.Lower, ir.VariableAccess("i"), ir.IntegerConstant(x.number)),
        "i" ~ "=" ~ "i" ~ "+" ~ "1", // FIXME
        x.statements.map(doTransformToIr(_)))
      case x : l4.FunctionCallStatement => new ir.ExpressionStatement(new ir.FunctionCallExpression(
        resolveLeveledName(x.identifier), x.arguments.map(doTransformToIr(_))))
      case _ => Log.error(s"No rule for progression of L4 node ${node}")
    }
  }
*/
  // def apply() = {

  //    var root = new ir.Root()

  /*    strategy += new Transformation("ProgressNodesToIr", {
      case x : l4.Statement => root += doTransformToIr(x); x
    })
*/
  //   this.apply
  //    println("new root:" + root)
  // }

}