package exastencils.languageprocessing.l4

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.L4LevelCollector
import exastencils.core.collectors.L4ValueCollector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.l4._
import exastencils.knowledge._

object ProgressToIr extends DefaultStrategy("ProgressToIr") {
  var levelCollector = new L4LevelCollector
  var valueCollector = new L4ValueCollector

  override def apply(node : Option[Node] = None) = {
    StateManager.register(levelCollector)
    StateManager.register(valueCollector)
    super.apply(node);
    StateManager.unregister(levelCollector)
    StateManager.unregister(valueCollector)
  }

  def doDuplicate[T <: HasIdentifier](t : T, level : LevelSpecification) : List[T] = {
    var ts = new ListBuffer[T]()
    level match {
      case level @ (SingleLevelSpecification(_) | CurrentLevelSpecification() | CoarserLevelSpecification() | FinerLevelSpecification()) => {
        var f = Duplicate(t)
        f.identifier = new LeveledIdentifier(f.identifier.name, level)
        ts += f
      }
      case level : ListLevelSpecification =>
        level.levels.foreach(level => ts ++= doDuplicate(t, level))
      case level : RangeLevelSpecification => // there is no relative (e.g., "current+1") level allowed for function definitions
        for (level <- math.min(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level) to math.max(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level)) {
          var f = Duplicate(t)
          f.identifier = new LeveledIdentifier(f.identifier.name, SingleLevelSpecification(level))
          ts += f
        }
      case _ => Logger.error(s"Invalid level specification for Value $t: $level")
    }
    return ts.toList
  }

  // resolve values in expressions by replacing them with their expression => let SimplifyStrategy do the work
  this += new Transformation("ResolveValuesInExpressions", {
    case x : UnresolvedAccess if (x.level == None && x.slot == None && x.arrayIndex == None) => {
      var value = valueCollector.getValue(x.identifier)
      value match {
        case None => { Logger.info(s"""Did not resolve identifier ${x.identifier} as no matching Val was found"""); x }
        case _    => value.get
      }
    }
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

  this += new Transformation("UnfoldValuesAndVariables", {
    case value : ValueDeclarationStatement => value.identifier match {
      case LeveledIdentifier(_, level) => doDuplicate(value, level)//duplicateValueDeclaration(value, level)
      case BasicIdentifier(_)          => value
    }
    case variable : VariableDeclarationStatement => variable.identifier match {
      case LeveledIdentifier(_, level) => doDuplicate(variable, level)//duplicateVariableDeclaration(variable, level)
      case BasicIdentifier(_)          => variable
    }
  })

  def duplicateValueDeclaration(value : ValueDeclarationStatement, level : LevelSpecification) : List[ValueDeclarationStatement] = {
    var values = new ListBuffer[ValueDeclarationStatement]()
    level match {
      case level @ (SingleLevelSpecification(_) | CurrentLevelSpecification() | CoarserLevelSpecification() | FinerLevelSpecification()) => {
        var f = Duplicate(value)
        f.identifier = new LeveledIdentifier(f.identifier.name, level)
        values += f
      }
      case level : ListLevelSpecification =>
        level.levels.foreach(level => values ++= duplicateValueDeclaration(value, level))
      case level : RangeLevelSpecification => // there is no relative (e.g., "current+1") level allowed for function definitions
        for (level <- math.min(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level) to math.max(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level)) {
          var f = Duplicate(value)
          f.identifier = new LeveledIdentifier(f.identifier.name, SingleLevelSpecification(level))
          values += f
        }
      case _ => Logger.error(s"Invalid level specification for Value $value: $level")
    }
    return values.toList
  }

  def duplicateVariableDeclaration(variable : VariableDeclarationStatement, level : LevelSpecification) : List[VariableDeclarationStatement] = {
    var variables = new ListBuffer[VariableDeclarationStatement]()
    level match {
      case level @ (SingleLevelSpecification(_) | CurrentLevelSpecification() | CoarserLevelSpecification() | FinerLevelSpecification()) => {
        var f = Duplicate(variable)
        f.identifier = new LeveledIdentifier(f.identifier.name, level)
        variables += f
      }
      case level : ListLevelSpecification =>
        level.levels.foreach(level => variables ++= duplicateVariableDeclaration(variable, level))
      case level : RangeLevelSpecification => // there is no relative (e.g., "current+1") level allowed for function definitions
        for (level <- math.min(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level) to math.max(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level)) {
          var f = Duplicate(variable)
          f.identifier = new LeveledIdentifier(f.identifier.name, SingleLevelSpecification(level))
          variables += f
        }
      case _ => Logger.error(s"Invalid level specification for Variable $variable: $level")
    }
    return variables.toList
  }

  // unfold function declarations and calls
  // FIXME: can this be combined into one more generic transformation?

  this += new Transformation("UnfoldLeveledFunctions", {
    case function : FunctionStatement => function.identifier match {
      case LeveledIdentifier(_, level) => doDuplicate(function, level) //duplicateFunctionDeclaration(function, level)
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

  // unfold stencil field declarations

  this += new Transformation("UnfoldLeveledStencilFieldDeclarations", {
    case stencilField : StencilFieldDeclarationStatement => stencilField.level match {
      case Some(level) => duplicateStencilFields(stencilField, level)
      case _           => stencilField
    }
  })

  def duplicateStencilFields(stencilField : StencilFieldDeclarationStatement, level : LevelSpecification) : List[StencilFieldDeclarationStatement] = {
    var stencilFields = new ListBuffer[StencilFieldDeclarationStatement]()
    level match {
      case level @ (SingleLevelSpecification(_) | CurrentLevelSpecification() | CoarserLevelSpecification() | FinerLevelSpecification()) => {
        var f = Duplicate(stencilField)
        f.level = Some(level)
        stencilFields += f
      }
      case level : ListLevelSpecification =>
        level.levels.foreach(level => stencilFields ++= duplicateStencilFields(stencilField, level))
      case level : RangeLevelSpecification => // there is no relative (e.g., "current+1") level allowed for function definitions
        for (level <- math.min(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level) to math.max(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level)) {
          var f = Duplicate(stencilField)
          f.level = Some(SingleLevelSpecification(level))
          stencilFields += f
        }
      case _ => Logger.error(s"Invalid level specification for stencilField $stencilField: $level")
    }
    return stencilFields.toList
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
    case level : CurrentLevelSpecification => SingleLevelSpecification(levelCollector.getCurrentLevel)
    case level : CoarserLevelSpecification => SingleLevelSpecification(levelCollector.getCurrentLevel - 1) // FIXME: coarser and finer are not reliable
    case level : FinerLevelSpecification   => SingleLevelSpecification(levelCollector.getCurrentLevel + 1)
  })

  // resolve accesses
  this += new Transformation("ResolveAccessSpecifications", {
    case access : UnresolvedAccess =>
      if (StateManager.root_.asInstanceOf[Root].fields.exists(f => access.identifier == f.name))
        access.resolveToFieldAccess
      else if (StateManager.root_.asInstanceOf[Root].stencils.exists(s => access.identifier == s.name))
        access.resolveToStencilAccess
      else if (StateManager.root_.asInstanceOf[Root].stencilFields.exists(s => access.identifier == s.name))
        access.resolveToStencilFieldAccess
      else access.resolveToBasicOrLeveledAccess
  })
}