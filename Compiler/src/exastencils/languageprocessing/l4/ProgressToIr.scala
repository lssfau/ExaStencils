package exastencils.languageprocessing.l4

import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.L4LevelCollector
import exastencils.core.collectors.L4ValueCollector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.l4._
import exastencils.knowledge._
import exastencils.logger._

object ProgressToIr extends DefaultStrategy("ProgressToIr") {
  var functions = new HashSet[Tuple2[String, Integer]]

  override def apply(applyAtNode : Option[Node]) = {
    this.transaction()
    this.resetCollectors()
    this.unregisterAll()

    Logger.info("Applying strategy " + name)
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    var levelCollector = new L4LevelCollector
    var valueCollector = new L4ValueCollector
    this.register(levelCollector)

    // ###################################################################################################################

    //    // annotate function calls to special function, e.g. return(), so they are not changed by the next transformation
    //    StateManager.apply(this.token.get, new Transformation("AnnotateFunctionCalls", {
    //      case f : FunctionCallExpression =>
    //        f.identifier match {
    //          case x : UnresolvedAccess if (x.identifier == "return") => x.annotate("NO_PROTECT_THIS")
    //          case _ =>
    //        }; f
    //    }))

    // rename identifiers that happen to have the same name as C/C++ keywords or start with "_"
    // identifiers starting with "_" are protected for internal use
    val protectedkeywords = HashSet("alignas", "alignof", "and", "and_eq",
      "asm", "auto", "bitand", "bitor", "bool", "break", "case", "catch", "char", "char16_t", "char32_t", "class", "compl",
      "const", "constexpr", "const_cast", "continue", "decltype", "default", "delete", "do", "double", "dynamic_cast",
      "else", "enum", "explicit", "export", "extern", "false", "float", "for", "friend", "goto", "if", "inline", "int",
      "long", "mutable", "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private",
      "protected", "public", "register", "reinterpret_cast", "return", "short", "signed", "sizeof", "static", "static_assert",
      "static_cast", "struct", "switch", "template", "this", "thread_local", "throw", "true", "try", "typedef",
      "typeid", "typename", "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq")

    // No need to transform Domain- and LayoutDeclarationStatements because their names are not outputted.
    StateManager.apply(this.token.get, new Transformation("EscapeCppKeywordsAndInternalIdentifiers", {
      case x : Identifier if (protectedkeywords.contains(x.name)) =>
        x.name = "user_" + x.name; x
      case x : Identifier if (x.name.startsWith("_")) =>
        x.name = "user_" + x.name; x
      case x : UnresolvedAccess if (protectedkeywords.contains(x.name) && !x.hasAnnotation("NO_PROTECT_THIS")) =>
        x.name = "user_" + x.name; x
      case x : UnresolvedAccess if (x.name.startsWith("_")) =>
        x.name = "user_" + x.name; x
      case x : ExternalFieldDeclarationStatement if (protectedkeywords.contains(x.extIdentifier)) =>
        x.extIdentifier = "user_" + x.extIdentifier; x
      case x : ExternalFieldDeclarationStatement if (x.extIdentifier.startsWith("_")) =>
        x.extIdentifier = "user_" + x.extIdentifier; x
    }))

    // resolve level identifiers "coarsest", "finest"
    StateManager.apply(this.token.get, new Transformation("ResolveIdentifierLevels", {
      case x : AllLevelsSpecification     => RangeLevelSpecification(SingleLevelSpecification(Knowledge.minLevel), SingleLevelSpecification(Knowledge.maxLevel))
      case x : CoarsestLevelSpecification => SingleLevelSpecification(Knowledge.minLevel)
      case x : FinestLevelSpecification   => SingleLevelSpecification(Knowledge.maxLevel)
    }))

    // resolve relative level identifiers
    StateManager.apply(this.token.get, new Transformation("ResolveRelativeIdentifiers", {
      case x : RelativeLevelSpecification => {
        def calc(a : Int, b : Int) = x.operator match {
          case "+" => a + b
          case "-" => a - b
        };
        x.base match {
          case SingleLevelSpecification(b) => SingleLevelSpecification(calc(b, x.offset))
        }
      }
    }))

    StateManager.apply(this.token.get, new Transformation("UnfoldValuesAndVariables", {
      case value : ValueDeclarationStatement => value.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(value, level)
        case BasicIdentifier(_)          => value
      }
      case variable : VariableDeclarationStatement => variable.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(variable, level)
        case BasicIdentifier(_)          => variable
      }
    }))

    // find all functions that are defined with an explicit level specification
    StateManager.apply(this.token.get, new Transformation("FindExplicitlyLeveledFunctions", {
      case function : FunctionStatement => function.identifier match {
        case LeveledIdentifier(_, level) => level match {
          case x : SingleLevelSpecification => {
            functions += ((function.identifier.name, x.level))
            function
          }
          case _ => function
        }
        case _ => function
      }
    }))

    // unfold function declarations
    StateManager.apply(this.token.get, new Transformation("UnfoldLeveledFunctions", {
      case function : FunctionStatement => function.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(function, level)
        case BasicIdentifier(_)          => function
      }
    }))

    // FIXME unfold function calls

    // unfold field layout declarations
    StateManager.apply(this.token.get, new Transformation("UnfoldLeveledFieldLayoutDeclarations", {
      case fieldLayout : LayoutDeclarationStatement => fieldLayout.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(fieldLayout, level)
        case BasicIdentifier(_)          => fieldLayout
      }
    }))

    // unfold field declarations
    StateManager.apply(this.token.get, new Transformation("UnfoldLeveledFieldDeclarations", {
      case field : FieldDeclarationStatement => field.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(field, level)
        case BasicIdentifier(_)          => field
      }
    }))

    // unfold stencil field declarations
    StateManager.apply(this.token.get, new Transformation("UnfoldLeveledStencilFieldDeclarations", {
      case stencilField : StencilFieldDeclarationStatement => stencilField.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(stencilField, level)
        case BasicIdentifier(_)          => stencilField
      }
    }))

    // unfold stencil declarations
    StateManager.apply(this.token.get, Transformation("UnfoldLeveledStencilDeclarations", {
      case stencil : StencilDeclarationStatement => stencil.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(stencil, level)
        case BasicIdentifier(_)          => stencil
      }
    }))

    // resolve level specifications
    StateManager.apply(this.token.get, new Transformation("ResolveRelativeLevelSpecifications", {
      case level : CurrentLevelSpecification => SingleLevelSpecification(levelCollector.getCurrentLevel)
      case level : CoarserLevelSpecification => SingleLevelSpecification(levelCollector.getCurrentLevel - 1) // FIXME: coarser and finer are not reliable
      case level : FinerLevelSpecification   => SingleLevelSpecification(levelCollector.getCurrentLevel + 1)
    }))

    this.register(valueCollector)

    StateManager.apply(this.token.get, new Transformation("FillValueCollector", {
      case x : GlobalDeclarationStatement => x
    }))

    // resolve values in expressions by replacing them with their expression => let SimplifyStrategy do the work
    StateManager.apply(this.token.get, new Transformation("ResolveValuesInExpressions", {
      case x : UnresolvedAccess if (x.level == None && x.slot == None && x.arrayIndex == None) => {
        var value = valueCollector.getValue(x.name)
        value match {
          case None => { Logger.info(s"""Did not resolve identifier ${x.name} as no matching Val was found"""); x }
          case _    => value.get
        }
      }
      case x : UnresolvedAccess if (x.level.isDefined && x.level.get.isInstanceOf[SingleLevelSpecification] && x.slot == None && x.arrayIndex == None) => {
        var value = valueCollector.getValue(x.name + "_" + x.level.get.asInstanceOf[SingleLevelSpecification].level)
        value match {
          case None => { Logger.info(s"""Did not resolve identifier ${x.name} as no matching Val was found"""); x }
          case _    => value.get
        }
      }
    }))

    // resolve accesses
    StateManager.apply(this.token.get, new Transformation("ResolveAccessSpecifications", {
      case access : UnresolvedAccess =>
        if (StateManager.root_.asInstanceOf[Root].fields.exists(f => access.name == f.identifier.name))
          access.resolveToFieldAccess
        else if (StateManager.root_.asInstanceOf[Root].stencils.exists(s => access.name == s.identifier.name))
          access.resolveToStencilAccess
        else if (StateManager.root_.asInstanceOf[Root].stencilFields.exists(s => access.name == s.identifier.name))
          access.resolveToStencilFieldAccess
        else access.resolveToBasicOrLeveledAccess
    }))

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
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
      case level : RangeLevelSpecification =>
        for (level <- math.min(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level) to math.max(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level)) {
          if (!functions.contains(t.identifier.name, level)) {
            var f = Duplicate(t)
            f.identifier = new LeveledIdentifier(f.identifier.name, SingleLevelSpecification(level))
            ts += f

            functions += ((f.identifier.name, level))
          }
        }
      case _ => Logger.error(s"Invalid level specification for Value $t: $level")
    }
    return ts.toList
  }
}