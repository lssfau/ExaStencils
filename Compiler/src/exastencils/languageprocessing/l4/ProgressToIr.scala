package exastencils.languageprocessing.l4

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors.L4LevelCollector
import exastencils.core.collectors.L4ValueCollector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.l4._
import exastencils.knowledge._
import scala.collection.immutable.HashSet

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
      case level : RangeLevelSpecification =>
        for (level <- math.min(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level) to math.max(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level)) {
          var f = Duplicate(t)
          f.identifier = new LeveledIdentifier(f.identifier.name, SingleLevelSpecification(level))
          ts += f
        }
      case _ => Logger.error(s"Invalid level specification for Value $t: $level")
    }
    return ts.toList
  }

  // ###################################################################################################################

  // rename identifiers that happen to have the same name as C/C++ keywords or start with "_"
  // identifiers starting with "_" are protected for internal use
  var protectedkeywords = HashSet("alignas", "alignof", "and", "and_eq",
    "asm", "auto", "bitand", "bitor", "bool", "break", "case", "catch", "char", "char16_t", "char32_t", "class", "compl",
    "const", "constexpr", "const_cast", "continue", "decltype", "default", "delete", "do", "double", "dynamic_cast",
    "else", "enum", "explicit", "export", "extern", "false", "float", "for", "friend", "goto", "if", "inline", "int",
    "long", "mutable", "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private",
    "protected", "public", "register", "reinterpret_cast", "return", "short", "signed", "sizeof", "static", "static_assert",
    "static_cast", "struct", "switch", "template", "this", "thread_local", "throw", "true", "try", "typedef",
    "typeid", "typename", "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq")
  this += new Transformation("EscapeCppKeywordsAndInternalIdentifiers", {
    case x : Identifier if (protectedkeywords.contains(x.name)) =>
      x.name = "user_" + x.name; x
    case x : Identifier if (x.name.startsWith("_")) =>
      x.name = "user_" + x.name; x
    case x : UnresolvedAccess if (x.identifier.startsWith("_")) =>
      x.identifier = "user_" + x.identifier; x
  })

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
  })

  this += new Transformation("UnfoldValuesAndVariables", {
    case value : ValueDeclarationStatement => value.identifier match {
      case LeveledIdentifier(_, level) => doDuplicate(value, level)
      case BasicIdentifier(_)          => value
    }
    case variable : VariableDeclarationStatement => variable.identifier match {
      case LeveledIdentifier(_, level) => doDuplicate(variable, level)
      case BasicIdentifier(_)          => variable
    }
  })

  // unfold function declarations
  this += new Transformation("UnfoldLeveledFunctions", {
    case function : FunctionStatement => function.identifier match {
      case LeveledIdentifier(_, level) => doDuplicate(function, level)
      case BasicIdentifier(_)          => function
    }
  })

  // FIXME unfold function calls

  // unfold field declarations
  this += new Transformation("UnfoldLeveledFieldDeclarations", {
    case field : FieldDeclarationStatement => field.identifier match {
      case LeveledIdentifier(_, level) => doDuplicate(field, level)
      case BasicIdentifier(_)          => field
    }
  })

  // unfold stencil field declarations
  this += new Transformation("UnfoldLeveledStencilFieldDeclarations", {
    case stencilField : StencilFieldDeclarationStatement => stencilField.identifier match {
      case LeveledIdentifier(_, level) => doDuplicate(stencilField, level)
      case BasicIdentifier(_)          => stencilField
    }
  })

  // unfold stencil declarations
  this += new Transformation("UnfoldLeveledStencilDeclarations", {
    case stencil : StencilDeclarationStatement => stencil.identifier match {
      case LeveledIdentifier(_, level) => doDuplicate(stencil, level)
      case BasicIdentifier(_)          => stencil
    }
  })

  // resolve level specifications
  this += new Transformation("ResolveRelativeLevelSpecifications", {
    case level : CurrentLevelSpecification => SingleLevelSpecification(levelCollector.getCurrentLevel)
    case level : CoarserLevelSpecification => SingleLevelSpecification(levelCollector.getCurrentLevel - 1) // FIXME: coarser and finer are not reliable
    case level : FinerLevelSpecification   => SingleLevelSpecification(levelCollector.getCurrentLevel + 1)
  })

  // resolve accesses
  this += new Transformation("ResolveAccessSpecifications", {
    case access : UnresolvedAccess =>
      if (StateManager.root_.asInstanceOf[Root].fields.exists(f => access.identifier == f.identifier.name))
        access.resolveToFieldAccess
      else if (StateManager.root_.asInstanceOf[Root].stencils.exists(s => access.identifier == s.identifier.name))
        access.resolveToStencilAccess
      else if (StateManager.root_.asInstanceOf[Root].stencilFields.exists(s => access.identifier == s.identifier.name))
        access.resolveToStencilFieldAccess
      else access.resolveToBasicOrLeveledAccess
  })
}