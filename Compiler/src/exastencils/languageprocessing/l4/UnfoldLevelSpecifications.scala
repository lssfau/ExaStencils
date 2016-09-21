package exastencils.languageprocessing.l4

import scala.collection.mutable.{ Node => _, _ }

import exastencils.base.l4._
import exastencils.core._
import exastencils.core.collectors.L4LevelCollector
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.field.l4._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.stencil.l4._

object UnfoldLevelSpecifications extends DefaultStrategy("UnfoldLevelSpecifications") {
  var functions = new HashSet[Tuple2[String, Integer]]

  override def apply(applyAtNode : Option[Node]) = {
    this.transaction()

    Logger.info("Applying strategy " + name)

    var levelCollector = new L4LevelCollector
    this.register(levelCollector)

    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    // ###################################################################################################################

    // resolve level identifiers "coarsest", "finest"
    this.execute(new Transformation("Resolve IdentifierLevelSpecifications", {
      case L4_AllLevels     => L4_LevelRange(L4_SingleLevel(Knowledge.minLevel), L4_SingleLevel(Knowledge.maxLevel))
      case L4_CoarsestLevel => L4_SingleLevel(Knowledge.minLevel)
      case L4_FinestLevel   => L4_SingleLevel(Knowledge.maxLevel)
    }))

    // resolve relative level identifiers
    this.execute(new Transformation("Resolve RelativeLevelSpecifications", {
      case x : L4_RelativeLevel => {
        def calc(a : Int, b : Int) = x.op match {
          case "+" => a + b
          case "-" => a - b
        };
        x.base match {
          case L4_SingleLevel(b) => L4_SingleLevel(calc(b, x.offset))
        }
      }
    }))

    this.execute(new Transformation("Resolve RangeLevelSpecifications", {
      case x : L4_LevelRange => {
        var set = HashSet[L4_LevelSpecification]()
        for (l <- x.begin.resolveLevel to x.end.resolveLevel) {
          set += L4_SingleLevel(l)
        }
        L4_LevelList(set)
      }
    }))

    this.execute(new Transformation("Flatten ListLevelSpecifications", {
      case x : L4_LevelList => x.flatten(); x
    }))

    this.execute(new Transformation("Remove NegatedLevelSpecifications", {
      case x : L4_LevelList => x.cleanup(); x
    }))

    this.execute(new Transformation("Unfold Values and Variables", {
      case value : L4_ValueDeclaration       => value.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(value, level)
        case BasicIdentifier(_)          => value
      }
      case variable : L4_VariableDeclaration => variable.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(variable, level)
        case BasicIdentifier(_)          => variable
      }
    }))

    // find all functions that are defined with an explicit level specification
    this.execute(new Transformation("Find explicitly leveled functions", {
      case function : L4_Function => function.identifier match {
        case LeveledIdentifier(_, level) => level match {
          case x : L4_SingleLevel => {
            functions += ((function.identifier.name, x.level))
            function
          }
          case _                  => function
        }
        case _                           => function
      }
    }))

    // unfold function declarations
    this.execute(new Transformation("Unfold leveled Function declarations", {
      case function : L4_Function => function.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(function, level)
        case BasicIdentifier(_)          => function
      }
    }))

    // Flatten leveled scope or remove completely
    this.execute(new Transformation("Resolve leveled scopes", {
      case scope : LeveledScopeStatement => scope.level match {
        case s : L4_SingleLevel => if (levelCollector.getCurrentLevel == s.level) scope.statements; else List()
        case s : L4_LevelList   => if (s.contains(levelCollector.getCurrentLevel)) scope.statements; else List()
      }
    }))

    // TODO: is it possible to use HasIdentifier to prevent having to list each and every applicable node type?
    // unfold field layout declarations
    this.execute(new Transformation("Unfold leveled FieldLayout declarations", {
      case fieldLayout : L4_FieldLayoutDecl => fieldLayout.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(fieldLayout, level)
        case BasicIdentifier(_)          => fieldLayout
      }
    }))

    // unfold field declarations
    this.execute(new Transformation("Unfold leveled Field declarations", {
      case field : L4_FieldDecl => field.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(field, level)
        case BasicIdentifier(_)          => field
      }
    }))

    // unfold stencil field declarations
    this.execute(new Transformation("Unfold leveled StencilField declarations", {
      case stencilField : L4_StencilFieldDecl => stencilField.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(stencilField, level)
        case BasicIdentifier(_)          => stencilField
      }
    }))

    // unfold stencil declarations
    this.execute(new Transformation("Unfold leveled Stencil declarations", {
      case stencil : L4_StencilDecl => stencil.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(stencil, level)
        case BasicIdentifier(_)          => stencil
      }
    }))

    // resolve level specifications
    this.execute(new Transformation("Resolve RelativeLevelSpecifications", {
      case L4_CurrentLevel => L4_SingleLevel(levelCollector.getCurrentLevel)
      case L4_CoarserLevel => L4_SingleLevel(levelCollector.getCurrentLevel - 1) // FIXME: coarser and finer are not reliable
      case L4_FinerLevel   => L4_SingleLevel(levelCollector.getCurrentLevel + 1)
    }))

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.unregister(levelCollector)
    this.commit()
  }

  def doDuplicate[T <: HasIdentifier](t : T, level : L4_LevelSpecification) : List[T] = {
    var ts = new ListBuffer[T]()
    level match {
      case level @ (L4_SingleLevel(_) | L4_CurrentLevel | L4_CoarserLevel | L4_FinerLevel) => {
        var f = Duplicate(t)
        f.identifier = new LeveledIdentifier(f.identifier.name, level)
        ts += f
      }
      case level : L4_LevelList                                                            =>
        level.levels.foreach(level => ts ++= doDuplicate(t, level))
      case level : L4_LevelRange                                                           =>
        for (level <- math.min(level.begin.resolveLevel, level.end.resolveLevel) to math.max(level.begin.resolveLevel, level.end.resolveLevel)) {
          if (!functions.contains(t.identifier.name, level)) {
            var f = Duplicate(t)
            f.identifier = new LeveledIdentifier(f.identifier.name, L4_SingleLevel(level))
            ts += f

            functions += ((f.identifier.name, level))
          }
        }
      case _                                                                               => Logger.error(s"Invalid level specification for Value $t: $level")
    }
    return ts.toList
  }
}
