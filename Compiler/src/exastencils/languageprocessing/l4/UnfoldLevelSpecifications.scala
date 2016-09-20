package exastencils.languageprocessing.l4

import scala.collection.mutable.{ Node => _, _ }

import exastencils.base.l4.L4_Function
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
      case AllLevelsSpecification     => RangeLevelSpecification(SingleLevelSpecification(Knowledge.minLevel), SingleLevelSpecification(Knowledge.maxLevel))
      case CoarsestLevelSpecification => SingleLevelSpecification(Knowledge.minLevel)
      case FinestLevelSpecification   => SingleLevelSpecification(Knowledge.maxLevel)
    }))

    // resolve relative level identifiers
    this.execute(new Transformation("Resolve RelativeLevelSpecifications", {
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

    this.execute(new Transformation("Resolve RangeLevelSpecifications", {
      case x : RangeLevelSpecification => {
        var set = HashSet[LevelSpecification]()
        for (l <- x.begin.asInstanceOf[SingleLevelSpecification].level to x.end.asInstanceOf[SingleLevelSpecification].level) {
          set += (SingleLevelSpecification(l))
        }
        ListLevelSpecification(set)
      }
    }))

    this.execute(new Transformation("Flatten ListLevelSpecifications", {
      case x : ListLevelSpecification => x.flatten(); x
    }))

    this.execute(new Transformation("Remove NegatedLevelSpecifications", {
      case x : ListLevelSpecification => x.cleanup(); x
    }))

    this.execute(new Transformation("Unfold Values and Variables", {
      case value : ValueDeclarationStatement       => value.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(value, level)
        case BasicIdentifier(_)          => value
      }
      case variable : VariableDeclarationStatement => variable.identifier match {
        case LeveledIdentifier(_, level) => doDuplicate(variable, level)
        case BasicIdentifier(_)          => variable
      }
    }))

    // find all functions that are defined with an explicit level specification
    this.execute(new Transformation("Find explicitly leveled functions", {
      case function : L4_Function => function.identifier match {
        case LeveledIdentifier(_, level) => level match {
          case x : SingleLevelSpecification => {
            functions += ((function.identifier.name, x.level))
            function
          }
          case _                            => function
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
        case s : SingleLevelSpecification => if (levelCollector.getCurrentLevel == s.level) scope.statements; else List()
        case s : ListLevelSpecification   => if (s.contains(levelCollector.getCurrentLevel)) scope.statements; else List()
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
      case CurrentLevelSpecification => SingleLevelSpecification(levelCollector.getCurrentLevel)
      case CoarserLevelSpecification => SingleLevelSpecification(levelCollector.getCurrentLevel - 1) // FIXME: coarser and finer are not reliable
      case FinerLevelSpecification   => SingleLevelSpecification(levelCollector.getCurrentLevel + 1)
    }))

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.unregister(levelCollector)
    this.commit()
  }

  def doDuplicate[T <: HasIdentifier](t : T, level : LevelSpecification) : List[T] = {
    var ts = new ListBuffer[T]()
    level match {
      case level @ (SingleLevelSpecification(_) | CurrentLevelSpecification | CoarserLevelSpecification | FinerLevelSpecification) => {
        var f = Duplicate(t)
        f.identifier = new LeveledIdentifier(f.identifier.name, level)
        ts += f
      }
      case level : ListLevelSpecification                                                                                          =>
        level.levels.foreach(level => ts ++= doDuplicate(t, level))
      case level : RangeLevelSpecification                                                                                         =>
        for (level <- math.min(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level) to math.max(level.begin.asInstanceOf[SingleLevelSpecification].level, level.end.asInstanceOf[SingleLevelSpecification].level)) {
          if (!functions.contains(t.identifier.name, level)) {
            var f = Duplicate(t)
            f.identifier = new LeveledIdentifier(f.identifier.name, SingleLevelSpecification(level))
            ts += f

            functions += ((f.identifier.name, level))
          }
        }
      case _                                                                                                                       => Logger.error(s"Invalid level specification for Value $t: $level")
    }
    return ts.toList
  }
}
