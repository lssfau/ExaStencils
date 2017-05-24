package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_LevelSpecification extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_LevelSpecification.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.base.|LAYER_LC|"""
    printer <<< """"""
    if (L2 == layer) {
      printer <<< """import exastencils.base.|NEXT_LC|._"""
    }
    if (L3 == layer) {
      printer <<< """import exastencils.base.|NEXT_LC|._"""
    }
    printer <<< """import exastencils.config.Knowledge"""
    if (L4 == layer) {
      printer <<< """import exastencils.core.collectors.|LAYER_UC|LevelCollector"""
    }
    printer <<< """import exastencils.datastructures._"""
    printer <<< """import exastencils.logger.Logger"""
    printer <<< """import exastencils.prettyprinting._"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_LevelSpecification"""
    printer <<< """"""
    if (L4 == layer) {
      printer <<< """trait |LAYER_UC|_LevelSpecification extends |LAYER_UC|_Node with PrettyPrintable {"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """object |LAYER_UC|_LevelSpecification {"""
    }
    if (L2 == layer) {
      printer <<< """  def extractLevelList(levels : Option[|LAYER_UC|_LevelSpecification], defForNone : List[Int]) : List[Int] = {"""
      printer <<< """    levels match {"""
      printer <<< """      case None                        => defForNone"""
      printer <<< """      case Some(|LAYER_UC|_SingleLevel(level)) => List(level)"""
      printer <<< """      case Some(|LAYER_UC|_LevelList(lvls))    => lvls.map(_.asInstanceOf[|LAYER_UC|_SingleLevel].level).toList"""
      printer <<< """      case other                       => Logger.error("Trying to extract level list from unsupported instance " + other)"""
      printer <<< """    }"""
      printer <<< """  }"""
      printer <<< """"""
      printer <<< """  // assumes list of all levels as default"""
      printer <<< """  def extractLevelListDefAll(levels : Option[|LAYER_UC|_LevelSpecification]) : List[Int] = extractLevelList(levels, (Knowledge.minLevel to Knowledge.maxLevel).toList)"""
      printer <<< """"""
      printer <<< """  // assumes empty level list as default"""
      printer <<< """  def extractLevelListDefEmpty(levels : Option[|LAYER_UC|_LevelSpecification]) : List[Int] = extractLevelList(levels, List())"""
      printer <<< """"""
      printer <<< """  def asSingleLevel(level : Option[|LAYER_UC|_LevelSpecification]) : Int = {"""
      printer <<< """    level match {"""
      printer <<< """      case Some(|LAYER_UC|_SingleLevel(lvl)) => lvl"""
      printer <<< """      case None                      => Logger.error("Missing level specification")"""
      printer <<< """      case Some(other)               => Logger.error(s"Invalid level specification: $other")"""
      printer <<< """    }"""
      printer <<< """  }"""
      printer <<< """}"""
      printer <<< """"""
      printer <<< """trait |LAYER_UC|_LevelSpecification extends |LAYER_UC|_Node with |LAYER_UC|_Progressable with PrettyPrintable {"""
      printer <<< """  override def progress : |NEXT_UC|_LevelSpecification"""
    }
    if (L3 == layer) {
      printer <<< """  def extractLevelList(levels : Option[|LAYER_UC|_LevelSpecification], defForNone : List[Int]) : List[Int] = {"""
      printer <<< """    levels match {"""
      printer <<< """      case None                        => defForNone"""
      printer <<< """      case Some(|LAYER_UC|_SingleLevel(level)) => List(level)"""
      printer <<< """      case Some(|LAYER_UC|_LevelList(lvls))    => lvls.map(_.asInstanceOf[|LAYER_UC|_SingleLevel].level).toList"""
      printer <<< """      case other                       => Logger.error("Trying to extract level list from unsupported instance " + other)"""
      printer <<< """    }"""
      printer <<< """  }"""
      printer <<< """"""
      printer <<< """  // assumes list of all levels as default"""
      printer <<< """  def extractLevelListDefAll(levels : Option[|LAYER_UC|_LevelSpecification]) : List[Int] = extractLevelList(levels, (Knowledge.minLevel to Knowledge.maxLevel).toList)"""
      printer <<< """"""
      printer <<< """  // assumes empty level list as default"""
      printer <<< """  def extractLevelListDefEmpty(levels : Option[|LAYER_UC|_LevelSpecification]) : List[Int] = extractLevelList(levels, List())"""
      printer <<< """"""
      printer <<< """  def asSingleLevel(level : Option[|LAYER_UC|_LevelSpecification]) : Int = {"""
      printer <<< """    level match {"""
      printer <<< """      case Some(|LAYER_UC|_SingleLevel(lvl)) => lvl"""
      printer <<< """      case None                      => Logger.error("Missing level specification")"""
      printer <<< """      case Some(other)               => Logger.error(s"Invalid level specification: $other")"""
      printer <<< """    }"""
      printer <<< """  }"""
      printer <<< """}"""
      printer <<< """"""
      printer <<< """trait |LAYER_UC|_LevelSpecification extends |LAYER_UC|_Node with |LAYER_UC|_Progressable with PrettyPrintable {"""
      printer <<< """  override def progress : |NEXT_UC|_LevelSpecification"""
    }
    printer <<< """  def resolveLevel : Int"""
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_DeclarationLevelSpecification"""
    printer <<< """"""
    printer <<< """// can be used for declarations, e.g., functions"""
    if (L4 == layer) {
      printer <<< """trait |LAYER_UC|_DeclarationLevelSpecification extends |LAYER_UC|_LevelSpecification"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """trait |LAYER_UC|_DeclarationLevelSpecification extends |LAYER_UC|_LevelSpecification {"""
    }
    if (L2 == layer) {
      printer <<< """  override def progress : |NEXT_UC|_DeclarationLevelSpecification"""
      printer <<< """}"""
    }
    if (L3 == layer) {
      printer <<< """  override def progress : |NEXT_UC|_DeclarationLevelSpecification"""
      printer <<< """}"""
    }
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_AccessLevelSpecification"""
    printer <<< """"""
    printer <<< """// can be used for accesses, e.g., in fields or function calls"""
    if (L4 == layer) {
      printer <<< """trait |LAYER_UC|_AccessLevelSpecification extends |LAYER_UC|_LevelSpecification"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """trait |LAYER_UC|_AccessLevelSpecification extends |LAYER_UC|_LevelSpecification {"""
    }
    if (L2 == layer) {
      printer <<< """  override def progress : |NEXT_UC|_AccessLevelSpecification"""
      printer <<< """}"""
    }
    if (L3 == layer) {
      printer <<< """  override def progress : |NEXT_UC|_AccessLevelSpecification"""
      printer <<< """}"""
    }
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_ResolveLevelSpecifications"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_ResolveLevelSpecifications extends DefaultStrategy("Resolve level specifications") {"""
    printer <<< """  // resolve level identifiers "coarsest", "finest""""
    printer <<< """  this += new Transformation("Resolve first batch of level aliases", {"""
    printer <<< """    case |LAYER_UC|_CoarsestLevel => |LAYER_UC|_SingleLevel(Knowledge.minLevel)"""
    printer <<< """    case |LAYER_UC|_FinestLevel   => |LAYER_UC|_SingleLevel(Knowledge.maxLevel)"""
    printer <<< """    case |LAYER_UC|_AllLevels     => |LAYER_UC|_LevelRange(|LAYER_UC|_SingleLevel(Knowledge.minLevel), |LAYER_UC|_SingleLevel(Knowledge.maxLevel))"""
    printer <<< """  })"""
    printer <<< """"""
    printer <<< """  // resolve relative level identifiers"""
    printer <<< """  this += new Transformation("Resolve relative level specifications", {"""
    printer <<< """    case |LAYER_UC|_RelativeLevel(|LAYER_UC|_SingleLevel(level), "+", offset) => |LAYER_UC|_SingleLevel(level + offset)"""
    printer <<< """    case |LAYER_UC|_RelativeLevel(|LAYER_UC|_SingleLevel(level), "-", offset) => |LAYER_UC|_SingleLevel(level - offset)"""
    printer <<< """    case level : |LAYER_UC|_RelativeLevel                             => Logger.error("Unsupported variant of |LAYER_UC|_RelativeLevel found: " + level)"""
    printer <<< """  })"""
    printer <<< """"""
    printer <<< """  // convert level ranges to level lists"""
    printer <<< """  this += new Transformation("Resolve level ranges", {"""
    printer <<< """    case |LAYER_UC|_LevelRange(|LAYER_UC|_SingleLevel(begin), |LAYER_UC|_SingleLevel(end)) => |LAYER_UC|_LevelList((begin to end).map(|LAYER_UC|_SingleLevel).toList)"""
    printer <<< """    case levels : |LAYER_UC|_LevelRange                                    => Logger.error("Unsupported variant of |LAYER_UC|_LevelRange found: " + levels)"""
    printer <<< """  })"""
    printer <<< """"""
    printer <<< """  // flatten level lists and incorporate negated level lists"""
    printer <<< """  this += new Transformation("Process level lists", {"""
    printer <<< """    case levels : |LAYER_UC|_LevelList =>"""
    printer <<< """      // resolve lists of lists"""
    printer <<< """      levels.flatten()"""
    printer <<< """"""
    printer <<< """      // resolve negations/ level exclusions"""
    printer <<< """      // TODO: will this work if elements of x occurs multiple times?"""
    printer <<< """      levels.levels.foreach {"""
    printer <<< """        case elem @ |LAYER_UC|_NegatedLevelList(|LAYER_UC|_LevelList(x)) =>"""
    printer <<< """          levels.levels --= x"""
    printer <<< """          levels.levels.remove(elem)"""
    printer <<< """        case _                                           =>"""
    printer <<< """      }"""
    printer <<< """"""
    printer <<< """      levels"""
    printer <<< """  })"""
    printer <<< """}"""
    printer <<< """"""
    if (L2 == layer || L3 == layer) {
      printer <<< """/// |LAYER_UC|_ResolveRelativeLevels"""
    }
    if (L4 == layer) {
      printer <<< """/// |LAYER_UC|_ResolveCurrentLevels"""
    }
    printer <<< """"""
    if (L4 == layer) {
      printer <<< """object |LAYER_UC|_ResolveCurrentLevels extends DefaultStrategy("Resolve current level references") {"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """object |LAYER_UC|_ResolveRelativeLevels extends DefaultStrategy("Resolve relative level specifications") {"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """  val collector = new |LAYER_UC|_LevelCollector"""
    }
    if (L4 == layer) {
      printer <<< """  var levelCollector = new L4LevelCollector"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """  this.register(collector)"""
    }
    if (L4 == layer) {
      printer <<< """  this.register(levelCollector)"""
    }
    printer <<< """"""
    if (L4 == layer) {
      printer <<< """  // resolve level specifications"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """  def getLevel() : Int = {"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """    if (collector.inLevelScope)"""
    }
    if (L4 == layer) {
      printer <<< """  this += new Transformation("Resolve relative level specifications", {"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """      collector.getCurrentLevel"""
    }
    if (L4 == layer) {
      printer <<< """    case |LAYER_UC|_CurrentLevel => |LAYER_UC|_SingleLevel(levelCollector.getCurrentLevel)"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """    else"""
    }
    if (L4 == layer) {
      printer <<< """    case |LAYER_UC|_CoarserLevel => |LAYER_UC|_SingleLevel(levelCollector.getCurrentLevel - 1)"""
    }
    if (L4 == layer) {
      printer <<< """    case |LAYER_UC|_FinerLevel   => |LAYER_UC|_SingleLevel(levelCollector.getCurrentLevel + 1)"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """      Logger.error("Trying to access current outside of a valid level scope")"""
    }
    if (L2 == layer) {
      printer <<< """  }"""
      printer <<< """"""
      printer <<< """  // resolve level identifiers "coarsest", "finest""""
      printer <<< """  this += new Transformation("Resolve relative level aliases", {"""
      printer <<< """    case |LAYER_UC|_CurrentLevel => |LAYER_UC|_SingleLevel(getLevel())"""
      printer <<< """    case |LAYER_UC|_CoarserLevel => |LAYER_UC|_SingleLevel(getLevel() - 1)"""
      printer <<< """    case |LAYER_UC|_FinerLevel   => |LAYER_UC|_SingleLevel(getLevel() + 1)"""
    }
    if (L3 == layer) {
      printer <<< """  }"""
      printer <<< """"""
      printer <<< """  // resolve level identifiers "coarsest", "finest""""
      printer <<< """  this += new Transformation("Resolve relative level aliases", {"""
      printer <<< """    case |LAYER_UC|_CurrentLevel => |LAYER_UC|_SingleLevel(getLevel())"""
      printer <<< """    case |LAYER_UC|_CoarserLevel => |LAYER_UC|_SingleLevel(getLevel() - 1)"""
      printer <<< """    case |LAYER_UC|_FinerLevel   => |LAYER_UC|_SingleLevel(getLevel() + 1)"""
    }
    printer <<< """  })"""
    printer <<< """}"""
    if (!(L2 == layer || L3 == layer)) {
    if (L4 == layer) {
      printer <<< """"""
      printer <<< """/// |LAYER_UC|_ReplaceExplicitLevelsWithCurrent"""
      printer <<< """"""
      printer <<< """object |LAYER_UC|_ReplaceExplicitLevelsWithCurrent extends QuietDefaultStrategy("Replace explicit levels with CurrentLevel, CoarserLevel and FinerLevel") {"""
      printer <<< """  var curLevel : Int = 0"""
      printer <<< """"""
      printer <<< """  this += new Transformation("Replace", {"""
      printer <<< """    case |LAYER_UC|_SingleLevel(level) if level == curLevel     => |LAYER_UC|_CurrentLevel"""
      printer <<< """    case |LAYER_UC|_SingleLevel(level) if level == curLevel - 1 => |LAYER_UC|_CoarserLevel"""
      printer <<< """    case |LAYER_UC|_SingleLevel(level) if level == curLevel + 1 => |LAYER_UC|_FinerLevel"""
      printer <<< """  })"""
      printer <<< """}"""
    }
    }
    printer.toString
  }
}
