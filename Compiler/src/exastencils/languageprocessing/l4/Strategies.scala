package exastencils.languageprocessing.l4

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.L4CommCollector
import exastencils.core.collectors.L4ValueCollector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.l4._
import exastencils.knowledge
import exastencils.logger._

object CollectCommInformation extends DefaultStrategy("Collecting information relevant for adding communication statements") {
  var commCollector : L4CommCollector = new L4CommCollector(HashMap())

  override def apply(node : Option[Node] = None) = {
    commCollector.reset()
    this.register(commCollector)
    super.apply(node)
    this.unregister(commCollector)
  }

  override def applyStandalone(node : Node) = {
    commCollector.reset()
    this.register(commCollector)
    super.applyStandalone(node)
    this.unregister(commCollector)
  }

  this += new Transformation("Collect", { // FIXME: add visitor strategy defining dummy trafo?
    case n : Node => n
  })
}

object ResolveL4 extends DefaultStrategy("Resolving L4 specifics") {
  val specialFields : ListBuffer[String] = ListBuffer(
    "vf_nodePosition_x", "vf_nodePosition_y", "vf_nodePosition_z",
    "nodePosition_x", "nodePosition_y", "nodePosition_z",

    "vf_gridWidth_x", "vf_gridWidth_y", "vf_gridWidth_z",
    "gridWidth_x", "gridWidth_y", "gridWidth_z",

    "vf_cellWidth_x", "vf_cellWidth_y", "vf_cellWidth_z",
    "cellWidth_x", "cellWidth_y", "cellWidth_z",

    "vf_stagCVWidth_x", "vf_stagCVWidth_y", "vf_stagCVWidth_z",
    "stagCVWidth_x", "stagCVWidth_y", "stagCVWidth_z",

    "vf_cellVolume", "vf_xStagCellVolume", "vf_yStagCellVolume", "vf_zStagCellVolume",
    "cellVolume", "xStagCellVolume", "yStagCellVolume", "zStagCellVolume",

    "vf_cellCenterToFace_x", "vf_cellCenterToFace_y", "vf_cellCenterToFace_z").map(_.toLowerCase())

  override def apply(applyAtNode : Option[Node]) = {
    this.transaction()
    var valueCollector = new L4ValueCollector

    // resolve values in expressions by replacing them with their expression => let SimplifyStrategy do the work
    this.register(valueCollector)
    //    this.execute(new Transformation("Put Values into collector", {
    //      case x : ValueDeclarationStatement => x
    //    }))

    this.execute(new Transformation("ResolveValuesInExpressions", {
      case x : UnresolvedAccess if (x.level == None && x.slot == None && x.arrayIndex == None) => {
        var value = valueCollector.getValue(x.name)
        value match {
          case None => { Logger.info(s"""Could not resolve identifier ${x.name} as no matching Val was found"""); x }
          case _    => value.get
        }
      }
      case x : UnresolvedAccess if (x.level.isDefined && x.level.get.isInstanceOf[SingleLevelSpecification] && x.slot == None && x.arrayIndex == None) => {
        var value = valueCollector.getValue(x.name + "_" + x.level.get.asInstanceOf[SingleLevelSpecification].level)
        value match {
          case None => { Logger.info(s"""Could not resolve identifier ${x.name} as no matching Val was found"""); x }
          case _    => value.get
        }
      }
    }))
    this.unregister(valueCollector)

    // resolve accesses
    this.execute(new Transformation("ResolveAccessSpecifications", {
      case access : UnresolvedAccess =>
        if (StateManager.root_.asInstanceOf[Root].fields.exists(f => access.name == f.identifier.name))
          access.resolveToFieldAccess
        else if (specialFields.contains(access.name.toLowerCase()))
          access.resolveToSpecialFieldAccess
        else if (StateManager.root_.asInstanceOf[Root].stencils.exists(s => access.name == s.identifier.name))
          access.resolveToStencilAccess
        else if (StateManager.root_.asInstanceOf[Root].stencilFields.exists(s => access.name == s.identifier.name))
          access.resolveToStencilFieldAccess
        else access.resolveToBasicOrLeveledAccess
    }))

    this.execute(new Transformation("special functions and constants", {
      // levelIndex
      case FunctionCallExpression(LeveledAccess("levels", SingleLevelSpecification(level)), List())      => IntegerConstant(level)
      case FunctionCallExpression(LeveledAccess("levelIndex", SingleLevelSpecification(level)), List())  => IntegerConstant(level - knowledge.Knowledge.minLevel)
      case FunctionCallExpression(LeveledAccess("levelString", SingleLevelSpecification(level)), List()) => StringConstant(level.toString())

      // constants
      case BasicAccess("PI") | BasicAccess("M_PI") | BasicAccess("Pi")                                   => FloatConstant(math.Pi)
    }))

    this.commit()
  }
}

object ReplaceExpressions extends DefaultStrategy("Replace something with something else") {
  var replacements : Map[String, Expression] = Map()

  override def applyStandalone(node : Node) = {
    val oldLvl = Logger.getLevel
    Logger.setLevel(Logger.WARNING)
    super.applyStandalone(node)
    Logger.setLevel(oldLvl)
  }

  this += new Transformation("SearchAndReplace", {
    case origAccess : UnresolvedAccess if replacements.exists(_._1 == origAccess.name) => {
      // includes accesses used as identifiers in function calls
      var newAccess = Duplicate(replacements.get(origAccess.name).get)
      newAccess match {
        case newAccess : UnresolvedAccess => {
          if (origAccess.slot.isDefined) {
            if (newAccess.slot.isDefined) Logger.warn("Overriding slot on access in function instantiation")
            newAccess.slot = origAccess.slot
          }
          if (origAccess.level.isDefined) {
            if (newAccess.level.isDefined) Logger.warn("Overriding level on access in function instantiation")
            newAccess.level = origAccess.level
          }
          if (origAccess.offset.isDefined) {
            if (newAccess.offset.isDefined) Logger.warn("Overriding offset on access in function instantiation")
            newAccess.offset = origAccess.offset
          }
          if (origAccess.arrayIndex.isDefined) {
            if (newAccess.arrayIndex.isDefined) Logger.warn("Overriding array index on access in function instantiation")
            newAccess.arrayIndex = origAccess.arrayIndex
          }
          if (origAccess.dirAccess.isDefined) {
            if (newAccess.dirAccess.isDefined) Logger.warn("Overriding direction access on access in function instantiation")
            newAccess.dirAccess = origAccess.dirAccess
          }
        }
        case _ =>
      }
      newAccess
    }
  })
}

object ResolveFunctionTemplates extends DefaultStrategy("Resolving function templates and instantiations") {
  this += new Transformation("Find and resolve", {
    case functionInst : FunctionInstantiationStatement => {
      val template = StateManager.root.asInstanceOf[Root].functionTemplates.find(_.name == functionInst.templateName)
      if (template.isEmpty) Logger.warn(s"Trying to instantiate unknown function template ${functionInst.templateName}")
      var instantiated = Duplicate(FunctionStatement(functionInst.targetFct, template.get.returntype, template.get.functionArgs, template.get.statements))

      ReplaceExpressions.replacements = (template.get.templateArgs zip functionInst.args).toMap[String, Expression]
      ReplaceExpressions.applyStandalone(instantiated)

      instantiated
    }
  })

  this += new Transformation("Remove function templates", {
    case root : Root =>
      root.functionTemplates.clear; root
    case functionTemplate : FunctionTemplateStatement => None
  })
}

object WrapL4FieldOpsStrategy extends DefaultStrategy("Adding communcation and loops to L4 statements") {
  this += new Transformation("Search and wrap", {
    case assignment @ AssignmentStatement(lhs : FieldAccess, rhs, op) => {
      CollectCommInformation.applyStandalone(assignment)

      var statements : ListBuffer[Statement] =
        CollectCommInformation.commCollector.communicates.map(comm =>
          CommunicateStatement(comm._1, "both", List( /* FIXME: add radius */ )) : Statement).to[ListBuffer]

      statements += LoopOverFragmentsStatement(List(
        LoopOverPointsStatement(lhs, None, false, None, None, None, None, List(assignment), None)),
        None)

      statements
    }

    // FIXME: handle reductions
    // FIXME: handle stencil fields
    // FIXME: handle region loops
  }, false /* recursion must be switched of due to wrapping mechanism */ )
}
