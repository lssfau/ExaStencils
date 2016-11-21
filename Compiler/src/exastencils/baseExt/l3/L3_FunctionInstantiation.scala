package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger._
import exastencils.prettyprinting._

/// L3_FunctionInstantiation

object L3_FunctionInstantiation {
  def apply(templateName : String, args : List[L3_Expression], targetFct : String, targetFctLevel : Option[L3_DeclarationLevelSpecification]) : L3_FunctionInstantiation =
    L3_FunctionInstantiation(templateName, args.to[ListBuffer], targetFct, targetFctLevel)
}

case class L3_FunctionInstantiation(
    var templateName : String,
    var args : ListBuffer[L3_Expression],
    var targetFct : String,
    var targetFctLevel : Option[L3_DeclarationLevelSpecification]) extends L3_Statement {

  override def prettyprint(out : PpStream) = ???
  override def progress = ???
}

/// L3_ResolveFunctionTemplates

object L3_ResolveFunctionTemplates extends DefaultStrategy("Resolving function templates and instantiations") {
  this += new Transformation("Find and resolve", {
    case functionInst : L3_FunctionInstantiation =>
      val template = StateManager.findFirst({ fctTemp : L3_FunctionTemplate => fctTemp.name == functionInst.templateName })
      if (template.isEmpty) Logger.warn(s"Trying to instantiate unknown function template ${ functionInst.templateName }")
      val instantiated = Duplicate(L3_Function(functionInst.targetFct, functionInst.targetFctLevel,
        template.get.returntype, template.get.functionArgs, template.get.statements))

      L3_ReplaceExpressions.replacements = (template.get.templateArgs zip functionInst.args).toMap[String, L3_Expression]
      L3_ReplaceExpressions.applyStandalone(instantiated)

      instantiated
  })

  this += new Transformation("Remove function templates", {
    case functionTemplate : L3_FunctionTemplate => None
  })

  object L3_ReplaceExpressions extends DefaultStrategy("Replace something with something else") {
    var replacements : Map[String, L3_Expression] = Map()

    override def applyStandalone(node : Node) = {
      val oldLvl = Logger.getLevel
      Logger.setLevel(Logger.WARNING)
      super.applyStandalone(node)
      Logger.setLevel(oldLvl)
    }

    this += new Transformation("SearchAndReplace", {
      case origAccess : L3_UnresolvedAccess if replacements.exists(_._1 == origAccess.name) =>
        // includes accesses used as identifiers in function calls
        val newAccess = Duplicate(replacements(origAccess.name))
        newAccess match {
          case newAccess : L3_UnresolvedAccess =>
            if (origAccess.level.isDefined) {
              if (newAccess.level.isDefined) Logger.warn("Overriding level on access in function instantiation")
              newAccess.level = origAccess.level
            }

          case _ =>
        }
        newAccess
    })
  }

}
