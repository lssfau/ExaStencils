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
    var targetFctLevel : Option[L3_DeclarationLevelSpecification]) extends L3_Node with PrettyPrintable {

  override def prettyprint(out : PpStream) = {
    out << "Instantiate " << templateName << " < " <<< (args, ", ") << " > " << " as " << targetFct
    if (targetFctLevel.isDefined) out << '@' << targetFctLevel.get
  }
}

/// L3_ResolveFunctionInstantiations

object L3_ResolveFunctionInstantiations extends DefaultStrategy("Resolving function templates and instantiations") {
  this += new Transformation("Find and resolve", {
    case functionInst : L3_FunctionInstantiation =>
      val templateOpt = StateManager.findFirst({ f : L3_FunctionTemplate => f.name == functionInst.templateName })
      if (templateOpt.isEmpty) Logger.warn(s"Trying to instantiate unknown function template ${ functionInst.templateName }")
      val template = templateOpt.get
      val instantiated = Duplicate(L3_FunctionDecl(functionInst.targetFct, functionInst.targetFctLevel,
        template.returnType, template.functionArgs, template.statements))

      L3_ReplaceUnresolvedAccess.replacements = Map() ++ (template.templateArgs zip functionInst.args).toMap[String, L3_Expression]
      L3_ReplaceUnresolvedAccess.applyStandalone(instantiated)

      instantiated // replace instantiation with function declaration
  })

  this += new Transformation("Remove function templates", {
    case _ : L3_FunctionTemplate => None
  })

  object L3_ReplaceUnresolvedAccess extends QuietDefaultStrategy("Replace something with something else") {
    var replacements : Map[String, L3_Expression] = Map()

    this += new Transformation("Search and replace", {
      case origAccess : L3_UnresolvedAccess if replacements.exists(_._1 == origAccess.name) =>
        val newAccess = Duplicate(replacements(origAccess.name))
        newAccess match {
          case newAccess : L3_UnresolvedAccess =>
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

          case _ =>
        }
        newAccess

      case origRef : L3_UnresolvedFunctionReference if replacements.exists(_._1 == origRef.name) =>
        replacements(origRef.name) match {
          case access : L3_UnresolvedAccess =>
            val newRef = L3_UnresolvedFunctionReference(access.name, Duplicate(access.level))

            if (origRef.level.isDefined) {
              if (newRef.level.isDefined) Logger.warn("Overriding level on reference in function instantiation")
              newRef.level = origRef.level
            }

            if (access.slot.isDefined) Logger.warn("Ignoring slot on access in function instantiation")
            if (access.offset.isDefined) Logger.warn("Ignoring offset on access in function instantiation")
            if (access.arrayIndex.isDefined) Logger.warn("Ignoring array index on access in function instantiation")
            if (access.dirAccess.isDefined) Logger.warn("Ignoring direction access on access in function instantiation")

            newRef

          case _ => ???
        }
    })
  }

}
