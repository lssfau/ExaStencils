package exastencils.baseExt.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_FunctionInstantiation

object L4_FunctionInstantiation {
  def apply(templateName : String, args : List[L4_Expression], targetFct : L4_Identifier) =
    new L4_FunctionInstantiation(templateName, args.to[ListBuffer], targetFct)
}

case class L4_FunctionInstantiation(var templateName : String, args : ListBuffer[L4_Expression], targetFct : L4_Identifier) extends L4_Node with PrettyPrintable {
  override def prettyprint(out : PpStream) = {
    out << "Instantiate " << templateName << " < " <<< (args, ", ") << " > "
    out << " as " << targetFct << "\n"
  }
}

/// L4_ResolveFunctionInstantiations

object L4_ResolveFunctionInstantiations extends DefaultStrategy("Resolving function templates and instantiations") {
  this += new Transformation("Find and resolve", {
    case functionInst : L4_FunctionInstantiation => {
      val templateOpt = StateManager.findFirst({ f : L4_FunctionTemplate => f.name == functionInst.templateName })
      if (templateOpt.isEmpty) Logger.warn(s"Trying to instantiate unknown function template ${ functionInst.templateName }")
      val template = templateOpt.get
      val instantiated = Duplicate(L4_Function(functionInst.targetFct, template.returntype, template.functionArgs, template.statements))

      L4_ReplaceUnresolvedAccess.replacements = Map() ++ (template.templateArgs zip functionInst.args).toMap[String, L4_Expression]
      L4_ReplaceUnresolvedAccess.applyStandalone(instantiated)

      instantiated // replace instantiation with function declaration
    }
  })

  this += new Transformation("Remove function templates", {
    case functionTemplate : L4_FunctionTemplate => None
  })

  object L4_ReplaceUnresolvedAccess extends DefaultStrategy("Replace something with something else") {
    var replacements : Map[String, L4_Expression] = Map()

    override def applyStandalone(node : Node) = {
      val oldLvl = Logger.getLevel
      Logger.setLevel(Logger.WARNING)
      super.applyStandalone(node)
      Logger.setLevel(oldLvl)
    }

    this += new Transformation("Search and replace", {
      case origAccess : L4_UnresolvedAccess if replacements.exists(_._1 == origAccess.name) => {
        // includes accesses used as identifiers in function calls
        val newAccess = Duplicate(replacements(origAccess.name))
        newAccess match {
          case newAccess : L4_UnresolvedAccess => {
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
          case _                               =>
        }
        newAccess
      }
    })
  }

}
