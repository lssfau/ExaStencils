package exastencils.baseExt.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.languageprocessing.l4.ReplaceExpressions
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
      var instantiated = Duplicate(L4_Function(functionInst.targetFct, template.returntype, template.functionArgs, template.statements))

      ReplaceExpressions.replacements = Map() ++ (template.templateArgs zip functionInst.args).toMap[String, L4_Expression]
      ReplaceExpressions.applyStandalone(instantiated)
      StateManager.root.asInstanceOf[Root].functions += instantiated

      None // consume instantiation
    }
  })

  this += new Transformation("Remove function templates", {
    case functionTemplate : L4_FunctionTemplate => None
  })
}
