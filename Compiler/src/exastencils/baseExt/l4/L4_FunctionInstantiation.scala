//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_FunctionInstantiation

object L4_FunctionInstantiation {
  def apply(templateName : String, args : List[L4_Expression], targetFct : String, targetFctLevel : Option[L4_DeclarationLevelSpecification]) : L4_FunctionInstantiation =
    L4_FunctionInstantiation(templateName, args.to[ListBuffer], targetFct, targetFctLevel)
}

case class L4_FunctionInstantiation(
    var templateName : String,
    var args : ListBuffer[L4_Expression],
    var targetFct : String,
    var targetFctLevel : Option[L4_DeclarationLevelSpecification]) extends L4_Node with PrettyPrintable {

  override def prettyprint(out : PpStream) = {
    out << "Instantiate " << templateName << " < " <<< (args, ", ") << " > " << " as " << targetFct
    if (targetFctLevel.isDefined) out << '@' << targetFctLevel.get
  }
}

/// L4_ResolveFunctionInstantiations

object L4_ResolveFunctionInstantiations extends DefaultStrategy("Resolving function templates and instantiations") {
  this += new Transformation("Find and resolve", {
    case functionInst : L4_FunctionInstantiation =>
      val templateOpt = StateManager.findFirst({ f : L4_FunctionTemplate => f.name == functionInst.templateName })
      if (templateOpt.isEmpty) Logger.warn(s"Trying to instantiate unknown function template ${ functionInst.templateName }")
      val template = templateOpt.get
      val instantiated = Duplicate(L4_FunctionDecl(functionInst.targetFct, functionInst.targetFctLevel,
        template.datatype, template.functionArgs, template.statements))

      L4_ReplaceUnresolvedAccess.replacements = Map() ++ (template.templateArgs zip functionInst.args).toMap[String, L4_Expression]
      L4_ReplaceUnresolvedAccess.applyStandalone(instantiated)

      instantiated // replace instantiation with function declaration
  })

  this += new Transformation("Remove function templates", {
    case _ : L4_FunctionTemplate => None
  })

  object L4_ReplaceUnresolvedAccess extends QuietDefaultStrategy("Replace something with something else") {
    var replacements : Map[String, L4_Expression] = Map()

    this += new Transformation("Search and replace", {
      case origAccess : L4_UnresolvedAccess if replacements.exists(_._1 == origAccess.name) =>
        val newAccess = Duplicate(replacements(origAccess.name))
        newAccess match {
          case newAccess : L4_UnresolvedAccess =>
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
            if (origAccess.matIndex.isDefined) {
              if (newAccess.matIndex.isDefined) Logger.warn("Overriding array index on access in function instantiation")
              newAccess.matIndex = origAccess.matIndex
            }
            if (origAccess.dirAccess.isDefined) {
              if (newAccess.dirAccess.isDefined) Logger.warn("Overriding direction access on access in function instantiation")
              newAccess.dirAccess = origAccess.dirAccess
            }

          case _ =>
        }
        newAccess

      case origRef : L4_UnresolvedFunctionReference if replacements.exists(_._1 == origRef.name) =>
        replacements(origRef.name) match {
          case access : L4_UnresolvedAccess =>
            val newRef = L4_UnresolvedFunctionReference(access.name, Duplicate(access.level), Duplicate(access.offset))

            if (origRef.level.isDefined) {
              if (newRef.level.isDefined) Logger.warn("Overriding level on reference in function instantiation")
              newRef.level = origRef.level
            }

            if (origRef.offset.isDefined) {
              if (newRef.offset.isDefined) Logger.warn("Overriding offset on reference in function instantiation")
              newRef.offset = origRef.offset
            }

            if (access.slot.isDefined) Logger.warn("Ignoring slot on access in function instantiation")
            if (access.matIndex.isDefined) Logger.warn("Ignoring mat index on access in function instantiation")
            if (access.dirAccess.isDefined) Logger.warn("Ignoring direction access on access in function instantiation")

            newRef

          case _ => ???
        }
    })
  }

}
