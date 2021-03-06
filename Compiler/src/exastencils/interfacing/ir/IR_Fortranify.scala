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

package exastencils.interfacing.ir

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.core.collectors._
import exastencils.datastructures._

/// IR_Fortranify

object IR_Fortranify extends DefaultStrategy("Prepare functions for fortran interfacing") {
  // map of function names and list of parameter indices and data types to be wrapped by address operations
  var functionsToBeProcessed = HashMap[String, ListBuffer[(Int, IR_Datatype)]]()

  def isTreatableFunction(name : String) = {
    name match {
      case "main" => false
      case _      => true
    }
  }

  override def apply(node : Option[Node] = None) = {
    functionsToBeProcessed.clear
    super.apply(node)
  }

  this += new Transformation("Process function declarations", {
    case functions : IR_FunctionCollection =>
      for (abstrFct <- functions.functions) {
        val fct = abstrFct.asInstanceOf[IR_Function] // assume resolved function declarations
        if (fct.allowFortranInterface && isTreatableFunction(fct.name)) {
          // remember for later usage
          functionsToBeProcessed += (fct.name -> ListBuffer())

          // adapt parameters
          var paramIdx = 0
          for (param <- fct.parameters) {
            param.datatype match {
              case IR_PointerDatatype(_)  => // already fortran compliant
              case IR_ArrayDatatype(_, _) => // should also be fortran compliant
              case datatype               =>
                // remember for later usage
                functionsToBeProcessed(fct.name) += ((paramIdx, datatype))

                // redirect parameter
                fct.body.prepend(
                  IR_VariableDeclaration(
                    IR_ReferenceDatatype(Duplicate(datatype)), param.name,
                    Some(IR_DerefAccess(IR_VariableAccess(param.name + "_ptr", IR_PointerDatatype(datatype))))))
                param.name += "_ptr"
                param.datatype = IR_PointerDatatype(Duplicate(datatype))
            }
            paramIdx += 1
          }

          // adapt name
          fct.name = fct.name.toLowerCase() + "_"
        }
      }
      functions
  })

  this += new Transformation("Prepend underscores to function calls", {
    case s : IR_Statement if !s.isInstanceOf[IR_Function] =>
      IR_FortranifyFunctionsInsideStatement.functionsToBeProcessed = functionsToBeProcessed
      IR_FortranifyFunctionsInsideStatement.applyStandalone(s)

      if (IR_FortranifyFunctionsInsideStatement.callByValReplacements.isEmpty)
        s
      else
        IR_Scope(IR_FortranifyFunctionsInsideStatement.callByValReplacements.values.to[ListBuffer] :+ s)
  })

  /// IR_FortranifyFunctionsInsideStatement

  object IR_FortranifyFunctionsInsideStatement extends QuietDefaultStrategy("Looking for function inside statements") {

    class IR_StatementCollector extends Collector {
      var insideStatement : Int = 0

      override def enter(node : Node) : Unit = {
        node match {
          case _ : IR_Statement => insideStatement += 1
          case _                =>
        }
      }

      override def leave(node : Node) : Unit = {
        node match {
          case _ : IR_Statement => insideStatement -= 1
          case _                =>
        }
      }

      override def reset() : Unit = {
        insideStatement = 0
      }
    }

    val collector = new IR_StatementCollector

    var functionsToBeProcessed : HashMap[String, ListBuffer[(Int, IR_Datatype)]] = HashMap()
    var callByValReplacements : HashMap[String, IR_Statement] = HashMap()

    override def apply(node : Option[Node]) = {
      callByValReplacements.clear
      register(collector)
      super.apply(node)
      unregister(collector)
    }

    override def applyStandalone(node : Node) = {
      callByValReplacements.clear
      register(collector)
      super.applyStandalone(node)
      unregister(collector)
    }

    this += new Transformation("", {
      case fct : IR_FunctionCall if collector.insideStatement <= 1 && functionsToBeProcessed.contains(fct.name) =>
        // adapt call arguments
        for ((paramIdx, datatype) <- functionsToBeProcessed(fct.name)) {
          fct.arguments(paramIdx) match {
            // variable accesses are simple
            case va : IR_VariableAccess =>
              fct.arguments(paramIdx) = IR_AddressOf(fct.arguments(paramIdx))
            // otherwise temp variables have to be created
            case _ =>
              var newName = s"callByValReplacement_${ fct.name }_${ paramIdx.toString }"
              while (callByValReplacements.contains(newName)) newName += "0"
              callByValReplacements += (newName -> IR_VariableDeclaration(Duplicate(datatype), newName, Some(fct.arguments(paramIdx))))
              fct.arguments(paramIdx) = IR_AddressOf(IR_VariableAccess(newName, Duplicate(datatype)))
          }
        }

        // adapt name
        fct.function.name = fct.function.name.toLowerCase() + "_"

        // return
        fct
    })
  }

}
