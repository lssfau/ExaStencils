package exastencils.strategies

import scala.collection.mutable.{ Node => _, _ }

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.core._
import exastencils.core.collectors.StatementCollector
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir._

object FortranifyFunctionsInsideStatement extends QuietDefaultStrategy("Looking for function inside statements") {
  val collector = new StatementCollector

  var functionsToBeProcessed : HashMap[String, ListBuffer[(Int, IR_Datatype)]] = HashMap()
  var callByValReplacements : HashMap[String, Statement] = HashMap()

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
    case fct : FunctionCallExpression if (collector.insideStatement <= 1 && functionsToBeProcessed.contains(fct.name)) =>
      // adapt call arguments
      for ((paramIdx, datatype) <- functionsToBeProcessed.get(fct.name).get) {
        fct.arguments(paramIdx) match {
          // variable accesses are simple
          case va : VariableAccess =>
            fct.arguments(paramIdx) = IR_AddressofExpression(fct.arguments(paramIdx))
          // otherwise temp variables have to be created
          case _ =>
            var newName = s"callByValReplacement_${ fct.name }_${ paramIdx.toString() }"
            while (callByValReplacements.contains(newName)) newName += "0"
            callByValReplacements += (newName -> VariableDeclarationStatement(Duplicate(datatype), newName, Some(fct.arguments(paramIdx))))
            fct.arguments(paramIdx) = IR_AddressofExpression(VariableAccess(newName, Some(Duplicate(datatype))))
        }
      }

      // adapt name
      fct.name = fct.name.toLowerCase() + "_"

      // return
      fct
  })
}

object Fortranify extends DefaultStrategy("Preparing function for fortran interfacing") {
  // map of function names and list of parameter indices and data types to be wrapped by address operations
  var functionsToBeProcessed : HashMap[String, ListBuffer[(Int, IR_Datatype)]] = HashMap()

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
    case functions : FunctionCollection =>
      for (abstrFct <- functions.functions) {
        val fct = abstrFct.asInstanceOf[FunctionStatement] // assume resolved function declarations
        if (fct.allowFortranInterface && isTreatableFunction(fct.name)) {
          // remember for later usage
          functionsToBeProcessed += (fct.name -> ListBuffer())

          // adapt parameters
          var paramIdx = 0
          for (param <- fct.parameters) {
            param.datatype match {
              case IR_PointerDatatype(_)  => // already fortran compliant
              case IR_ArrayDatatype(_, _) => // should also be fortran compliant
              case datatype               => {
                // remember for later usage
                functionsToBeProcessed.get(fct.name).get += ((paramIdx, datatype))

                // redirect parameter
                fct.body.prepend(
                  VariableDeclarationStatement(
                    IR_ReferenceDatatype(Duplicate(datatype)), param.name,
                    Some(DerefAccess(VariableAccess(param.name + "_ptr", Some(IR_PointerDatatype(datatype)))))))
                param.name += "_ptr"
                param.datatype = IR_PointerDatatype(Duplicate(datatype))
              }
            }
            paramIdx += 1
          }

          // adapt name
          fct.name = fct.name.toLowerCase() + "_"
        }
      }
      functions
  })

  this += new Transformation("Prepending underscores to function calls", {
    case s : Statement if !s.isInstanceOf[FunctionStatement] =>
      FortranifyFunctionsInsideStatement.functionsToBeProcessed = functionsToBeProcessed
      FortranifyFunctionsInsideStatement.applyStandalone(s)

      if (FortranifyFunctionsInsideStatement.callByValReplacements.isEmpty)
        s
      else
        Scope(FortranifyFunctionsInsideStatement.callByValReplacements.map(_._2).to[ListBuffer] ++ ListBuffer[Statement](s))
  })
}
