package exastencils.optimization

import scala.collection.mutable.ArrayStack
import scala.collection.mutable.HashMap

import exastencils.core.Logger
import exastencils.core.StateManager
import exastencils.core.collectors.Collector
import exastencils.datastructures.CustomStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._

object TypeInference extends CustomStrategy("Type inference") {

  private[optimization] final val TYPE_ANNOT = "InfType"

  override def apply() : Unit = {

    this.transaction()
    Logger.info("Applying strategy " + name)

    val annotate = new AnnotateStringConstants()
    StateManager.register(annotate)
    this.execute(new Transformation("infer types", PartialFunction.empty))
    StateManager.unregister(annotate)

    this.execute(new Transformation("replace nodes", CreateVariableAccesses))

    this.commit()
  }
}

private final class AnnotateStringConstants extends Collector {
  import TypeInference._

  this.reset()

  private final val START_COND_BLOCK_ANNOT = "FirstElse"

  private object SymbolTable {

    private val scopes = new ArrayStack[HashMap[String, Datatype]]()

    def declare(name : String, dType : Datatype) : Unit = {
      scopes.top.put(name, dType)
    }

    def findType(name : String) : Datatype = {
      for (scope <- scopes) {
        val typeOp : Option[Datatype] = scope.get(name)
        if (typeOp.isDefined)
          return typeOp.get
      }
      return null // not found :(
    }

    def enterScope() : Unit = {
      scopes.push(new HashMap[String, Datatype]())
    }

    def leaveScope() : Unit = {
      scopes.pop()
    }

    def reset() : Unit = {
      scopes.clear()
      this.enterScope() // for global scope
    }
  }

  def enter(node : Node) : Unit = {

    handleScopes(node, true)

    node match {
      case VariableDeclarationStatement(ty : Datatype, name : String, _) =>
        SymbolTable.declare(name, ty)

      case node @ StringConstant(str) =>
        val ty : Datatype = SymbolTable.findType(str)
        if (ty != null)
          node.annotate(TYPE_ANNOT, ty)

      case node @ VariableAccess(name, None) =>
        val ty : Datatype = SymbolTable.findType(name)
        if (ty != null)
          node.annotate(TYPE_ANNOT, ty)

      case VariableAccess(name, Some(ty)) =>
        val inferred = SymbolTable.findType(name)
        if (inferred == null)
          Logger.warn("[Type inference]  declaration to " + name + " missing?")
        else if (ty != inferred)
          Logger.warn("[Type inference]  inferred type (" + inferred + ") different from actual type stored in node (" + ty + "); ignoring")

      case FunctionStatement(_, _, params, _) =>
        for (param <- params)
          SymbolTable.declare(param.name, param.dType.get)

      // HACK: ensure the iterator declaration is visited before the body...
      case ForLoopStatement(begin, _, _, _, _) =>
        this.enter(begin)

      case _ =>
    }
  }

  def leave(node : Node) : Unit = {
    handleScopes(node, false)
  }

  def reset() : Unit = {
    SymbolTable.reset()
  }

  private def handleScopes(node : Node, isEnter : Boolean) : Unit = {

    if (node.removeAnnotation(START_COND_BLOCK_ANNOT).isDefined) { // HACK: check for "switch-info"
      SymbolTable.leaveScope()
      SymbolTable.enterScope()
    }

    node match {
      case ConditionStatement(_, trueBody, falseBody) =>
        if (isEnter) {
          SymbolTable.enterScope()
          if (!trueBody.isEmpty) // HACK: add "switch-info"
            trueBody(0).annotate(START_COND_BLOCK_ANNOT)
          if (!falseBody.isEmpty) // HACK: add "switch-info"
            falseBody(0).annotate(START_COND_BLOCK_ANNOT)

        } else
          SymbolTable.leaveScope()

      case _ : Scope
        | _ : ForLoopStatement
        | _ : WhileLoopStatement
        | _ : FunctionStatement
        | _ : SwitchStatement
        | _ : CaseStatement =>

        if (isEnter)
          SymbolTable.enterScope()
        else
          SymbolTable.leaveScope()

      case _ =>
    }
  }
}

private final object CreateVariableAccesses extends PartialFunction[Node, Transformation.Output[_]] {
  import TypeInference._

  def isDefinedAt(node : Node) : Boolean = {
    return (node.isInstanceOf[StringConstant] || node.isInstanceOf[VariableAccess]) && node.hasAnnotation(TYPE_ANNOT)
  }

  def apply(node : Node) : Transformation.Output[_] = {

    val typee : Datatype = node.removeAnnotation(TYPE_ANNOT).get.value.asInstanceOf[Datatype]
    val varr : String =
      node match {
        case StringConstant(name)    => name
        case VariableAccess(name, _) => name
      }
    return VariableAccess(varr, Some(typee))
  }
}

