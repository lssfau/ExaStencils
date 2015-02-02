package exastencils.optimization

import scala.collection.mutable.Map

import exastencils.core._
import exastencils.core.collectors.ScopeCollector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.logger._

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

private final class AnnotateStringConstants extends ScopeCollector(Map[String, Datatype]()) {
  import TypeInference._

  override def cloneCurScope() : Map[String, Datatype] = {
    return curScope.clone()
  }

  private def declare(name : String, dType : Datatype) : Unit = {
    curScope(name) = dType
  }

  private def findType(name : String) : Datatype = {
    val typeOp : Option[Datatype] = curScope.get(name)
    if (typeOp.isDefined)
      return typeOp.get
    return null // not found :(
  }

  override def enter(node : Node) : Unit = {
    super.enter(node)

    node match {
      case VariableDeclarationStatement(ty : Datatype, name : String, _) =>
        declare(name, ty)

      case node @ StringConstant(str) =>
        val ty : Datatype = findType(str)
        if (ty != null)
          node.annotate(TYPE_ANNOT, ty)

      case node @ VariableAccess(name, None) =>
        val ty : Datatype = findType(name)
        if (ty != null)
          node.annotate(TYPE_ANNOT, ty)
        else
          Logger.warn("[Type inference]  declaration to " + name + " missing?")

      case VariableAccess(name, Some(ty)) =>
        val inferred = findType(name)
        if (inferred == null)
          Logger.warn("[Type inference]  declaration to " + name + " missing?")
        else if (ty != inferred)
          Logger.warn("[Type inference]  inferred type (" + inferred + ") different from actual type stored in node (" + ty + "); ignoring")

      case FunctionStatement(_, _, params, _) =>
        for (param <- params)
          declare(param.name, param.dType.get)

      // HACK: ensure the iterator declaration is visited before the body...
      case ForLoopStatement(begin, _, _, _, _) =>
        this.enter(begin)

      case _ =>
    }
  }
}

private final object CreateVariableAccesses extends PartialFunction[Node, Transformation.OutputType] {
  import TypeInference._

  def isDefinedAt(node : Node) : Boolean = {
    return (node.isInstanceOf[StringConstant] || node.isInstanceOf[VariableAccess]) && node.hasAnnotation(TYPE_ANNOT)
  }

  def apply(node : Node) : Transformation.OutputType = {

    // do not remove annotation as the same object could be used multiple times in AST (which is a bug, yes ;))
    val typee : Datatype = node.getAnnotation(TYPE_ANNOT).get.value.asInstanceOf[Datatype]
    val varr : String =
      node match {
        case StringConstant(name)    => name
        case VariableAccess(name, _) => name
      }
    return VariableAccess(varr, Some(typee))
  }
}

