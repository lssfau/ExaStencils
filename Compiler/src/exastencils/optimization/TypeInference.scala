package exastencils.optimization

import scala.collection.mutable.Map

import exastencils.base.ir._
import exastencils.config.Settings
import exastencils.core.Duplicate
import exastencils.core.collectors.ScopeCollector
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.logger._

object TypeInference extends CustomStrategy("Type inference") {
  private[optimization] final val TYPE_ANNOT = "InfType"
  private[optimization] final val SKIP_ANNOT = "TypSkip"
  var warnMissingDeclarations : Boolean = false

  override def apply() : Unit = {
    this.transaction()

    Logger.info("Applying strategy " + name)
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    val annotate = new AnnotateStringConstants()
    this.register(annotate)
    this.execute(new Transformation("load global declarations first", PartialFunction.empty), Some(IR_GlobalCollection.get))
    this.execute(new Transformation("infer types", PartialFunction.empty))
    this.unregister(annotate)

    this.execute(new Transformation("replace nodes", CreateVariableAccesses))

    this.execute(new Transformation("remove annotations", {
      case node : Node =>
        node.removeAnnotation(TYPE_ANNOT)
        node.removeAnnotation(SKIP_ANNOT)
        node
    }))

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }
}

private final class AnnotateStringConstants extends ScopeCollector(Map[String, IR_Datatype]()) {

  import TypeInference._

  override def cloneCurScope() : Map[String, IR_Datatype] = {
    curScope.clone()
  }

  private def declare(name : String, datatype : IR_Datatype) : Unit = {
    curScope(name) = datatype
  }

  private def findType(name : String) : IR_Datatype = {
    curScope.getOrElse(name, null)
  }

  override def enter(node : Node) : Unit = {
    super.enter(node)

    if (node.removeAnnotation(SKIP_ANNOT).isDefined)
      return

    node match {
      case IR_VariableDeclaration(ty : IR_Datatype, name : String, _) =>
        declare(name, ty)

      case node @ IR_StringLiteral(str) =>
        val ty : IR_Datatype = findType(str)
        if (ty != null)
          node.annotate(TYPE_ANNOT, ty)

      case node @ IR_VariableAccess(name, IR_UnknownDatatype) =>
        val ty : IR_Datatype = findType(name)
        if (ty != null)
          node.annotate(TYPE_ANNOT, ty)
        else if (warnMissingDeclarations)
          Logger.warn("[Type inference]  declaration to " + name + " missing?")

      case IR_VariableAccess(name, ty) =>
        val inferred = findType(name)
        if (inferred == null) {
          if (warnMissingDeclarations)
            Logger.warn("[Type inference]  declaration to " + name + " missing?")
        } else if (ty != inferred)
          Logger.warn("[Type inference]  inferred type (" + inferred + ") different from actual type stored in node (" + ty + "); ignoring")

      case IR_Function(_, _, params, _, _, _, _) =>
        for (param <- params)
          declare(param.name, param.datatype)

      // HACK: ensure the iterator declaration is visited before the body...
      case IR_ForLoop(begin, _, _, _, _) =>
        this.enter(begin)

      case _ =>
    }
  }
}

private final object CreateVariableAccesses extends PartialFunction[Node, Transformation.OutputType] {

  import TypeInference._

  override def isDefinedAt(node : Node) : Boolean = {
    (node.isInstanceOf[IR_StringLiteral] || node.isInstanceOf[IR_VariableAccess]) && node.hasAnnotation(TYPE_ANNOT)
  }

  override def apply(node : Node) : Transformation.OutputType = {

    // do not remove annotation as the same object could be used multiple times in AST (which is a bug, yes ;))
    val typee : IR_Datatype = node.getAnnotation(TYPE_ANNOT).get.asInstanceOf[IR_Datatype]
    val varr : String =
      node match {
        case IR_StringLiteral(name)     => name
        case IR_VariableAccess(name, _) => name
      }
    IR_VariableAccess(varr, Duplicate(typee))
  }
}
