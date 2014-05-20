package exastencils.polyhedron

import exastencils.core.Logger
import exastencils.core.StateManager
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.convFromNode
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.StringConstant
import exastencils.datastructures.ir.VariableAccess

object PolyOpt extends DefaultStrategy("Polyhedral optimizations") {

  override def apply(node : Option[Node] = None) : Unit = {

    token = Some(StateManager.transaction(this))

    StateManager.register(Extractor)
    this.execute(new Transformation("extract model", PartialFunction.empty))
    StateManager.unregister(Extractor)

    Logger.debug("    SCoPs: " + Extractor.scops.size)
    Logger.debug("    trash: " + Extractor.trash.size)

    val replaceCallback = { (oldVar : String, newExpr : Expression, applyAt : Node) =>
      this.execute(
        new Transformation("update loop iterator", {
          case VariableAccess(str, _) if (str == oldVar) => newExpr
          case StringConstant(str) if (str == oldVar)    => newExpr
        }), Some(applyAt))
    }
    val builder : ASTBuilder = new ASTBuilder(replaceCallback)
    for (scop <- Extractor.scops) {
      val node = builder.generateAST(scop)
      this.execute(new Transformation("insert optimized loop AST", {
        case loop if (loop == scop.root) => node
      }))
    }

    StateManager.commit(token.get)
  }
}
