package exastencils.polyhedron

import exastencils.core.Logger
import exastencils.core.StateManager
import exastencils.datastructures.CustomStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.convFromNode
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.StringConstant
import exastencils.datastructures.ir.VariableAccess

object PolyOpt extends CustomStrategy("Polyhedral optimizations") {

  final val SCOP_ANNOT : String = "PolySCoP"

  override def apply() : Unit = {

    this.transaction()
    Logger.info("Applying strategy " + name)

    StateManager.register(Extractor)
    this.execute(new Transformation("extract model", PartialFunction.empty))
    StateManager.unregister(Extractor)

    Logger.debug("    valid SCoPs: " + Extractor.scops.size)
    Logger.debug("    rejected:    " + Extractor.trash.size)

    val replaceCallback = { (oldVar : String, newExpr : Expression, applyAt : Node) =>
      this.execute(
        new Transformation("update loop iterator", {
          case VariableAccess(str, _) if (str == oldVar) => newExpr
          case StringConstant(str) if (str == oldVar)    => newExpr
        }), Some(applyAt))
    }
    this.execute(new ASTBuilderTransformation(replaceCallback))

    this.commit()
  }
}
