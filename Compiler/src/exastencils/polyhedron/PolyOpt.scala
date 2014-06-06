package exastencils.polyhedron

import scala.collection.mutable.ArrayStack

import exastencils.core.Logger
import exastencils.core.StateManager
import exastencils.datastructures.CustomStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.convFromNode
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.StringConstant
import exastencils.datastructures.ir.VariableAccess

trait PolyhedronAccessable

object PolyOpt extends CustomStrategy("Polyhedral optimizations") {

  final val SCOP_ANNOT : String = "PolySCoP"

  override def apply() : Unit = {

    this.transaction()
    Logger.info("Applying strategy " + name)

    val scops : ArrayStack[Scop] = extractPolyModel()
    for (scop <- scops)
      computeDependences(scop)
    recreateAndInsertAST()

    this.commit()
  }

  private def extractPolyModel() : ArrayStack[Scop] = {

    val extr = new Extractor()
    StateManager.register(extr)
    this.execute(new Transformation("extract model", PartialFunction.empty))
    StateManager.unregister(extr)

    val scops : ArrayStack[Scop] = extr.scops

    Logger.debug("    valid SCoPs: " + scops.size)
    Logger.debug("    rejected:    " + extr.trash.size)

    return scops
  }

  private def computeDependences(scop : Scop) : Unit = {

    val empty = isl.UnionMap.empty(scop.writes.getSpace())
    val depArr = new Array[isl.UnionMap](1)

    if (scop.writes != null) {

      // output
      scop.writes.computeFlow(scop.writes, empty, scop.schedule,
        depArr, null, null, null)
      scop.deps = depArr(0)

      if (scop.reads != null) {

        // flow
        scop.reads.computeFlow(scop.writes, empty, scop.schedule,
          depArr, null, null, null)
        scop.deps = scop.deps.union(depArr(0))

        // anti
        scop.writes.computeFlow(scop.reads, empty, scop.schedule,
          depArr, null, null, null)
        scop.deps = scop.deps.union(depArr(0))
      }
    }
  }

  private def recreateAndInsertAST() : Unit = {

    val replaceCallback = { (oldVar : String, newExpr : Expression, applyAt : Node) =>
      val oldLvl = Logger.getLevel
      Logger.setLevel(1)
      this.execute(
        new Transformation("update loop iterator", {
          case VariableAccess(str, _) if (str == oldVar) => newExpr
          case StringConstant(str) if (str == oldVar)    => newExpr
        }), Some(applyAt))
      Logger.setLevel(oldLvl)
    }
    this.execute(new ASTBuilderTransformation(replaceCallback))
  }
}
