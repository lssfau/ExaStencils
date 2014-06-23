package exastencils.polyhedron

import scala.collection.mutable.ArrayStack
import scala.collection.mutable.HashMap

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

  final val SCOP_ANNOT : String = "PolyScop"

  override def apply() : Unit = {

    this.transaction()
    Logger.info("Applying strategy " + name)

    val scops : ArrayStack[Scop] = extractPolyModel()
    for (scop <- scops) {
      computeDependences(scop)
      deadCodeElimination(scop)
      //      optimize(scop)
    }
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

    // output
    scop.writes.computeFlow(scop.writes, empty, scop.schedule,
      depArr, null, null, null) // output params
    scop.deps.output = depArr(0)

    if (scop.reads != null) {

      // input
      scop.reads.computeFlow(scop.reads, empty, scop.schedule,
        depArr, null, null, null) // output params
      scop.deps.input = depArr(0)

      // flow
      scop.reads.computeFlow(scop.writes, empty, scop.schedule,
        depArr, null, null, null) // output params
      scop.deps.flow = depArr(0)

      // anti
      scop.writes.computeFlow(scop.reads, empty, scop.schedule,
        depArr, null, null, null) // output params
      scop.deps.anti = depArr(0)

    } else {
      val noDeps = isl.UnionMap.empty(scop.deps.output.getSpace())
      scop.deps.input = noDeps
      scop.deps.flow = noDeps
      scop.deps.anti = noDeps
    }
  }

  private def deadCodeElimination(scop : Scop) : Unit = {

    var live = scop.writes.intersectDomain(scop.domain).reverse().lexmax().range()
    if (scop.deadAfterScop != null)
      live = live.subtract(scop.deadAfterScop)
    live = live.union(scop.deps.flow.domain().intersect(scop.domain))

    scop.domain = live
  }

  private def optimize(scop : Scop) : Unit = {

    val validity : isl.UnionMap = scop.deps.validity()
    val proximity : isl.UnionMap = scop.deps.input

    val njuSched = scop.domain.computeSchedule(validity, proximity)
    println("===================================================================")
    println(scop.schedule)
    println(njuSched)
    println(scop.domain)
    println(scop.reads)
    println(scop.writes)
  }

  private def recreateAndInsertAST() : Unit = {

    val replaceCallback = { (repl : HashMap[String, Expression], applyAt : Node) =>
      val oldLvl = Logger.getLevel
      Logger.setLevel(1)
      this.execute(
        new Transformation("update loop iterator", {
          case old @ VariableAccess(str, _) => repl.getOrElse(str, old)
          case old @ StringConstant(str)    => repl.getOrElse(str, old)
        }), Some(applyAt))
      Logger.setLevel(oldLvl)
    }
    this.execute(new ASTBuilderTransformation(replaceCallback))
  }
}
