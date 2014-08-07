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
import exastencils.knowledge.Knowledge
import isl.Conversions.convertIntToVal
import isl.Conversions.convertLambdaToVoidCallback1

trait PolyhedronAccessable

object PolyOpt extends CustomStrategy("Polyhedral optimizations") {

  final val SCOP_ANNOT : String = "PolyScop"

  override def apply() : Unit = {

    this.transaction()
    Logger.info("Applying strategy " + name)

    val scops : ArrayStack[Scop] = extractPolyModel()
    for (scop <- scops) {
      simplifyModel(scop)
      computeDependences(scop)
      deadCodeElimination(scop)
      optimize(scop)
      simplifyModel(scop)
    }
    recreateAndInsertAST()

    this.commit()
  }

  /** Register the name of a side-effect free function, that is safe to be used inside a scop. */
  def registerSideeffectFree(functionName : String) : Unit = {
    Extractor.registerSideeffectFree(functionName)
  }

  /** Register the name of a symbolic constant, that is not modified inside a scop. */
  def registerSymbolicConstant(constName : String) : Unit = {
    Extractor.registerSymbolicConstant(constName)
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

  private def simplifyModel(scop : Scop) : Unit = {

    if (scop.domain != null)
      scop.domain = Isl.simplify(scop.domain)
    if (scop.schedule != null)
      scop.schedule = Isl.simplify(scop.schedule)

    if (scop.reads != null)
      scop.reads = Isl.simplify(scop.reads)
    if (scop.writes != null)
      scop.writes = Isl.simplify(scop.writes)

    if (scop.deadAfterScop != null)
      scop.deadAfterScop = Isl.simplify(scop.deadAfterScop)

    if (scop.deps.flow != null)
      scop.deps.flow = Isl.simplify(scop.deps.flow)
    if (scop.deps.anti != null)
      scop.deps.anti = Isl.simplify(scop.deps.anti)
    if (scop.deps.input != null)
      scop.deps.input = Isl.simplify(scop.deps.input)
    if (scop.deps.output != null)
      scop.deps.output = Isl.simplify(scop.deps.output)
  }

  private def computeDependences(scop : Scop) : Unit = {

    val empty = isl.UnionMap.empty(scop.writes.getSpace())
    val depArr = new Array[isl.UnionMap](1)

    // output
    scop.writes.computeFlow(scop.writes, empty, scop.schedule,
      depArr, null, null, null) // output params
    scop.deps.output = depArr(0)

    if (scop.reads != null) {
      var readArrays : isl.UnionMap = empty
      // input dependences on scalars are irrelevant
      scop.reads.foreachMap({
        read : isl.Map =>
          if (read.dim(isl.DimType.Out) > 0)
            readArrays = readArrays.addMap(read)
      })

      // input
      readArrays.computeFlow(readArrays, empty, scop.schedule,
        depArr, null, null, null) // output params (C-style)
      scop.deps.input = depArr(0)

      // flow
      scop.reads.computeFlow(scop.writes, empty, scop.schedule,
        depArr, null, null, null) // output params (C-style)
      scop.deps.flow = depArr(0)

      // anti
      scop.writes.computeFlow(scop.reads, empty, scop.schedule,
        depArr, null, null, null) // output params (C-style)
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

    if (!scop.domain.isEqual(live)) // the new one could be more complex, so keep old ;)
      scop.domain = live

    // update schedule, accesses and dependencies
    //    scop.schedule = scop.schedule.intersectDomain(live)
    //    if (scop.reads != null)
    //      scop.reads = scop.reads.intersectDomain(live)
    //    if (scop.writes != null)
    //      scop.writes = scop.writes.intersectDomain(live)
    //    if (scop.deps.flow != null)
    //      scop.deps.flow = scop.deps.flow.intersectDomain(live)
    //    if (scop.deps.anti != null)
    //      scop.deps.anti = scop.deps.anti.intersectDomain(live)
    //    if (scop.deps.output != null)
    //      scop.deps.output = scop.deps.output.intersectDomain(live)
    //    if (scop.deps.input != null)
    //      scop.deps.input = scop.deps.input.intersectDomain(live)
  }

  private def optimize(scop : Scop) : Unit = {

    var schedConstr : isl.ScheduleConstraints = isl.ScheduleConstraints.onDomain(scop.domain)

    schedConstr = schedConstr.setValidity(scop.deps.validity())
    //    schedConstr = schedConstr.setCoincidence(coincidence)
    schedConstr = schedConstr.setProximity(scop.deps.input)

    val schedule : isl.Schedule = schedConstr.computeSchedule()
    scop.noParDims.clear()
    if (scop.parallelize) {
      var v : isl.Vec = isl.Vec.alloc(3)
      v = v.setElementVal(0, Knowledge.poly_tileSize_z)
      v = v.setElementVal(1, Knowledge.poly_tileSize_y)
      v = v.setElementVal(2, Knowledge.poly_tileSize_x)
      var tiled : Boolean = false
      schedule.foreachBand({ // TODO
        band : isl.Band =>
          if (band.nMember() == 3) {
            band.tile(v)
            tiled = true
          }
      })
      if (tiled) { // filter dimensions with too few iterations
        val fragSize : Int = 1 << Knowledge.maxLevel // roughly
        var loopIterations_x : Int = fragSize
        var loopIterations_y : Int = fragSize
        var loopIterations_z : Int = fragSize
        if (Knowledge.domain_summarizeBlocks) {
          loopIterations_x *= Knowledge.domain_numFragsPerBlock_x
          loopIterations_y *= Knowledge.domain_numFragsPerBlock_y
          loopIterations_z *= Knowledge.domain_numFragsPerBlock_z
        }
        if (loopIterations_z / Knowledge.poly_tileSize_z < Knowledge.omp_numThreads)
          scop.noParDims += 0
        if (loopIterations_y / Knowledge.poly_tileSize_y < Knowledge.omp_numThreads)
          scop.noParDims += 1
        if (loopIterations_x / Knowledge.poly_tileSize_x < Knowledge.omp_numThreads)
          scop.noParDims += 2
      }
    }

    scop.schedule = schedule.getMap()
    scop.updateLoopVars()
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
