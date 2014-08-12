package exastencils.polyhedron

import scala.collection.mutable.ArrayStack
import scala.collection.mutable.HashMap
import scala.collection.mutable.TreeSet

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

  /** Register the name of a side-effect free function, that is safe to be used inside a scop. */
  def registerSideeffectFree(functionName : String) : Unit = {
    Extractor.registerSideeffectFree(functionName)
  }

  /** Register the name of a symbolic constant, that is not modified inside a scop. */
  def registerSymbolicConstant(constName : String) : Unit = {
    Extractor.registerSymbolicConstant(constName)
  }

  override def apply() : Unit = {

    this.transaction()
    Logger.info("Applying strategy " + name)

    val scops : ArrayStack[Scop] = extractPolyModel()
    for (scop <- scops) {
      simplifyModel(scop)
      computeDependences(scop)
      deadCodeElimination(scop)
      handleReduction(scop)
      optimize(scop)
      simplifyModel(scop)
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

  private def handleReduction(scop : Scop) : Unit = {

    if (scop.reduction.isEmpty)
      return

    val name : String = Extractor.replaceSpecial(scop.reduction.get.target.cpp())
    val stmts = new TreeSet[String]()
    scop.writes.foreachMap({ map : isl.Map =>
      if (map.getTupleName(isl.DimType.Out) == name)
        stmts += map.getTupleName(isl.DimType.In)
    } : isl.Map => Unit)

    var toRemove = isl.UnionMap.empty(scop.deps.flow.getSpace())
    scop.deps.flow.foreachMap({ dep : isl.Map =>
      if (stmts.contains(dep.getTupleName(isl.DimType.In)))
        toRemove = toRemove.addMap(isl.Map.identity(dep.getSpace()).complement())
    } : isl.Map => Unit)
    scop.deps.flow = scop.deps.flow.subtract(toRemove)
    // filter others too, as we do not have an ordering between read and write in the same statement
    scop.deps.anti = scop.deps.anti.subtract(toRemove)
    scop.deps.input = scop.deps.input.subtract(toRemove)
    scop.deps.output = scop.deps.output.subtract(toRemove)
  }

  private def optimize(scop : Scop) : Unit = {

    var schedConstr : isl.ScheduleConstraints = isl.ScheduleConstraints.onDomain(scop.domain)

    schedConstr = schedConstr.setValidity(scop.deps.validity())
    //    schedConstr = schedConstr.setCoincidence(coincidence)
    schedConstr = schedConstr.setProximity(scop.deps.input)

    val schedule : isl.Schedule = schedConstr.computeSchedule()
    scop.noParDims.clear()
    if (scop.parallelize) {
      var v2 : isl.Vec = isl.Vec.alloc(2)
      v2 = v2.setElementVal(0, Knowledge.poly_tileSize_y)
      v2 = v2.setElementVal(1, Knowledge.poly_tileSize_x)
      var v3 : isl.Vec = isl.Vec.alloc(3)
      v3 = v3.setElementVal(0, Knowledge.poly_tileSize_z)
      v3 = v3.setElementVal(1, Knowledge.poly_tileSize_y)
      v3 = v3.setElementVal(2, Knowledge.poly_tileSize_x)
      var v4 : isl.Vec = isl.Vec.alloc(4)
      v4 = v4.setElementVal(0, Knowledge.poly_tileSize_w)
      v4 = v4.setElementVal(1, Knowledge.poly_tileSize_z)
      v4 = v4.setElementVal(2, Knowledge.poly_tileSize_y)
      v4 = v4.setElementVal(3, Knowledge.poly_tileSize_x)
      var tiled : Int = 0
      schedule.foreachBand({
        band : isl.Band =>
          var prefix : Int = 0
          band.getPrefixSchedule().foreachMap({ map : isl.Map =>
            if (!map.range().isSingleton())
              prefix = math.max(prefix, map.dim(isl.DimType.Out))
          })
          if (prefix == 0) {
            tiled = band.nMember()
            tiled match {
              case 2 => band.tile(v2)
              case 3 => band.tile(v3)
              case 4 => band.tile(v4)
              case _ =>
            }
          }
      } : isl.Band => Unit)
      val threads = Knowledge.omp_numThreads
      for (i <- 0 until tiled) {
        val tileSize = i match {
          case 0 => Knowledge.poly_tileSize_x
          case 1 => Knowledge.poly_tileSize_y
          case 2 => Knowledge.poly_tileSize_z
          case 3 => Knowledge.poly_tileSize_w
        }
        val tiles : Long = scop.origIterationCount(i) / tileSize
        if (tiles != threads && tiles < 2 * threads)
          scop.noParDims += tiled - i - 1
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
