package exastencils.polyhedron

import scala.collection.mutable.Map
import scala.collection.mutable.TreeSet

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge.Knowledge
import isl.Conversions._

trait PolyhedronAccessable {
  var optLevel : Int = 3 // optimization level  0 [without/fastest] ... 3 [aggressive/slowest]
}

object PolyOpt extends CustomStrategy("Polyhedral optimizations") {

  final val SCOP_ANNOT : String = "PolyScop"
  final val IMPL_CONDITION_ANNOT : String = "ImplCondition"

  // FIXME: HACK: only use shipped versions of jna and isl, NO system libraries (prevent version conflicts)
  System.setProperty("jna.nosys", "true")
  System.setProperty("jna.platform.library.path", "NO_SYSTEM")

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

    isl.Options.setTileScaleTileLoops(false)
    isl.Options.setTileShiftPointLoops(false)

    val scops : Seq[Scop] = extractPolyModel()
    for (scop <- scops if (!scop.remove)) {
      mergeScops(scop)
      simplifyModel(scop)
      if (scop.optLevel >= 2) {
        computeDependences(scop)
        deadCodeElimination(scop)
        handleReduction(scop)
        optimize(scop)
        simplifyModel(scop)
      }
    }
    recreateAndInsertAST()

    this.commit()
  }

  private def simplifyModel(scop : Scop) : Unit = {

    scop.domain = Isl.simplify(scop.domain)
    scop.schedule = Isl.simplify(scop.schedule)

    scop.reads = Isl.simplify(scop.reads)
    scop.writes = Isl.simplify(scop.writes)

    scop.deadAfterScop = Isl.simplify(scop.deadAfterScop)

    scop.deps.flow = Isl.simplify(scop.deps.flow)
    scop.deps.anti = Isl.simplify(scop.deps.anti)
    scop.deps.input = Isl.simplify(scop.deps.input)
    scop.deps.output = Isl.simplify(scop.deps.output)
  }

  private def extractPolyModel() : Seq[Scop] = {

    val extr = new Extractor()
    StateManager.register(extr)
    this.execute(new Transformation("extract model", PartialFunction.empty))
    StateManager.unregister(extr)

    val scops : Seq[Scop] = extr.scops

    Logger.debug("    valid SCoPs: " + scops.size)
    Logger.debug("    rejected:    " + extr.trash.size)

    return scops
  }

  private def mergeScops(scop : Scop) : Unit = {

    var toMerge : Scop = scop.nextMerge
    var i : Int = 0
    scop.schedule = insertCst(scop.schedule, i)
    while (toMerge != null) {
      i += 1
      if (scop.root.reduction != toMerge.root.reduction) {
        Logger.warn("[PolyOpt]  cannot merge two loops with different reduction clauses (maybe a bug in previous generation?)")
        scop.nextMerge = null
        return
      }
      scop.optLevel = math.max(scop.optLevel, toMerge.optLevel)
      scop.domain = union(scop.domain, toMerge.domain)
      scop.schedule = union(scop.schedule, insertCst(toMerge.schedule, i))
      scop.stmts ++= toMerge.stmts
      scop.decls ++= toMerge.decls
      scop.reads = union(scop.reads, toMerge.reads)
      scop.writes = union(scop.writes, toMerge.writes)
      scop.deadAfterScop = union(scop.deadAfterScop, toMerge.deadAfterScop)
      scop.deps.flow = union(scop.deps.flow, toMerge.deps.flow)
      scop.deps.anti = union(scop.deps.anti, toMerge.deps.anti)
      scop.deps.input = union(scop.deps.input, toMerge.deps.input)
      scop.deps.output = union(scop.deps.output, toMerge.deps.output)
      if (scop.origIterationCount == null && toMerge.origIterationCount != null)
        scop.origIterationCount = toMerge.origIterationCount
      scop.parallelize &= toMerge.parallelize
      toMerge.remove = true
      toMerge = toMerge.nextMerge
    }
    scop.updateLoopVars()
  }

  private def insertCst(sched : isl.UnionMap, i : Int) : isl.UnionMap = {
    var s = isl.UnionMap.empty(sched.getSpace())
    sched.foreachMap({
      map : isl.Map =>
        var nju = map.insertDims(isl.DimType.Out, 0, 1)
        s = s.addMap(nju.fixVal(isl.DimType.Out, 0, i))
    })
    return s
  }

  private def union(a : isl.UnionSet, b : isl.UnionSet) : isl.UnionSet = {
    (a, b) match {
      case (null, y) => y
      case (x, null) => x
      case (x, y)    => x.union(y)
    }
  }

  private def union(a : isl.UnionMap, b : isl.UnionMap) : isl.UnionMap = {
    (a, b) match {
      case (null, y) => y
      case (x, null) => x
      case (x, y)    => x.union(y)
    }
  }

  private def computeDependences(scop : Scop) : Unit = {

    val empty = isl.UnionMap.empty(scop.writes.getSpace())
    val depArr = new Array[isl.UnionMap](1)

    val schedule = Isl.simplify(scop.schedule.intersectDomain(scop.domain))
    //    val schedule = scop.schedule

    // output
    scop.writes.computeFlow(scop.writes, empty, schedule,
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
      readArrays.computeFlow(readArrays, empty, schedule,
        depArr, null, null, null) // output params (C-style)
      scop.deps.input = depArr(0)

      // flow
      scop.reads.computeFlow(scop.writes, empty, schedule,
        depArr, null, null, null) // output params (C-style)
      scop.deps.flow = depArr(0)

      // anti
      scop.writes.computeFlow(scop.reads, empty, schedule,
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

    if (scop.root.reduction.isEmpty)
      return

    val name : String = Extractor.replaceSpecial(scop.root.reduction.get.target.prettyprint())
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

  private final val tileSizes = Array(Knowledge.poly_tileSize_x, Knowledge.poly_tileSize_y, Knowledge.poly_tileSize_z, Knowledge.poly_tileSize_w)

  private def getTileVec(dims : Int, itCount : Array[Long]) : isl.Vec = {
    var vec = isl.Vec.alloc(dims)
    val iind : Int = dims - 1
    for (i <- 0 until dims) {
      var tileSize = if (i != 0 || Knowledge.poly_tileOuterLoop) tileSizes(iind - i) else 1000000000
      //      tileSize = math.min(tileSize, itCount(i).toInt + 20) // TODO: "+ 20" (heuristics)
      vec = vec.setElementVal(i, tileSize)
    }
    return vec
  }

  private def optimize(scop : Scop) : Unit = {

    //    if (scop.nextMerge != null) {
    //      println("val dom = new isl.UnionSet(\"" + scop.domain + "\")")
    //      println("val depsVal = new isl.UnionMap(\"" + scop.deps.validity() + "\")")
    //      println("val depsInp = new isl.UnionMap(\"" + scop.deps.input + "\")")
    //      println()
    //    }

    var schedConstr : isl.ScheduleConstraints = isl.ScheduleConstraints.onDomain(scop.domain)

    schedConstr = schedConstr.setValidity(scop.deps.validity())
    //    schedConstr = schedConstr.setCoincidence(coincidence)
    schedConstr = schedConstr.setProximity(scop.deps.input)

    val schedule : isl.Schedule = schedConstr.computeSchedule()
    scop.noParDims.clear()
    var perfTiling : Boolean = false
    for (i <- 0 until scop.origIterationCount.length)
      perfTiling |= tileSizes(i) < scop.origIterationCount(i)
    if (scop.optLevel >= 3 && perfTiling) {
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
            if (2 <= tiled && tiled <= 4)
              band.tile(getTileVec(tiled, scop.origIterationCount))
          }
      } : isl.Band => Unit)
      val threads = Knowledge.omp_numThreads
      for (i <- 0 until tiled) {
        val tiles : Long = scop.origIterationCount(i) / tileSizes(i)
        if (tiles != threads && tiles < 2 * threads)
          scop.noParDims += tiled - i - 1
      }
      if (tiled > 0 && !Knowledge.poly_tileOuterLoop)
        scop.noParDims += 0
    }

    scop.schedule = schedule.getMap()
    scop.updateLoopVars()
  }

  private def recreateAndInsertAST() : Unit = {

    val replaceCallback = { (repl : Map[String, Expression], applyAt : Node) =>
      val oldLvl = Logger.getLevel
      Logger.setLevel(1)
      this.execute(
        new Transformation("update loop iterator", {
          case VariableAccess(str, _) if (repl.isDefinedAt(str)) => Duplicate(repl(str))
          case StringConstant(str) if (repl.isDefinedAt(str))    => Duplicate(repl(str))
        }), Some(applyAt))
      Logger.setLevel(oldLvl)
    }
    this.execute(new ASTBuilderTransformation(replaceCallback))
  }
}
