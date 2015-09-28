package exastencils.polyhedron

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.TreeSet
import scala.util.control.Breaks

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._

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
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    isl.Options.setTileScaleTileLoops(false)
    isl.Options.setTileShiftPointLoops(false)

    Knowledge.poly_scheduleAlgorithm match {
      case "isl"       => isl.Options.setScheduleAlgorithm(isl.Options.SCHEDULE_ALGORITHM_ISL)
      case "feautrier" => isl.Options.setScheduleAlgorithm(isl.Options.SCHEDULE_ALGORITHM_FEAUTRIER)
      case unknown     => Logger.debug("Unknown schedule algorithm \"" + unknown + "\"; no change (default is isl)")
    }

    Knowledge.poly_fusionStrategy match {
      case "min"   => isl.Options.setScheduleFuse(isl.Options.SCHEDULE_FUSE_MIN)
      case "max"   => isl.Options.setScheduleFuse(isl.Options.SCHEDULE_FUSE_MAX)
      case unknown => Logger.debug("Unknown fusion strategy \"" + unknown + "\"; no change...")
    }
    isl.Options.setScheduleMaximizeBandDepth(Knowledge.poly_maximizeBandDepth)
    isl.Options.setScheduleMaxConstantTerm(Knowledge.poly_maxConstantTerm)
    isl.Options.setScheduleMaxCoefficient(Knowledge.poly_maxCoefficient)

    val scops : Seq[Scop] = extractPolyModel()
    for (scop <- scops if (!scop.remove)) {
      mergeLocalScalars(scop)
      mergeScops(scop)
      simplifyModel(scop)
      computeDependences(scop)
      deadCodeElimination(scop)
      handleReduction(scop)
      simplifyModel(scop)
      if (scop.optLevel >= 2)
        optimize(scop)
    }
    recreateAndInsertAST()

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }

  private def simplifyModel(scop : Scop) : Unit = {

    scop.domain = Isl.simplify(scop.domain)
    scop.schedule = Isl.simplify(scop.schedule)

    scop.reads = Isl.simplify(scop.reads)
    scop.writes = Isl.simplify(scop.writes)

    scop.deadAfterScop = Isl.simplify(scop.deadAfterScop)

    scop.deps.flow = Isl.simplify(scop.deps.flow)
    scop.deps.antiOut = Isl.simplify(scop.deps.antiOut)
    scop.deps.input = Isl.simplify(scop.deps.input)
  }

  private def extractPolyModel() : Seq[Scop] = {
    val extr = new Extractor()
    this.register(extr)
    this.execute(new Transformation("extract model", PartialFunction.empty))
    this.unregister(extr)

    val scops : Seq[Scop] = extr.scops

    Logger.debug("    valid SCoPs: " + scops.size)
    Logger.debug("    rejected:    " + extr.trash.size)

    return scops
  }

  private def mergeLocalScalars(scop : Scop) : Unit = {
    var toFind : String = null
    var found : Boolean = false
    val search = new Transformation("search...", {
      case va : VariableAccess if (va.name == toFind) =>
        found = true
        va
      case sc : StringConstant if (sc.value == toFind) =>
        found = true
        sc
    })
    for (decl : VariableDeclarationStatement <- scop.decls) {
      Breaks.breakable {
        val name : String = decl.name
        toFind = name
        var fstStmt : Int = -1
        var lstStmt : Int = -1
        var stmts : Buffer[(String, (Seq[Statement], ArrayBuffer[String]))] =
          scop.stmts.toBuffer.sorted(new Ordering[(String, _)] {
            override def compare(a : (String, _), b : (String, _)) : Int = {
              return a._1.compareTo(b._1)
            }
          })
        var i : Int = 0
        val oldLvl = Logger.getLevel
        Logger.setLevel(Logger.WARNING)
        for ((lab, (Seq(stmt), _)) <- stmts) {
          found = false
          this.execute(search, Some(stmt))
          if (found) {
            if (fstStmt < 0)
              fstStmt = i
            lstStmt = i
          }
          i += 1
        }
        Logger.setLevel(oldLvl)
        stmts = stmts.slice(fstStmt, lstStmt + 1)
        // check if all statements have the same iteration space
        val remDoms = new ArrayBuffer[isl.Set]()
        var njuDomain : isl.UnionSet = null
        scop.domain.foreachSet({
          set : isl.Set =>
            var found : Boolean = false
            for ((lab, (stmt, _)) <- stmts)
              if (set.getTupleName() == lab)
                found = true
            if (found)
              remDoms += set
            else
              njuDomain = if (njuDomain == null) set else njuDomain.addSet(set)
        })
        var proto : isl.Set = remDoms(0).resetTupleId()
        for (i <- 1 until remDoms.length)
          if (!proto.isEqual(remDoms(i).resetTupleId())) {
            println("foo...")
            Breaks.break() // continue... different domains, cannot merge statements
          }
        val mergedStmts = new ListBuffer[Statement]()
        var mergedLoopIts : ArrayBuffer[String] = null
        for ((lab, (Seq(stmt), loopIts)) <- stmts) {
          mergedStmts += stmt
          if (mergedLoopIts == null)
            mergedLoopIts = loopIts
          else if (!mergedLoopIts.sameElements(loopIts)) {
            println("bar...")
            Breaks.break() // continue... loopIts must be identical?!
          }
        }
        scop.domain =
          if (njuDomain == null) remDoms(0)
          else njuDomain.addSet(remDoms(0)) // re-add one of the domains
        val njuLabel : String = remDoms(0).getTupleName()
        for ((lab, _) <- stmts)
          scop.stmts(lab) = (mergedStmts, mergedLoopIts)
        // adjust read and write accesses
        var nju : isl.UnionMap = null
        scop.reads.foreachMap({
          map : isl.Map =>
            if (map.getTupleName(isl.DimType.Out) != name) { // remove all accesses to the scalar
              val oldLabel : String = map.getTupleName(isl.DimType.In)
              var toAdd : isl.Map = map
              if (oldLabel != njuLabel)
                for ((lab, _) <- stmts if (oldLabel == lab))
                  toAdd = toAdd.setTupleName(isl.DimType.In, njuLabel)
              nju = if (nju == null) toAdd else nju.addMap(toAdd)
            }
        })
        scop.reads = nju
        nju = null
        scop.writes.foreachMap({
          map : isl.Map =>
            if (map.getTupleName(isl.DimType.Out) != name) { // remove all accesses to the scalar
              val oldLabel : String = map.getTupleName(isl.DimType.In)
              var toAdd : isl.Map = map
              if (oldLabel != njuLabel)
                for ((lab, _) <- stmts if (oldLabel == lab))
                  toAdd = toAdd.setTupleName(isl.DimType.In, njuLabel)
              nju = if (nju == null) toAdd else nju.addMap(toAdd)
            }
        })
        scop.writes = nju
      }
    }
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
      scop.domain = unionNull(scop.domain, toMerge.domain)
      scop.schedule = unionNull(scop.schedule, insertCst(toMerge.schedule, i))
      scop.stmts ++= toMerge.stmts
      scop.decls ++= toMerge.decls
      scop.reads = unionNull(scop.reads, toMerge.reads)
      scop.writes = unionNull(scop.writes, toMerge.writes)
      scop.deadAfterScop = unionNull(scop.deadAfterScop, toMerge.deadAfterScop)
      scop.deps.flow = unionNull(scop.deps.flow, toMerge.deps.flow)
      scop.deps.antiOut = unionNull(scop.deps.antiOut, toMerge.deps.antiOut)
      scop.deps.input = unionNull(scop.deps.input, toMerge.deps.input)
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

  private def unionNull(a : isl.UnionSet, b : isl.UnionSet) : isl.UnionSet = {
    (a, b) match {
      case (null, y) => y
      case (x, null) => x
      case (x, y)    => x.union(y)
    }
  }

  private def unionNull(a : isl.UnionMap, b : isl.UnionMap) : isl.UnionMap = {
    (a, b) match {
      case (null, y) => y
      case (x, null) => x
      case (x, y)    => x.union(y)
    }
  }

  private def computeDependences(scop : Scop) : Unit = {

    val empty = isl.UnionMap.empty(scop.writes.getSpace())
    val depArr = new Array[isl.UnionMap](1)
    val depArr2 = new Array[isl.UnionMap](1)

    val schedule = Isl.simplify(scop.schedule.intersectDomain(scop.domain))
    //    val schedule = scop.schedule

    val writes = Isl.simplify(scop.writes.intersectDomain(scop.domain))
    //    val writes = scop.writes
    val reads = if (scop.reads == null) empty else Isl.simplify(scop.reads.intersectDomain(scop.domain))
    //    val reads = scop.reads

    // anti & output
    writes.computeFlow(writes, reads, schedule,
      depArr, depArr2, null, null) // output params
    scop.deps.antiOut = depArr(0).union(depArr2(0))

    if (scop.reads != null) {
      var readArrays : isl.UnionMap = empty
      // input dependences on scalars are irrelevant
      reads.foreachMap({
        read : isl.Map =>
          if (read.dim(isl.DimType.Out) > 0)
            readArrays = readArrays.addMap(read)
      })

      // input
      readArrays.computeFlow(readArrays, empty, schedule,
        depArr, null, null, null) // output params (C-style)
      scop.deps.input = depArr(0)

      // flow
      reads.computeFlow(writes, empty, schedule,
        depArr, null, null, null) // output params (C-style)
      scop.deps.flow = depArr(0)

    } else {
      val noDeps = isl.UnionMap.empty(scop.deps.antiOut.getSpace())
      scop.deps.input = noDeps
      scop.deps.flow = noDeps
    }
  }

  private def deadCodeElimination(scop : Scop) : Unit = {

    var live = scop.writes.intersectDomain(scop.domain).reverse().lexmax().range()
    if (scop.deadAfterScop != null)
      live = live.subtract(scop.deadAfterScop)
    live = live.union(scop.deps.flow.domain().intersect(scop.domain)) // keeps even dead instances?
    // live = live.apply(scop.deps.flow.reverse().transitiveClosure(null)) // better? test!

    if (!scop.domain.isEqual(live)) // the new one could be more complex, so keep old ;)
      scop.domain = live

    // update schedule and dependencies
    scop.schedule = scop.schedule.intersectDomain(live)
    if (scop.deps.flow != null)
      scop.deps.flow = scop.deps.flow.intersectDomain(live)
    if (scop.deps.antiOut != null)
      scop.deps.antiOut = scop.deps.antiOut.intersectDomain(live)
    if (scop.deps.input != null)
      scop.deps.input = scop.deps.input.intersectDomain(live)
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
    scop.deps.antiOut = scop.deps.antiOut.subtract(toRemove)
    scop.deps.input = scop.deps.input.subtract(toRemove)
  }

  private final val tileSizes = Array(Knowledge.poly_tileSize_x, Knowledge.poly_tileSize_y, Knowledge.poly_tileSize_z, Knowledge.poly_tileSize_w)

  private def getTileVec(dims : Int) : isl.Vec = {
    var vec = isl.Vec.alloc(dims)
    val iind : Int = dims - 1
    for (i <- 0 until dims) {
      var tileSize = if (i != 0 || Knowledge.poly_tileOuterLoop) tileSizes(iind - i) else 1000000000
      if (tileSize <= 0)
        tileSize = 1000000000
      vec = vec.setElementVal(i, tileSize)
    }
    return vec
  }

  private def optimize(scop : Scop) : Unit = {

    var schedConstr : isl.ScheduleConstraints = isl.ScheduleConstraints.onDomain(scop.domain)

    var validity = scop.deps.validity()
    var coincidence = validity
    var proximity = Knowledge.poly_optimizeDeps match {
      case "all" => validity
      case "raw" => scop.deps.flow
      case "rar" => scop.deps.input
      case _ =>
        Logger.debug("Don't know how to optimize for " + Knowledge.poly_optimizeDeps + "; falling back to \"all\"")
        validity
    }

    if (Knowledge.poly_simplifyDeps) {
      validity = validity.gistRange(scop.domain)
      validity = validity.gistDomain(scop.domain)
      coincidence = coincidence.gistRange(scop.domain)
      coincidence = coincidence.gistDomain(scop.domain)
      proximity = proximity.gistRange(scop.domain)
      proximity = proximity.gistDomain(scop.domain)
    }

    if (Knowledge.poly_filterDeps)
      proximity = Isl.simplify(proximity.lexmin())

    schedConstr = schedConstr.setValidity(validity)
    schedConstr = schedConstr.setCoincidence(coincidence)
    schedConstr = schedConstr.setProximity(proximity)

    val schedule : isl.Schedule = schedConstr.computeSchedule()
    scop.noParDims.clear()
    var perfTiling : Boolean = false
    if (scop.origIterationCount != null)
      for (i <- 0 until scop.origIterationCount.length)
        perfTiling |= tileSizes(i) < scop.origIterationCount(i)
    else
      perfTiling = true // no info about iteration size...
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
            if (tiled <= 4)
              band.tile(getTileVec(tiled))
          }
      } : isl.Band => Unit)
      val threads = Knowledge.omp_numThreads
      for (i <- 0 until tiled) {
        val tiles : Long =
          if (scop.origIterationCount != null)
            scop.origIterationCount(i) / tileSizes(i)
          else {
            Logger.warn("[PolyOpt]  unable to determine iteration count, check results of LoopOverDimensions.maxIterationCount(); parallelization might be inefficient")
            if (tileSizes(i) >= 1000000)
              1
            else // don't know how much iterations loop have... so assume there are enough to parallelize it...
              1000
          }
        if (tiles != threads && tiles < 2 * threads)
          scop.noParDims += tiled - i - 1
      }
      if (tiled > 0 && !Knowledge.poly_tileOuterLoop)
        scop.noParDims += 0
    }

    scop.schedule = Isl.simplify(schedule.getMap())
    scop.updateLoopVars()
  }

  private def recreateAndInsertAST() : Unit = {

    val replaceCallback = { (repl : Map[String, Expression], applyAt : Node) =>
      val oldLvl = Logger.getLevel
      Logger.setLevel(Logger.WARNING)
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
