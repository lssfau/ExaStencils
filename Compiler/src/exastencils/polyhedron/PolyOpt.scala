package exastencils.polyhedron

import java.io.File
import java.text.DecimalFormat

import scala.collection.mutable.Map
import scala.collection.mutable.TreeSet
import scala.io.Source

import org.exastencils.schedopt.exploration.Exploration

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.polyhedron.Isl.TypeAliases._

import isl.Conversions._

trait PolyhedronAccessable {
  var optLevel : Int = 3 // optimization level  0 [without/fastest] ... 3 [aggressive/slowest]
}

object PolyOpt extends CustomStrategy("Polyhedral optimizations") {

  final val POLY_EXPL_CHKP : String = "PolyExplChkp"

  final val SCOP_ANNOT : String = "PolyScop"
  final val IMPL_CONDITION_ANNOT : String = "ImplCondition"

  import scala.language.implicitConversions
  implicit def convertIntToVal(i : Int) : isl.Val = isl.Val.intFromSi(Isl.ctx, i)

  /** Register the name of a side-effect free function, that is safe to be used inside a scop. */
  def registerSideeffectFree(functionName : String) : Unit = {
    Extractor.registerSideeffectFree(functionName)
  }

  /** Register the name of a symbolic constant, that is not modified inside a scop. */
  def registerSymbolicConstant(constName : String) : Unit = {
    Extractor.registerSymbolicConstant(constName)
  }

  override def apply() : Unit = {
    if (Knowledge.poly_scheduleAlgorithm == "exploration")
      Logger.error("[PolyOpt] Exploration: Configuration ID required!")
    this.apply(0)
  }

  def apply(confID : Int) : Unit = {
    this.transaction()

    Logger.info("Applying strategy " + name)
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    Settings.additionalMacros += "#define floord(n,d) (((n)>=0) ? (n)/(d) : (((n)-(d)+1)/(d)))"

    Isl.ctx.setTileScaleTileLoops(0)
    Isl.ctx.setTileShiftPointLoops(0)
    //    isl.Options.setTileScaleTileLoops(false)
    //    isl.Options.setTileShiftPointLoops(false)

    //    Knowledge.poly_scheduleAlgorithm match {
    //      case "isl"         => isl.Options.setScheduleAlgorithm(isl.Options.SCHEDULE_ALGORITHM_ISL)
    //      case "feautrier"   => isl.Options.setScheduleAlgorithm(isl.Options.SCHEDULE_ALGORITHM_FEAUTRIER)
    //      case "exploration" => TODO
    //      case unknown       => Logger.debug("Unknown schedule algorithm \"" + unknown + "\"; no change (default is isl)")
    //    }
    //
    //    Knowledge.poly_fusionStrategy match {
    //      case "min"   => isl.Options.setScheduleFuse(isl.Options.SCHEDULE_FUSE_MIN)
    //      case "max"   => isl.Options.setScheduleFuse(isl.Options.SCHEDULE_FUSE_MAX)
    //      case unknown => Logger.debug("Unknown fusion strategy \"" + unknown + "\"; no change...")
    //    }
    //    isl.Options.setScheduleMaximizeBandDepth(Knowledge.poly_maximizeBandDepth)
    //    isl.Options.setScheduleMaxConstantTerm(Knowledge.poly_maxConstantTerm)
    //    isl.Options.setScheduleMaxCoefficient(Knowledge.poly_maxCoefficient)

    var scops : Seq[Scop] = extractPolyModel()
    for (scop <- scops if (!scop.remove)) {
      mergeScops(scop)
      simplifyModel(scop)
      computeDependences(scop)
      deadCodeElimination(scop)
      handleReduction(scop)
      simplifyModel(scop)
      if (scop.optLevel >= 2)
        optimize(scop, confID)
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
        var nju = map.insertDims(T_OUT, 0, 1)
        s = s.addMap(nju.fixVal(T_OUT, 0, i))
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
          if (read.dim(T_OUT) > 0)
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
      if (map.getTupleName(T_OUT) == name)
        stmts += map.getTupleName(T_IN)
    } : isl.Map => Unit)

    var toRemove = isl.UnionMap.empty(scop.deps.flow.getSpace())
    scop.deps.flow.foreachMap({ dep : isl.Map =>
      if (stmts.contains(dep.getTupleName(T_IN)))
        toRemove = toRemove.addMap(isl.Map.identity(dep.getSpace()).complement())
    } : isl.Map => Unit)
    scop.deps.flow = scop.deps.flow.subtract(toRemove)
    // filter others too, as we do not have an ordering between read and write in the same statement
    scop.deps.antiOut = scop.deps.antiOut.subtract(toRemove)
    scop.deps.input = scop.deps.input.subtract(toRemove)
  }

  private final val tileSizes = Array(Knowledge.poly_tileSize_x, Knowledge.poly_tileSize_y, Knowledge.poly_tileSize_z, Knowledge.poly_tileSize_w)

  private def getTileVec(dims : Int) : isl.Vec = {
    var vec = isl.Vec.alloc(Isl.ctx, dims)
    for (i <- 0 until dims) {
      var tileSize = if (i != 0 || Knowledge.poly_tileOuterLoop) tileSizes(dims - 1 - i) else 1000000000
      if (tileSize <= 0)
        tileSize = 1000000000
      vec = vec.setElementVal(i, tileSize)
    }
    return vec
  }

  private def optimize(scop : Scop, confID : Int) : Unit = {
    if (Knowledge.poly_scheduleAlgorithm == "exploration")
      optimizeExpl(scop, confID)
    else
      optimizeIsl(scop)
  }

  private def optimizeIsl(scop : Scop) : Unit = {

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
              prefix = math.max(prefix, map.dim(T_OUT))
          })
          if (prefix == 0) {
            tiled = band.nMember()
            if (tiled <= 4)
              band.tile(getTileVec(tiled))
          }
      })
      if (tiled <= 4)
        setSeqTileDims(scop, tiled)
    }

    scop.schedule = Isl.simplify(schedule.getMap())
    scop.updateLoopVars()
  }

  private def setSeqTileDims(scop : Scop, nrTiledDims : Int) : Unit = {
    val threads = Knowledge.omp_numThreads
    for (i <- 0 until nrTiledDims) {
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
        scop.noParDims += nrTiledDims - i - 1
    }
    if (nrTiledDims > 0 && !Knowledge.poly_tileOuterLoop)
      scop.noParDims += 0
  }

  private def optimizeExpl(scop : Scop, confID : Int) : Unit = {

    val df = new DecimalFormat()
    df.setMinimumIntegerDigits(5)
    df.setGroupingUsed(false)
    Settings.outputPath += df.format(confID)

    val explConfig = new File(Settings.poly_explorationConfig)
    if (!explConfig.exists()) {
      Logger.debug("[PolyOpt] Exploration: no configuration file found, so perform exploration and create it, progress:")
      performExploration(scop, explConfig, df)
      Logger.debug("[PolyOpt] Exploration: configuration finished, creating base version (without any schedule changes)")
    } else
      applyConfig(scop, explConfig, df.format(confID))
  }

  private def performExploration(scop : Scop, explConfig : File, df : DecimalFormat) : Unit = {
    var validity = scop.deps.validity()

    if (Knowledge.poly_simplifyDeps) {
      validity = validity.gistRange(scop.domain)
      validity = validity.gistDomain(scop.domain)
    }

    val eConfOut = new java.io.PrintWriter(explConfig)
    var i : Int = 0
    Exploration.guidedExploration(scop.domain, validity, {
      (sched : String, bands : Seq[Int]) =>
        i += 1
        if (i % 100 == 0) {
          Console.print('.')
          Console.flush()
        }
        if (i % 5000 == 0) {
          Console.println()
          Console.flush()
        }
        eConfOut.print(df.format(i))
        eConfOut.print('\t')
        eConfOut.print(bands.mkString(","))
        eConfOut.print('\t')
        eConfOut.print(sched)
        eConfOut.println()
    })
    if (i % 5000 != 0) {
      Console.println()
      Console.flush()
    }
    eConfOut.flush()
    eConfOut.close()
    Logger.debug(s"[PolyOpt] Exploration: found $i configurations")
  }

  private def applyConfig(scop : Scop, explConfig : File, confID : String) : Unit = {
    var lines : Iterator[String] = Source.fromFile(explConfig).getLines()
    lines = lines.dropWhile(l => !l.startsWith(confID))

    val configLine : String = lines.next()
    Logger.debug("[PolyOpt] Exploration: configuration found:")
    Logger.debug(" " + configLine)
    val Array(_, bandsStr, scheduleStr) = configLine.split("\t")

    val bands : Array[Int] = bandsStr.split(",").map(str => Integer.parseInt(str))
    var schedule : isl.UnionMap = isl.UnionMap.readFromStr(scop.domain.getCtx(), scheduleStr)

    scop.noParDims.clear()

    // apply tiling
    val tilableDims : Int = bands(0)
    if (scop.optLevel >= 3 && tilableDims > 1 && tilableDims <= 4) {

      val sample : isl.BasicMap = schedule.sample()
      val domSp : isl.Space = sample.getSpace().range()
      val ranSp : isl.Space = domSp.insertDims(T_SET, 0, tilableDims)
      val ctx : isl.Ctx = sample.getCtx()
      var mAff = isl.MultiAff.zero(isl.Space.mapFromDomainAndRange(domSp, ranSp))
      for (i <- 0 until tilableDims) {
        var tileSize = if (i != 0 || Knowledge.poly_tileOuterLoop) tileSizes(tilableDims - 1 - i) else 1000000000
        // if we don't want to tile a dimension, leave the outer tile loop constant 0
        if (tileSize > 0 && tileSize < 100 + scop.origIterationCount(tilableDims - 1 - i)) {
          var aff = isl.Aff.varOnDomain(isl.LocalSpace.fromSpace(domSp), T_SET, i)
          aff = aff.scaleDownUi(tileSize)
          aff = aff.floor()
          mAff = mAff.setAff(i, aff)
        }
      }
      for (i <- 0 until domSp.dim(T_SET)) {
        var aff = isl.Aff.varOnDomain(isl.LocalSpace.fromSpace(domSp), T_SET, i)
        mAff = mAff.setAff(tilableDims + i, aff)
      }
      val trafo = isl.BasicMap.fromMultiAff(mAff)
      schedule = schedule.applyRange(trafo)
      setSeqTileDims(scop, tilableDims)
    }

    scop.schedule = schedule
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
