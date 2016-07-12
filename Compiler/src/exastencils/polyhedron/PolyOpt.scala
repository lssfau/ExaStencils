package exastencils.polyhedron

import java.io.File
import java.text._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.io.Source
import scala.util.control.Breaks
import org.exastencils.schedopt.exploration.DomainCoeffInfo
import org.exastencils.schedopt.exploration.Exploration
import org.exastencils.schedopt.exploration.PartialSchedule
import org.exastencils.schedopt.exploration.ScheduleSpace
import org.exastencils.schedopt.exploration.StmtCoeffInfo
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.polyhedron.Isl.TypeAliases._
import isl.Conversions._

trait PolyhedronAccessible {
  var optLevel : Int = 3 // optimization level  0 [without/fastest] ... 3 [aggressive/slowest]
}

object PolyOpt extends CustomStrategy("Polyhedral optimizations") {

  final val SCOP_ANNOT : String = "PolyScop"
  final val IMPL_CONDITION_ANNOT : String = "ImplCondition"

  var timeSingleSteps : Boolean = false

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

    def time[T](op : => T, name : String) : T = {
      if (timeSingleSteps) {
        StrategyTimer.startTiming(name)
        val res = op
        StrategyTimer.stopTiming(name)
        return res
      } else
        return op
    }

    val scops : Seq[Scop] = time(extractPolyModel(), "po:extractPolyModel")
    for (scop <- scops if (!scop.remove)) {
      time(mergeLocalScalars(scop), "po:mergeLocalScalars")
      time(mergeScops(scop), "po:mergeScops")
      time(simplifyModel(scop), "po:simplifyModel")
      time(computeDependences(scop), "po:computeDependences")
      if (Knowledge.poly_performDCE)
        time(deadCodeElimination(scop), "po:deadCodeElimination")
      time(handleReduction(scop), "po:handleReduction")
      time(simplifyModel(scop), "po:simplifyModel")

      if (scop.optLevel >= 2)
        time(optimize(scop, confID), "po:optimize")
    }
    time(recreateAndInsertAST(), "po:recreateAndInsertAST")

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

    scop.deps.flowPar = Isl.simplify(scop.deps.flowPar)
    scop.deps.flowParVec = Isl.simplify(scop.deps.flowParVec)
    scop.deps.antiOutParVec = Isl.simplify(scop.deps.antiOutParVec)
    scop.deps.antiOutPar = Isl.simplify(scop.deps.antiOutPar)
    scop.deps.mapInputLazy { input => Isl.simplify(input) }
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
      case sc : StringLiteral if (sc.value == toFind) =>
        found = true
        sc
    })
    for (decl : VariableDeclarationStatement <- scop.decls) Breaks.breakable {
      val name : String = decl.name
      toFind = name
      var fstStmt : Int = -1
      var lstStmt : Int = -1
      var stmts : Buffer[(String, (ListBuffer[Statement], ArrayBuffer[String]))] = scop.stmts.toBuffer.sortBy(_._1)
      var i : Int = 0
      val oldLvl = Logger.getLevel
      Logger.setLevel(Logger.WARNING)
      for ((lab, (stmt, _)) <- stmts) {
        found = false
        this.execute(search, Some(Scope(stmt)))
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
      val mergedDom : isl.Set = remDoms(0)
      val proto : isl.Set = mergedDom.resetTupleId()
      for (i <- 1 until remDoms.length)
        if (!proto.isEqual(remDoms(i).resetTupleId()))
          Breaks.break() // continue... different domains, cannot merge statements
      val mergedStmts = new ListBuffer[Statement]()
      var mergedLoopIts : ArrayBuffer[String] = null
      for ((lab, (stmt, loopIts)) <- stmts) {
        mergedStmts ++= stmt
        if (mergedLoopIts == null)
          mergedLoopIts = loopIts
        else if (!mergedLoopIts.equals(loopIts))
          Breaks.break() // continue... loopIts must be identical?!
      }
      scop.domain =
        if (njuDomain == null) mergedDom
        else njuDomain.addSet(mergedDom) // re-add one of the domains
      val njuLabel : String = mergedDom.getTupleName()
      for ((lab, _) <- stmts)
        if (lab == njuLabel)
          scop.stmts(lab) = (mergedStmts, mergedLoopIts)
        else
          scop.stmts -= lab
      // adjust read and write accesses
      def adjust(umap : isl.UnionMap) : isl.UnionMap = {
        var nju : isl.UnionMap = null
        umap.foreachMap({
          map : isl.Map =>
            if (map.getTupleName(T_OUT) != name) { // remove all accesses to the scalar
            val oldLabel : String = map.getTupleName(T_IN)
              var toAdd : isl.Map = map
              if (oldLabel != njuLabel)
                for ((lab, _) <- stmts if (oldLabel == lab))
                  toAdd = toAdd.setTupleName(T_IN, njuLabel)
              nju = if (nju == null) toAdd else nju.addMap(toAdd)
            }
        })
        return nju
      }
      scop.reads = adjust(scop.reads)
      scop.writes = adjust(scop.writes)

      // update scop.deadAfterScop
      var resurrect : Boolean = false
      for (set <- remDoms)
        if (scop.deadAfterScop.intersect(set).isEmpty())
          resurrect = true
      if (resurrect)
        scop.deadAfterScop = scop.deadAfterScop.subtract(isl.Set.universeLike(remDoms(0)))
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
      scop.localContext = scop.localContext.union(toMerge.localContext)
      scop.globalContext = scop.globalContext.intersect(toMerge.globalContext)
      scop.domain = scop.domain.union(toMerge.domain)
      scop.schedule = scop.schedule.union(insertCst(toMerge.schedule, i))
      scop.stmts ++= toMerge.stmts
      scop.decls ++= toMerge.decls
      scop.reads = unionNull(scop.reads, toMerge.reads)
      scop.writes = unionNull(scop.writes, toMerge.writes)
      scop.deadAfterScop = unionNull(scop.deadAfterScop, toMerge.deadAfterScop)
      scop.deps.flowParVec = unionNull(scop.deps.flowParVec, toMerge.deps.flowParVec)
      scop.deps.flowPar = unionNull(scop.deps.flowPar, toMerge.deps.flowPar)
      scop.deps.antiOutParVec = unionNull(scop.deps.antiOutParVec, toMerge.deps.antiOutParVec)
      scop.deps.antiOutPar = unionNull(scop.deps.antiOutPar, toMerge.deps.antiOutPar)
      scop.deps.mapInputLazy { input => unionNull(input, toMerge.deps.input) }
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
        val nju = map.insertDims(T_OUT, 0, 1)
        s = s.addMap(nju.fixVal(T_OUT, 0, i))
    })
    return s
  }

  private def unionNull(a : isl.Set, b : isl.Set) : isl.Set = {
    (a, b) match {
      case (null, y) => y
      case (x, null) => x
      case (x, y)    => x.union(y)
    }
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
    val domain : isl.UnionSet = scop.domain.intersectParams(scop.getContext())

    val schedule = Isl.simplify(scop.schedule.intersectDomain(domain))

    val writes = (scop.writes.intersectDomain(domain))
    val reads = if (scop.reads == null) empty else Isl.simplify(scop.reads.intersectDomain(domain))

    var writesToVec, writesNotVec : isl.UnionMap = empty
    var readsToVec, readsNotVec : isl.UnionMap = empty

    writes.foreachMap { map : isl.Map =>
      if (map.getTupleName(isl.DimType.Out).startsWith(iv.LoopCarriedCSBuffer.commonPrefix))
        writesToVec = writesToVec.addMap(map)
      else
        writesNotVec = writesNotVec.addMap(map)
    }
    reads.foreachMap { map : isl.Map =>
      if (map.getTupleName(isl.DimType.Out).startsWith(iv.LoopCarriedCSBuffer.commonPrefix))
        readsToVec = readsToVec.addMap(map)
      else
        readsNotVec = readsNotVec.addMap(map)
    }

    // anti & output
    writesNotVec.computeFlow(writesNotVec, readsNotVec, schedule,
      depArr, depArr2, null, null) // output params
    scop.deps.antiOutParVec = depArr(0).union(depArr2(0))
    writesToVec.computeFlow(writesToVec, readsToVec, schedule,
      depArr, depArr2, null, null) // output params
    scop.deps.antiOutPar = depArr(0).union(depArr2(0))

    if (scop.reads != null) {
      var readArrays : isl.UnionMap = empty
      // input dependences on scalars are irrelevant
      reads.foreachMap({
        read : isl.Map =>
          if (read.dim(T_OUT) > 0)
            readArrays = readArrays.addMap(read)
      })

      // input
      scop.deps.setInputLazy { () =>
        readArrays.computeFlow(readArrays, empty, schedule,
          depArr, null, null, null) // output params (C-style)
        depArr(0)
      }

      // flow
      readsNotVec.computeFlow(writesNotVec, empty, schedule,
        depArr, null, null, null) // output params (C-style)
      scop.deps.flowParVec = depArr(0)
      readsToVec.computeFlow(writesToVec, empty, schedule,
        depArr, null, null, null) // output params (C-style)
      scop.deps.flowPar = depArr(0)

    } else {
      val noDeps = isl.UnionMap.empty(scop.deps.antiOutParVec.getSpace())
      scop.deps.input = noDeps
      scop.deps.flowParVec = noDeps
      scop.deps.flowPar = noDeps
    }
  }

  private def deadCodeElimination(scop : Scop) : Unit = {

    var live = scop.writes.intersectDomain(scop.domain).reverse().lexmax().range()
    if (scop.deadAfterScop != null)
      live = live.subtract(scop.deadAfterScop)
    // live = live.union(scop.deps.flow.domain().intersect(scop.domain)) // keeps even dead instances?
    val trans = scop.deps.flow.reverse().transitiveClosure(new Array[Int](1))
    live = live.union(live.apply(trans))
    live = live.intersect(scop.domain)
    live = live.coalesce()

    if (!scop.domain.isEqual(live)) // the new one could be more complex, so keep old if possible
      scop.domain = live
  }

  private def handleReduction(scop : Scop) : Unit = {
    if (scop.root.reduction.isEmpty)
      return

    val name : String = Extractor.replaceSpecial(scop.root.reduction.get.target.prettyprint())
    val stmts = Set[String]()
    scop.writes.foreachMap({ map : isl.Map =>
      if (map.getTupleName(T_OUT) == name)
        stmts += map.getTupleName(T_IN)
    } : isl.Map => Unit)

    var toRemove = isl.UnionMap.empty(scop.deps.flowParVec.getSpace())
    scop.deps.flowParVec.foreachMap({ dep : isl.Map =>
      if (stmts.contains(dep.getTupleName(T_IN)))
        toRemove = toRemove.addMap(isl.Map.identity(dep.getSpace()).complement())
    } : isl.Map => Unit)
    scop.deps.flowPar.foreachMap({ dep : isl.Map =>
      if (stmts.contains(dep.getTupleName(T_IN)))
        toRemove = toRemove.addMap(isl.Map.identity(dep.getSpace()).complement())
    } : isl.Map => Unit)
    scop.deps.flowParVec = scop.deps.flowParVec.subtract(toRemove)
    scop.deps.flowPar = scop.deps.flowPar.subtract(toRemove)
    // filter others too, as we do not have an ordering between read and write in the same statement
    scop.deps.antiOutParVec = scop.deps.antiOutParVec.subtract(toRemove)
    scop.deps.antiOutPar = scop.deps.antiOutPar.subtract(toRemove)
    scop.deps.mapInputLazy { input => input.subtract(toRemove) }
  }

  private def optimize(scop : Scop, confID : Int) : Unit = {
    Knowledge.poly_scheduleAlgorithm match {
      case "exploration" => optimizeExpl(scop, confID)
      case "test"        => optimizeTest(scop)
      case _             => optimizeIsl(scop)
    }
  }

  private def optimizeIsl(scop : Scop) : Unit = {

    val domain = scop.domain.intersectParams(scop.getContext())
    var schedConstr : isl.ScheduleConstraints = isl.ScheduleConstraints.onDomain(domain)

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
      validity = validity.gistRange(domain)
      validity = validity.gistDomain(domain)
      coincidence = coincidence.gistRange(domain)
      coincidence = coincidence.gistDomain(domain)
      proximity = proximity.gistRange(domain)
      proximity = proximity.gistDomain(domain)
    }

    if (Knowledge.poly_filterDeps)
      proximity = Isl.simplify(proximity.lexmin())

    schedConstr = schedConstr.setValidity(validity)
    schedConstr = schedConstr.setCoincidence(coincidence)
    schedConstr = schedConstr.setProximity(proximity)

    val schedule : isl.Schedule = schedConstr.computeSchedule()
    var scheduleMap : isl.UnionMap = schedule.getMap()
    scop.noParDims.clear()
    if (scop.optLevel >= 3) {
      var tilableDims : Int = 0
      schedule.foreachBand({
        band : isl.Band =>
          var prefix : Int = 0
          band.getPrefixSchedule().foreachMap({ map : isl.Map =>
            if (!map.range().isSingleton())
              prefix = math.max(prefix, map.dim(T_OUT))
          })
          if (prefix == 0)
            tilableDims = band.nMember()
      })
      if (tilableDims > 1 && tilableDims <= 4)
        scheduleMap = tileSchedule(scheduleMap, scop, tilableDims)
    }

    scop.schedule = Isl.simplify(scheduleMap)
    scop.updateLoopVars()
  }

  private def optimizeTest(scop : Scop) : Unit = {
    val domain = scop.domain.intersectParams(scop.getContext())
    var validity = scop.deps.validity()

    if (Knowledge.poly_simplifyDeps) {
      validity = validity.gistRange(domain)
      validity = validity.gistDomain(domain)
    }

    val depList : ArrayBuffer[isl.BasicMap] = Exploration.preprocess(validity)

    val domInfo = DomainCoeffInfo(domain)
    val sched = new PartialSchedule(domInfo, depList)
    var coeffSpace : isl.Set = sched.computeLinIndepSpace()

    // all dependences must be satisfied at least weakly
    for (dep <- depList) {
      val constr : isl.BasicSet = ScheduleSpace.compSchedConstrForDep(dep, sched.domInfo, false)
      coeffSpace = coeffSpace.intersect(constr)
    }

    val zeroVal = isl.Val.zero(scop.domain.getCtx())
    for ((stmt, StmtCoeffInfo(itStart, nrIt, _, cstIdx)) <- domInfo.stmtInfo) {
      coeffSpace = coeffSpace.fixVal(T_SET, itStart + nrIt - 1, zeroVal)
      coeffSpace = coeffSpace.lowerBoundVal(T_SET, cstIdx, zeroVal)
    }
    val addDims : Int = domInfo.nrIt - domInfo.nrStmts + 2 // sum of abs coeffs and sum of paramcoeffs
    coeffSpace = coeffSpace.insertDims(T_SET, 0, addDims)

    var idx : Int = 1
    var i : Int = 0
    while (idx < addDims - 1) {
      for ((stmt, StmtCoeffInfo(itStart, nrIt, parStart, cstIdx)) <- domInfo.stmtInfo)
        if (i < nrIt - 1) {
          var aff = isl.Aff.zeroOnDomain(isl.LocalSpace.fromSpace(coeffSpace.getSpace()))
          aff = aff.setCoefficientSi(T_IN, addDims + itStart + i, -2)
          aff = aff.setCoefficientSi(T_IN, idx, -1)
          var set : isl.Set = aff.zeroBasicSet() // p = -2*c
          aff = aff.setCoefficientSi(T_IN, addDims + itStart + i, 2)
          aff = aff.setConstantSi(-1)
          set = set.union(aff.zeroBasicSet()) // p = 2*c - 1
          set = set.lowerBoundVal(T_SET, idx, zeroVal) // p >= 0
          coeffSpace = coeffSpace.intersect(set)
          idx += 1
        }
      i += 1
    }

    // set coeffSpace[idx] to sum of parameter coeffs and restrict the latter to natural numbers
    var aff = isl.Aff.zeroOnDomain(isl.LocalSpace.fromSpace(coeffSpace.getSpace()))
    aff = aff.setCoefficientSi(T_IN, idx, -1)
    for (pi <- 0 until domInfo.nrParPS * domInfo.nrStmts) {
      val pos : Int = addDims + domInfo.nrIt + pi
      coeffSpace = coeffSpace.lowerBoundVal(T_SET, pos, zeroVal)
      aff = aff.setCoefficientSi(T_IN, pos, 1)
    }
    coeffSpace = coeffSpace.intersect(aff.zeroBasicSet())
    coeffSpace = coeffSpace.lowerBoundVal(T_SET, 0, isl.Val.one(coeffSpace.getCtx())) // prevent 0 solution

    // set coeffSpace[0] to sum of abs coeffs
    aff = isl.Aff.zeroOnDomain(isl.LocalSpace.fromSpace(coeffSpace.getSpace()))
    aff = aff.setCoefficientSi(T_IN, 0, -1)
    for (i <- 1 until addDims - 1)
      aff = aff.setCoefficientSi(T_IN, i, 1)
    coeffSpace = coeffSpace.intersect(aff.zeroBasicSet())
    val schedPoint = coeffSpace.lexmin().removeDims(T_SET, 0, addDims).samplePoint()
    println(schedPoint)
    System.exit(42)
  }

  private def optimizeExpl(scop : Scop, confID : Int) : Unit = {

    val df = new DecimalFormat()
    df.setMinimumIntegerDigits(5)
    df.setGroupingUsed(false)

    val explConfig = new File(Settings.poly_explorationConfig)
    if (!explConfig.exists() || explConfig.length() == 0) {
      Logger.debug("[PolyOpt] Exploration: no configuration file found or file empty, perform exploration and create it, progress:")
      performExploration(scop, explConfig, df)
      Logger.debug("[PolyOpt] Exploration: configuration finished, creating base version (without any schedule changes)")
      Settings.outputPath += df.format(0)
    } else {
      applyConfig(scop, explConfig, df.format(confID))
      Settings.outputPath += df.format(confID)
    }
  }

  private def performExploration(scop : Scop, explConfig : File, df : DecimalFormat) : Unit = {

    val domain = scop.domain.intersectParams(scop.getContext())
    var validity = scop.deps.validity()

    if (Knowledge.poly_simplifyDeps) {
      validity = validity.gistRange(domain)
      validity = validity.gistDomain(domain)
    }

    explConfig.getParentFile().mkdirs()
    val eConfOut = new java.io.PrintWriter(explConfig)
    eConfOut.println(domain)
    eConfOut.println(validity)
    eConfOut.println(Knowledge.poly_exploration_extended)
    eConfOut.println()
    var i : Int = 0
    Console.println("         1k          2k          3k          4k          5k")
    Exploration.guidedExploration(domain, validity, Knowledge.poly_exploration_extended, {
      (sched : isl.UnionMap, schedVect : Seq[Array[Int]], bands : Seq[Int]) =>
        i += 1
        if (i % 100 == 0) {
          Console.print('.')
          Console.flush()
        }
        if (i % 500 == 0) {
          Console.print(' ')
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
        eConfOut.print('\t')
        eConfOut.print(schedVect.map(arr => java.util.Arrays.toString(arr)).mkString(", "))
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
    val Array(_, bandsStr, scheduleStr, _) = configLine.split("\t")

    val bands : Array[Int] = bandsStr.split(",").map(str => Integer.parseInt(str))
    var schedule : isl.UnionMap = isl.UnionMap.readFromStr(scop.domain.getCtx(), scheduleStr)

    scop.noParDims.clear()

    // apply tiling
    val tilableDims : Int = bands(0)
    if (scop.optLevel >= 3 && tilableDims > 1 && tilableDims <= 4)
      schedule = tileSchedule(schedule, scop, tilableDims)

    scop.schedule = Isl.simplify(schedule)
    scop.updateLoopVars()
  }

  private final val tileSizes = Array(Knowledge.poly_tileSize_x, Knowledge.poly_tileSize_y, Knowledge.poly_tileSize_z, Knowledge.poly_tileSize_w)

  private def tileSchedule(schedule : isl.UnionMap, scop : Scop, tilableDims : Int) : isl.UnionMap = {
    val sample : isl.BasicMap = schedule.sample()
    val domSp : isl.Space = sample.getSpace().range()
    val ranSp : isl.Space = domSp.insertDims(T_SET, 0, tilableDims)
    val ctx : isl.Ctx = sample.getCtx()
    var mAff = isl.MultiAff.zero(isl.Space.mapFromDomainAndRange(domSp, ranSp))
    for (i <- 0 until tilableDims) {
      val tileSize = if (i != 0 || Knowledge.poly_tileOuterLoop) tileSizes(tilableDims - 1 - i) else 0
      // if we don't want to tile a dimension, leave the outer tile loop constant 0
      if (tileSize > 0 && (scop.origIterationCount == null || tileSize < 100 + scop.origIterationCount(tilableDims - 1 - i))) {
        var aff = isl.Aff.varOnDomain(isl.LocalSpace.fromSpace(domSp), T_SET, i)
        aff = aff.scaleDownUi(tileSize)
        aff = aff.floor()
        mAff = mAff.setAff(i, aff)
      }
    }
    for (i <- 0 until domSp.dim(T_SET)) {
      val aff = isl.Aff.varOnDomain(isl.LocalSpace.fromSpace(domSp), T_SET, i)
      mAff = mAff.setAff(tilableDims + i, aff)
    }
    val trafo = isl.BasicMap.fromMultiAff(mAff)
    setSeqTileDims(scop, tilableDims)
    return schedule.applyRange(trafo)
  }

  private def tileSchedule(schedule : isl.UnionMap, scop : Scop, tilableDims : Int, tile_sizes : List[Int]) : isl.UnionMap = {
    var node : isl.AstNode = new isl.AstNode()
    schedule.getSpace
    val space2 = isl_schedule_node_band_get_space(node)
    node = isl_schedule_node_parent(node)
    var space = isl_schedule_node_band_get_space(node)

    space = space.product(space2)
    val mv : isl.MultiVal = multiValFromIntList(space, tile_sizes)
    node = ppcg_ht_bounds_insert_tiling(bounds, mv, node)
    node = hybrid_tile_foreach_phase(node, &update_phase, gen)
    node = hybrid_tile_drop_phase_marks(node)
    return node;
  }

  private def multiValFromIntList(space : isl.Space, intList : List[Int]) = {
    val ctx = space.getCtx
    val n = space.dim(isl.DimType.Set)
    var mv = isl.MultiVal.zero(space)
    (0 until n).foreach(x => {
      val v = isl.Val.intFromSi(ctx, intList(x))
      mv = mv.setVal(x, v)
    })

    mv
  }

  var spamcount : Int = 0 // HACK to reduce the number of warnings generated

  private def setSeqTileDims(scop : Scop, nrTiledDims : Int) : Unit = {
    val threads = Knowledge.omp_numThreads
    for (i <- 0 until nrTiledDims) {
      val tiles : Long =
        if (tileSizes(i) <= 0)
          1
        else if (scop.origIterationCount != null)
          scop.origIterationCount(i) / tileSizes(i)
        else {
          spamcount += 1
          if (spamcount < 4)
            Logger.warn("[PolyOpt]  unable to determine iteration count, check results of LoopOverDimensions.maxIterationCount(); parallelization might be inefficient")
          else if (spamcount == 4)
            Logger.warn("[PolyOpt]  unable to determine iteration count; spam protection: suppress further warnings for this problem from now on")

          if (tileSizes(i) <= 0)
            1
          else // don't know how much iterations this loop has... so assume there are enough to parallelize it...
            1000
        }
      if (tiles != threads && tiles < 2 * threads)
        scop.noParDims += nrTiledDims - i - 1
    }
    if (nrTiledDims > 0 && !Knowledge.poly_tileOuterLoop)
      scop.noParDims += 0
  }

  private def recreateAndInsertAST() : Unit = {

    val replaceCallback = { (repl : Map[String, Expression], applyAt : Node) =>
      val oldLvl = Logger.getLevel
      Logger.setLevel(Logger.WARNING)
      this.execute(
        new Transformation("update loop iterator", {
          case VariableAccess(str, _) if (repl.isDefinedAt(str)) => Duplicate(repl(str))
          case StringLiteral(str) if (repl.isDefinedAt(str))     => Duplicate(repl(str))
        }), Some(applyAt))
      Logger.setLevel(oldLvl)
    }
    this.execute(new ASTBuilderTransformation(replaceCallback))
  }
}
