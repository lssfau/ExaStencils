package exastencils.polyhedron

import scala.collection.mutable
import scala.collection.mutable.{ ArrayBuffer, ListBuffer }
import scala.io.Source
import scala.util.control._

import java.text.DecimalFormat

import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.logger._
import exastencils.optimization.ir.IR_IV_LoopCarriedCSBuffer
import exastencils.polyhedron.Isl.TypeAliases._
import exastencils.polyhedron.exploration.Exploration
import isl.Conversions._

object IR_PolyOpt extends CustomStrategy("Polyhedral optimizations") {

  final val SCOP_ANNOT : String = "PolyScop"
  final val IMPL_CONDITION_ANNOT : String = "ImplCondition"

  import scala.language.implicitConversions

  implicit def convertIntToVal(i : Int) : isl.Val = isl.Val.intFromSi(Isl.ctx, i)

  /**
    * The command line argument for the scop ID to the exploration ID matching.
    * Possible structures:
    * - a single Int, used for every scop
    * - a list of scop ID to exploration ID mappings, key and values are spearated by a colon, list entries are separated by a pipe, e.g.: "3:42|5:1159"
    */
  var polyOptExplIDs : String = ""

  /** Register the name of a side-effect free function, that is safe to be used inside a scop. */
  def registerSideeffectFree(functionName : String) : Unit = {
    IR_PolyExtractor.registerSideeffectFree(functionName)
  }

  /** Register the name of a symbolic constant, that is not modified inside a scop. */
  def registerSymbolicConstant(constName : String) : Unit = {
    IR_PolyExtractor.registerSymbolicConstant(constName)
  }

  def apply() : Unit = {
    this.transaction()

    Logger.info("Applying strategy " + name)
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    // macro is sufficient here (for isl) since both arguments are either constants, variables, or contain at most a single add/sub
    Settings.additionalMacros += "#define floord(n,d) (((n)>=0) ? (n)/(d) : (((n)-(d)+1)/(d)))"

    Isl.ctx.optionsSetTileScaleTileLoops(0)
    Isl.ctx.optionsSetTileShiftPointLoops(0)

    Knowledge.poly_scheduleAlgorithm match {
      case "isl"       => Isl.ctx.optionsSetScheduleAlgorithm(0)
      case "feautrier" => Isl.ctx.optionsSetScheduleAlgorithm(1)
      case unknown     => Logger.debug("Unknown schedule algorithm \"" + unknown + "\"; no change (default is isl)")
    }

    Isl.ctx.optionsSetScheduleSeparateComponents(if (Knowledge.poly_separateComponents) 1 else 0)
    Isl.ctx.optionsSetScheduleSerializeSccs(if (Knowledge.poly_serializeSCCs) 1 else 0)
    Isl.ctx.optionsSetScheduleMaximizeBandDepth(if (Knowledge.poly_maximizeBandDepth) 1 else 0)
    Isl.ctx.optionsSetScheduleMaxConstantTerm(Knowledge.poly_maxConstantTerm)
    Isl.ctx.optionsSetScheduleMaxCoefficient(Knowledge.poly_maxCoefficient)

    // preprocess external schedules
    val extSchedules = mutable.Map[Int, isl.UnionMap]()
    for (str <- Knowledge.poly_externalSchedules) {
      val Array(id, sched) = str.split('|')
      extSchedules += ((id.toInt, isl.UnionMap.readFromStr(Isl.ctx, sched)))
    }

    val explIDMap = mutable.Map[Int, Int]()
    val explIDDef : Int = if (polyOptExplIDs.matches("[0-9]+")) polyOptExplIDs.toInt else 0
    for (id <- Knowledge.poly_explorationIDs)
      explIDMap += ((id, explIDDef))
    if (polyOptExplIDs.contains(':'))
      for (map <- polyOptExplIDs.split('|')) {
        val Array(scopID, explID) = map.split(':').map(_.toInt)
        if (explIDMap.contains(-1) || explIDMap.contains(scopID))
          explIDMap += ((scopID, explID))
      }

    def time[T](op : => T, name : String) : T = {
      if (Settings.timePolyOptSteps) {
        StrategyTimer.startTiming(name)
        val res = op
        StrategyTimer.stopTiming(name)
        res
      } else
        op
    }

    val scops : Seq[Scop] = time(extractPolyModel(), "po:extractPolyModel")
    var i : Int = 0
    for (scop <- scops if !scop.remove) {
      i += 1
      time(mergeLocalScalars(scop), "po:mergeLocalScalars")
      time(mergeScops(scop), "po:mergeScops")
      time(simplifyModel(scop), "po:simplifyModel")
      time(computeDependences(scop), "po:computeDependences")
      if (Knowledge.poly_performDCE)
        time(deadCodeElimination(scop), "po:deadCodeElimination")
      time(handleReduction(scop), "po:handleReduction")
      time(simplifyModel(scop), "po:simplifyModel")

      if (extSchedules.contains(scop.ID))
        useExternalSchedule(scop, extSchedules(scop.ID))
      else if (explIDMap.contains(scop.ID) || (explIDMap.contains(-1) && scop.optLevel >= 2))
        optimizeExpl(scop, explIDMap.getOrElse(scop.ID, explIDDef))
      else if (scop.optLevel >= 2)
        time(optimize(scop), "po:optimize")

      if (Knowledge.poly_printDebug) {
        Logger.debug("SCoP " + i)
        Logger.debug("  domain:   " + scop.domain)
        Logger.debug("  context:  " + scop.getContext())
        Logger.debug("  reads:    " + scop.reads)
        Logger.debug("  writes:   " + scop.writes)
        Logger.debug("  dependences")
        Logger.debug("    flow:   " + scop.deps.flow)
        Logger.debug("    valid:  " + scop.deps.validity())
        Logger.debug("    input:  " + scop.deps.input)
        Logger.debug("  schedule: " + scop.schedule)
      }
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
    val extr = new IR_PolyExtractor()
    this.register(extr)
    this.execute(new Transformation("extract model", PartialFunction.empty))
    this.unregister(extr)

    val scops : Seq[Scop] = extr.scops

    Logger.debug("    valid SCoPs: " + scops.size)
    Logger.debug("    rejected:    " + extr.trash.size)

    scops
  }

  private def mergeLocalScalars(scop : Scop) : Unit = {
    var toFind : String = null
    var found : Boolean = false
    val search = new Transformation("search...", {
      case va : IR_VariableAccess if va.name == toFind =>
        found = true
        va
      case sc : IR_StringLiteral if sc.value == toFind =>
        found = true
        sc
    })
    for (decl : IR_VariableDeclaration <- scop.decls) Breaks.breakable {
      val name : String = decl.name
      toFind = name
      var fstStmt : Int = -1
      var lstStmt : Int = -1
      var stmts : mutable.Buffer[(String, (ListBuffer[IR_Statement], ArrayBuffer[String]))] = scop.stmts.toBuffer.sortBy(_._1)
      Logger.pushLevel(Logger.WARNING)
      for (((_, (stmt, _)), i) <- stmts.zipWithIndex) {
        found = false
        this.execute(search, Some(IR_Scope(stmt)))
        if (found) {
          if (fstStmt < 0)
            fstStmt = i
          lstStmt = i
        }
      }
      Logger.popLevel()
      stmts = stmts.slice(fstStmt, lstStmt + 1)
      // check if all statements have the same iteration space
      val remDoms = new ArrayBuffer[isl.Set]()
      var njuDomain : isl.UnionSet = null
      scop.domain.foreachSet({
        set : isl.Set =>
          val tupName = set.getTupleName()
          if (stmts.exists { case (label, _) => tupName == label })
            remDoms += set
          else
            njuDomain = if (njuDomain == null) set else njuDomain.addSet(set)
      })
      val mergedDom : isl.Set = remDoms(0)
      val proto : isl.Set = mergedDom.resetTupleId()
      for (i <- 1 until remDoms.length)
        if (!proto.isEqual(remDoms(i).resetTupleId()))
          Breaks.break() // continue... different domains, cannot merge statements
      val mergedStmts = new ListBuffer[IR_Statement]()
      var mergedLoopIts : ArrayBuffer[String] = null
      for ((_, (stmt, loopIts)) <- stmts) {
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
            if (map.getTupleName(T_OUT) != name) {
              // remove all accesses to the scalar
              val oldLabel : String = map.getTupleName(T_IN)
              var toAdd : isl.Map = map
              if (oldLabel != njuLabel)
                for ((lab, _) <- stmts if oldLabel == lab)
                  toAdd = toAdd.setTupleName(T_IN, njuLabel)
              nju = if (nju == null) toAdd else nju.addMap(toAdd)
            }
        })
        nju
      }

      scop.reads = adjust(scop.reads)
      scop.writes = adjust(scop.writes)

      // update scop.deadAfterScop
      if (scop.deadAfterScop != null) {
        var resurrect : Boolean = false
        for (set <- remDoms)
          if (scop.deadAfterScop.intersect(set).isEmpty)
            resurrect = true
        if (resurrect)
          scop.deadAfterScop = scop.deadAfterScop.subtract(isl.Set.universe(remDoms(0).getSpace))
      }
    }
  }

  private def mergeScops(scop : Scop) : Unit = {

    var toMerge : Scop = scop.nextMerge
    var i : Int = 0
    scop.schedule = insertCst(scop.schedule, i)
    while (toMerge != null) {
      i += 1
      if (scop.root.parallelization != toMerge.root.parallelization) {
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
    var s = isl.UnionMap.empty(sched.getSpace)
    sched.foreachMap({
      map : isl.Map =>
        val nju = map.insertDims(T_OUT, 0, 1)
        s = s.addMap(nju.fixVal(T_OUT, 0, i))
    })
    s
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

    val empty = isl.UnionMap.empty(scop.writes.getSpace)
    val depArr = new Array[isl.UnionMap](1)
    val depArr2 = new Array[isl.UnionMap](1)
    val domain : isl.UnionSet = scop.domain.intersectParams(scop.getContext())

    val schedule = Isl.simplify(scop.schedule.intersectDomain(domain))

    val writes = scop.writes.intersectDomain(domain)
    val reads = if (scop.reads == null) empty else Isl.simplify(scop.reads.intersectDomain(domain))

    var writesToVec, writesNotVec : isl.UnionMap = empty
    var readsToVec, readsNotVec : isl.UnionMap = empty

    writes.foreachMap { map : isl.Map =>
      if (map.getTupleName(isl.DimType.Out).startsWith(IR_IV_LoopCarriedCSBuffer.commonPrefix))
        writesToVec = writesToVec.addMap(map)
      else
        writesNotVec = writesNotVec.addMap(map)
    }
    reads.foreachMap { map : isl.Map =>
      if (map.getTupleName(isl.DimType.Out).startsWith(IR_IV_LoopCarriedCSBuffer.commonPrefix))
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
      val noDeps = isl.UnionMap.empty(scop.deps.antiOutParVec.getSpace)
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
    live = Isl.simplify(live)

    val domWCtx = scop.domain.intersectParams(scop.getContext())
    val liveWCtx = live.intersectParams(scop.getContext())
    if (!domWCtx.isEqual(liveWCtx)) // the new one could be more complex, so keep old if possible
      scop.domain = live
  }

  private def handleReduction(scop : Scop) : Unit = {
    if (scop.root.parallelization.reduction.isEmpty)
      return

    val name : String = IR_PolyExtractor.replaceSpecial(scop.root.parallelization.reduction.get.target.prettyprint())
    val stmts = mutable.Set[String]()
    scop.writes.foreachMap({ map : isl.Map =>
      if (map.getTupleName(T_OUT) == name)
        stmts += map.getTupleName(T_IN)
    } : isl.Map => Unit)

    var toRemove = isl.UnionMap.empty(scop.deps.flowParVec.getSpace)
    scop.deps.flowParVec.foreachMap({ dep : isl.Map =>
      if (stmts.contains(dep.getTupleName(T_IN)))
        toRemove = toRemove.addMap(isl.Map.identity(dep.getSpace).complement())
    } : isl.Map => Unit)
    scop.deps.flowPar.foreachMap({ dep : isl.Map =>
      if (stmts.contains(dep.getTupleName(T_IN)))
        toRemove = toRemove.addMap(isl.Map.identity(dep.getSpace).complement())
    } : isl.Map => Unit)
    scop.deps.flowParVec = scop.deps.flowParVec.subtract(toRemove)
    scop.deps.flowPar = scop.deps.flowPar.subtract(toRemove)
    // filter others too, as we do not have an ordering between read and write in the same statement
    scop.deps.antiOutParVec = scop.deps.antiOutParVec.subtract(toRemove)
    scop.deps.antiOutPar = scop.deps.antiOutPar.subtract(toRemove)
    scop.deps.mapInputLazy { input => input.subtract(toRemove) }
  }

  private def optimize(scop : Scop) : Unit = {

    val domain = scop.domain.intersectParams(scop.getContext())
    var schedConstr : isl.ScheduleConstraints = isl.ScheduleConstraints.onDomain(domain)

    var validity = scop.deps.validity()
    var coincidence = validity
    var proximity = Knowledge.poly_optimizeDeps match {
      case "all" => validity
      case "raw" => scop.deps.flow
      case "rar" => scop.deps.input
      case _     =>
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

    // simple heuristics that leads to a better schedule for non-dense Jacobi smoothers
    if (Knowledge.poly_filterDeps)
      proximity = Isl.simplify(proximity.lexmin())

    schedConstr = schedConstr.setValidity(validity)
    schedConstr = schedConstr.setCoincidence(coincidence)
    schedConstr = schedConstr.setProximity(proximity)

    val schedule : isl.Schedule = schedConstr.computeSchedule()
    var scheduleMap : isl.UnionMap = schedule.getMap
    scop.noParDims.clear()
    if (scop.optLevel >= 3) {
      var tilableDims : Int = 0
      var prefix : Int = 10000 // nr of dimensions outside the band
      // search for first band (smallest prefix) with a width larger than one and a "constant prefix" (no loop in prefix/no surrounding loop)
      schedule.foreachBand({
        band : isl.Band =>
          var pref : Int = 0
          band.getPrefixSchedule().foreachMap({ map : isl.Map =>
            if (map.range().isSingleton())
              pref = math.max(pref, map.dim(T_OUT))
            else
              pref = prefix + 1 // will be ignored later
          })
          val bMem = band.nMember()
          if (bMem > 1) {
            if (pref == prefix)
              tilableDims = math.min(tilableDims, bMem)
            else if (pref < prefix) {
              tilableDims = bMem
              prefix = pref
            }
          }
      })
      if (tilableDims > 1 && tilableDims <= 4)
        scheduleMap = tileSchedule(scheduleMap, scop, tilableDims, scop.tileSizes, prefix)
    }

    scop.schedule = Isl.simplify(scheduleMap)
    scop.updateLoopVars()
  }

  private def useExternalSchedule(scop : Scop, umap : isl.UnionMap) : Unit = {
    if (Knowledge.poly_extSched_unrollTime) {
      if (umap.nMap() > 1)
        Logger.error("given schedule must be an isl.Map if Knowledge.poly_extSched_unrollTime is set")
      var map : isl.Map = null
      umap.foreachMap( m => map = m)
      scop.noParDims.clear()
      var schedule = isl.UnionMap.empty(isl.Space.mapFromSet(scop.domain.getSpace()))
      for ((stmt, i) <- scop.stmts.keySet.toArray.sorted.zipWithIndex) {
        val m = map.fixVal(T_IN, 0, i).projectOut(T_IN, 0, 1).setTupleName(T_IN, stmt)
        schedule = schedule.addMap(m)
      }
      scop.schedule = Isl.simplify(schedule)
    } else
      scop.schedule = Isl.simplify(umap)
    scop.noParDims.clear()

    val tilableDims : Int = Knowledge.poly_extSched_outerBandSize
    if (tilableDims > 1) {
      // apply tiling
      if (scop.optLevel >= 3 && tilableDims > 1 && tilableDims <= 4)
        scop.schedule = tileSchedule(scop.schedule, scop, tilableDims, scop.tileSizes)
    }

    scop.schedule = Isl.simplify(scop.schedule)
    scop.updateLoopVars()
  }

  private def optimizeExpl(scop : Scop, confID : Int) : Unit = {

    var validity = scop.deps.validity()
    if (validity.isEmpty()) // TODO: allow/implement exploration for scops without any data dependence?
      return

    val df = new DecimalFormat()
    df.setMinimumIntegerDigits(5)
    df.setGroupingUsed(false)

    val path = new StringBuilder(Settings.poly_explorationConfig)
    val ind = path.lastIndexOf('.')
    if (ind < 0)
      path.append('.').append(scop.ID)
    else
      path.insert(ind, "." + scop.ID)
    val explConfig = new java.io.File(path.toString())
    if (!explConfig.exists() || explConfig.length() == 0) {
      Logger.debug("[PolyOpt] Exploration: no configuration file found or file empty, perform exploration and create it")
      val domain = scop.domain.intersectParams(scop.getContext())

      if (Knowledge.poly_simplifyDeps) {
        validity = validity.gistRange(domain)
        validity = validity.gistDomain(domain)
      }

      explConfig.getAbsoluteFile().getParentFile().mkdirs()
      val eConfOut = new java.io.PrintWriter(explConfig)
      eConfOut.println(domain)
      eConfOut.println(validity)
      eConfOut.println(Knowledge.poly_exploration_extended)
      eConfOut.println()
      var i : Int = 0
      val timingName = "po:exploration"
      if (Settings.timePolyOptSteps)
        StrategyTimer.startTiming(timingName)
      Exploration.guidedExploration(domain, validity, Knowledge.poly_exploration_extended, Console.out, {
        (sched : isl.UnionMap, schedVect : Seq[Array[Int]], bands : Seq[Int], nrCarried : Seq[Int], cstVectable : Boolean) =>
          i += 1
          eConfOut.print(df.format(i))
          eConfOut.print('\t')
          eConfOut.print(bands.mkString(","))
          eConfOut.print('\t')
          eConfOut.print(sched)
          eConfOut.print('\t')
          eConfOut.print(schedVect.map(arr => java.util.Arrays.toString(arr)).mkString(", "))
          eConfOut.print('\t')
          eConfOut.print(nrCarried.mkString(","))
          eConfOut.print('\t')
          eConfOut.print(cstVectable)
          eConfOut.println()
      })
      if (Settings.timePolyOptSteps)
        StrategyTimer.stopTiming(timingName)
      eConfOut.flush()
      eConfOut.close()
      Logger.debug(s"[PolyOpt] Exploration finished: found $i configurations")
    }
    if (confID != 0)
      applyConfig(scop, explConfig, df.format(confID))
    if (Settings.poly_exploration_appendID2path)
      Settings.outputPath_suffix += df.format(confID)
  }

  private def applyConfig(scop : Scop, explConfig : java.io.File, confID : String) : Unit = {
    var lines : Iterator[String] = Source.fromFile(explConfig).getLines()
    lines = lines.dropWhile(l => !l.startsWith(confID))

    val configLine : String = lines.next()
    Logger.debug("[PolyOpt] Exploration: configuration found:")
    Logger.debug(" " + configLine)
    val Array(_, bandsStr, scheduleStr, _, _, _) = configLine.split("\t")

    val bands : Array[Int] = bandsStr.split(",").map(str => Integer.parseInt(str))
    var schedule : isl.UnionMap = isl.UnionMap.readFromStr(scop.domain.getCtx, scheduleStr)

    scop.noParDims.clear()

    // apply tiling
    val tilableDims : Int = bands(0)
    if (scop.optLevel >= 3 && tilableDims > 1 && tilableDims <= 4)
      schedule = tileSchedule(schedule, scop, tilableDims, scop.tileSizes)

    scop.schedule = Isl.simplify(schedule)
    scop.updateLoopVars()
  }

  private def tileSchedule(schedule : isl.UnionMap, scop : Scop, tilableDims : Int, tileSizes : Array[Int], prefix : Int = 0) : isl.UnionMap = {
    val sample : isl.BasicMap = schedule.sample()
    val domSp : isl.Space = sample.getSpace.range()
    val ranSp : isl.Space = domSp.insertDims(T_SET, 0, tilableDims)
    var mAff = isl.MultiAff.zero(isl.Space.mapFromDomainAndRange(domSp, ranSp))
    for (i <- 0 until tilableDims) {
      val tileSize = if (i != 0 || Knowledge.poly_tileOuterLoop) tileSizes(tilableDims - 1 - i) else 0
      // if we don't want to tile a dimension, leave the outer tile loop constant 0
      // scop.origIterationCount does not know anything about constant dims (prefix dims), so ignore prefix here
      if (tileSize > 0 && (scop.origIterationCount == null || tileSize < 100 + scop.origIterationCount(tilableDims - 1 - i))) {
        var aff = isl.Aff.varOnDomain(isl.LocalSpace.fromSpace(domSp), T_SET, i + prefix)
        aff = aff.scaleDownUi(tileSize)
        aff = aff.floor()
        mAff = mAff.setAff(i + prefix, aff)
      }
    }
    for (i <- 0 until domSp.dim(T_SET)) {
      val aff = isl.Aff.varOnDomain(isl.LocalSpace.fromSpace(domSp), T_SET, i)
      if (i < prefix)
        mAff = mAff.setAff(i, aff)
      else
        mAff = mAff.setAff(tilableDims + i, aff)
    }
    val trafo = isl.BasicMap.fromMultiAff(mAff)
    setSeqTileDims(scop, tilableDims, prefix)
    schedule.applyRange(trafo)
  }

  var spamcount : Int = 0 // HACK to reduce the number of warnings generated

  private def setSeqTileDims(scop : Scop, nrTiledDims : Int, prefix : Int) : Unit = {
    val threads = Knowledge.omp_numThreads
    for (i <- 0 until nrTiledDims) {
      val tileSize = scop.tileSizes(i)
      val tiles : Long =
        if (tileSize <= 0)
          1
        else if (scop.origIterationCount != null)
          // scop.origIterationCount doesn not know anything about constant dims (prefix dims), so ignore prefix here
          (scop.origIterationCount(i) + tileSize - 1) / tileSize // ceil division
        else {
          spamcount += 1
          if (spamcount < 4)
            Logger.warn("[PolyOpt]  unable to determine iteration count, check results of LoopOverDimensions.maxIterationCount(); parallelization might be inefficient")
          else if (spamcount == 4)
            Logger.warn("[PolyOpt]  unable to determine iteration count; spam protection: suppress further warnings for this problem from now on")

          if (tileSize <= 0)
            1
          else // don't know how much iterations this loop has... so assume there are enough to parallelize it...
            1000
        }
      if (tiles != threads && tiles < 2 * threads)
        scop.noParDims += nrTiledDims - i - 1 + prefix
    }
    if (nrTiledDims > 0 && !Knowledge.poly_tileOuterLoop)
      scop.noParDims += prefix
  }

  private def recreateAndInsertAST() : Unit = {
    this.execute(new IR_ASTBuilderTransformation())
  }
}
