package exastencils.polyhedron.exploration

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayStack

import java.io.PrintStream

import exastencils.config.Knowledge
import exastencils.polyhedron.Isl
import exastencils.polyhedron.Isl.TypeAliases._
import isl.Conversions._
import org.exastencils.schedopt.chernikova.Chernikova
import org.exastencils.schedopt.chernikova.Generators

class ExplorationError(msg : String) extends Error(msg)

object Exploration {

  import scala.language.implicitConversions

  implicit def islVal2int(v : isl.Val) : Int = {
    return v.getNumSi().toInt
  }

  import scala.util.control.Breaks

  private val filterInnerPar : Boolean = Knowledge.poly_exploration_filterLevel >= 1
  private val filterLinMemAc : Boolean = Knowledge.poly_exploration_filterLevel >= 2
  private val filterTextDeps : Boolean = Knowledge.poly_exploration_filterLevel >= 3
  private val filterAlignVec : Boolean = Knowledge.poly_exploration_filterLevel >= 4
  private val filterPosMemAc : Boolean = Knowledge.poly_exploration_filterLevel >= 5
  private val filterPosCoeff : Boolean = Knowledge.poly_exploration_filterLevel >= 6
  private val filterSmallCof : Boolean = Knowledge.poly_exploration_filterLevel >= 7

  def preprocess(domain : isl.UnionSet, deps : isl.UnionMap) : ArrayBuffer[isl.BasicMap] = {

    val res = new ArrayBuffer[isl.BasicMap]()
    val maxSplit : Int = 100

    deps.foreachMap { map : isl.Map =>
      val interm = new ArrayBuffer[isl.BasicMap]()
      var i : Int = 0
      var depsMap : isl.Map = map
      do {
        val dep : isl.Map = Isl.simplify(depsMap.lexmin())
        Breaks.breakable {
          dep.foreachBasicMap { bmap : isl.BasicMap =>
            interm += bmap
            i += 1
            if (i > maxSplit)
              Breaks.break()
          }
        }
        depsMap = depsMap.subtract(dep)
      } while (!depsMap.isEmpty() && i <= maxSplit)
      if (i <= maxSplit)
        res ++= interm
      else
        map.foreachBasicMap { bmap : isl.BasicMap =>
          res += bmap
        }
    }

    return res.view.filter {
      d : isl.BasicMap =>
        !d.intersectDomain(domain).intersectRange(domain).coalesce().isEmpty()
    }.map {
      d : isl.BasicMap =>
        if (d.dim(isl.DimType.Div) == 0)
          d
        else
          d.removeDivs()
    }.to[ArrayBuffer].sorted(new Ordering[isl.BasicMap]() {
      def compare(x : isl.BasicMap, y : isl.BasicMap) : Int = {
        val xMax : String = Ordering[String].max(x.getTupleName(isl.DimType.In), x.getTupleName(isl.DimType.Out))
        val yMax : String = Ordering[String].max(y.getTupleName(isl.DimType.In), y.getTupleName(isl.DimType.Out))
        return xMax.compareTo(yMax)
      }
    })
  }

  def computeConstDimPossibilities(nrStmts : Int, stmtGrps : List[List[Int]]) : ArrayBuffer[Array[Int]] = {
    var combinations = new ArrayBuffer[Array[Int]]() += new Array[Int](nrStmts)
    for (stmtGrp <- stmtGrps) {
      val combsMeta = combinationsMeta(stmtGrp.size)
      val njuCombs = new ArrayBuffer[Array[Int]]()
      for (comb <- combinations)
        for (combM <- combsMeta)
          perms(combM.toArray, stmtGrp, comb, njuCombs)
      combinations = njuCombs
    }
    return combinations
  }

  def combinationsMeta(n : Int) : ArrayBuffer[List[Int]] = {
    val res = ArrayBuffer(n :: Nil)
    for (i <- n - 1 until 0 by -1)
      res ++= combinationsMeta(n - i).transform(x => i :: x)
    return res
  }

  def perms(nrs : Array[Int], stmtGrp : List[Int], partRes : Array[Int], results : ArrayBuffer[Array[Int]]) : Unit = {
    if (stmtGrp.isEmpty) {
      results += java.util.Arrays.copyOf(partRes, partRes.length)
      return
    }
    for (i <- 0 until nrs.length)
      if (nrs(i) > 0) {
        nrs(i) -= 1
        partRes(stmtGrp.head) = i
        perms(nrs, stmtGrp.tail, partRes, results)
        nrs(i) += 1
      }
  }

  def guidedExploration(domain : isl.UnionSet, deps : isl.UnionMap, extended : Boolean, progressOStream : PrintStream,
      resultsCallback : (isl.UnionMap, Seq[Array[Int]], Seq[Int], Seq[Int], Boolean) => Unit) : Unit = {

    val domInfo = DomainCoeffInfo(domain)
    val depList : ArrayBuffer[isl.BasicMap] = preprocess(domain, deps)
    if (progressOStream != null) {
      // print progress to console and update it every 10 schedules found
      progressOStream.print("0")
      progressOStream.flush()
    }
    var i : Int = 0
    val previous = new mutable.HashSet[SchedVecWrapper]()
    var noTextDepsSchedules = new ArrayBuffer[(isl.UnionMap, Seq[Array[Int]], Seq[Int], Seq[Int], Boolean)]()
    completeScheduleGuided(new PartialSchedule(domInfo, depList), extended, {
      (sched : isl.UnionMap, schedVect : Seq[Array[Int]], bands : Seq[Int], nrCarried : Seq[Int], cstVect : Boolean) =>
        i += 1
        if (progressOStream != null && i % 10 == 0) {
          progressOStream.print("\r" + i)
          progressOStream.flush()
        }
        val remove : Boolean = (filterAlignVec && !cstVect) || // remove those which cannot be vectorized without aligned memory accesses
          (filterInnerPar && nrCarried.view.slice(1, bands(0)).exists(_ != 0)) // remove those whose inner loops in the outer band are not parallel
        val wrap = new SchedVecWrapper(schedVect)
        if (!previous.contains(wrap) && !remove) {
          previous += wrap
          if (filterTextDeps) {
            val hasTextDep : Boolean = nrCarried.last > 0
            if (noTextDepsSchedules != null) {
              if (hasTextDep)
                noTextDepsSchedules = null
              else
                noTextDepsSchedules += ((sched, schedVect, bands, nrCarried, cstVect))
            }
            if (noTextDepsSchedules == null && hasTextDep)
              resultsCallback(sched, schedVect, bands, nrCarried, cstVect)
          } else
            resultsCallback(sched, schedVect, bands, nrCarried, cstVect)
        }
    })
    if (noTextDepsSchedules != null)
      for (sched <- noTextDepsSchedules)
        resultsCallback.tupled(sched)
    if (progressOStream != null)
      progressOStream.print("\r")
  }

  def completeScheduleGuided(prefix : PartialSchedule, extended : Boolean,
      resultsCallback : (isl.UnionMap, Seq[Array[Int]], Seq[Int], Seq[Int], Boolean) => Unit) : Unit = {

    var coeffSpace : isl.Set = prefix.computeLinIndepSpace()
    if (coeffSpace == null)
      throw new Error("should not happen?!")

    // all dependences must be satisfied at least weakly
    for (dep <- prefix.remainingDeps) {
      val constr : isl.BasicSet = ScheduleSpace.compSchedConstrForDep(dep, prefix.domInfo, false)
      coeffSpace = coeffSpace.intersect(constr)
    }
    prefix.allowedVectors = coeffSpace

    val coeffSpacePoints = new ArrayBuffer[Array[Int]]()
    val gens : Set[Generators] = Chernikova.constraintsToGenerators(coeffSpace)
    val nrIt : Int = prefix.domInfo.nrIt
    for (g <- gens) {

      val vertices = new ArrayBuffer[Array[Int]](g.vertices.size)
      for ((vertV, den) <- g.vertices) {
        val vert : Array[Int] = Util.vectorToArray(vertV)
        if (den.intValue() != 1)
          throw new Error("Uups...")
        vertices += vert
      }

      val rays = new ArrayBuffer[Array[Int]](g.rays.size + 2 * g.lines.size)
      for (rayV <- g.rays) {
        val ray : Array[Int] = Util.vectorToArray(rayV)
        if (!Util.fstNZero(ray, nrIt))
          rays += ray
      }

      // "convert" a line into two rays
      for (lineV <- g.lines) {
        val line : Array[Int] = Util.vectorToArray(lineV)
        if (!Util.fstNZero(line, nrIt)) {
          rays += line
          rays += Util.negateArrayPW(line, line)
        }
      }

      if (vertices.length != 1)
        throw new Error("Not implemented yet...")

      for (vert <- vertices) {
        if (!Util.fstNZero(vert, nrIt))
          coeffSpacePoints += Util.reduceByGCD(vert, null)

        // check all combinations with rays
        for (ray <- rays) {
          val point : Array[Int] = Util.addArrayPW(vert, ray, null)
          if (!Util.fstNZero(point, nrIt))
            coeffSpacePoints += Util.reduceByGCD(point, point)
        }

        // also take a closer look at all pairs of rays(/lines)
        for (i <- 0 until rays.length) {
          val ray1 : Array[Int] = Util.addArrayPW(vert, rays(i), null)
          for (j <- i + 1 until rays.length) {
            val point : Array[Int] = Util.addArrayPW(ray1, rays(j), null)
            if (!Util.fstNZero(point, nrIt))
              coeffSpacePoints += Util.reduceByGCD(point, point)
          }
        }

        if (extended) {
          // ... and triples
          for (i <- 0 until rays.length) {
            val ray1 : Array[Int] = Util.addArrayPW(vert, rays(i), null)
            for (j <- i + 1 until rays.length) {
              val ray2 : Array[Int] = Util.addArrayPW(ray1, rays(j), null)
              for (k <- j + 1 until rays.length) {
                val point : Array[Int] = Util.addArrayPW(ray2, rays(k), null)
                if (!Util.fstNZero(point, nrIt))
                  coeffSpacePoints += Util.reduceByGCD(point, point)
              }
            }
          }
        }
      }
    }

    // ensure all constant parts are positive and the smallest constant is always 0
    val cstStart : Int = prefix.domInfo.dim - prefix.domInfo.nrStmts
    for (p <- coeffSpacePoints) {
      val minCst = p.view.drop(cstStart).min
      for (i <- cstStart until p.length)
        p(i) -= minCst
    }

    // filter out duplicates
    val toRemove = new mutable.BitSet(coeffSpacePoints.length)
    for (i <- 0 until coeffSpacePoints.length)
      for (j <- i + 1 until coeffSpacePoints.length)
        if (java.util.Arrays.equals(coeffSpacePoints(i), coeffSpacePoints(j)))
          toRemove.add(j)

    if (filterPosCoeff) {
      // remove those with negative coefficients
      for (i <- 0 until coeffSpacePoints.length)
        if (coeffSpacePoints(i).view.take(nrIt).exists(_ < 0))
          toRemove.add(i)
    } else if (filterPosMemAc) {
      // ensure positive linear memory access (linear already satisfied by other/previous filer)
      for (i <- 0 until coeffSpacePoints.length) {
        val vec : Array[Int] = coeffSpacePoints(i)
        if (prefix.domInfo.stmtInfo.values.exists(sInfo => vec(sInfo.itStart + sInfo.nrIt - 1) < 0))
          toRemove.add(i)
      }
    }
    if (filterSmallCof) {
      // remove all with a coefficient or the constant (offset) larger than 2
      for (i <- 0 until coeffSpacePoints.length) {
        val vec : Array[Int] = coeffSpacePoints(i)
        if (vec.view.take(nrIt).exists(_ > 2) || vec(cstStart+1) > 2) // vec(cstStart) is always 0 (see above)
          toRemove.add(i)
      }
    }

    val filteredCoeffSpacePoints = new ArrayBuffer[Array[Int]]()
    for (i <- 0 until coeffSpacePoints.length) {
      if (!toRemove.contains(i))
        filteredCoeffSpacePoints += coeffSpacePoints(i)
    }

    prefix.newBand()
    completeScheduleGuidedTilable(prefix, filteredCoeffSpacePoints, extended, resultsCallback)
  }

  def completeScheduleGuidedTilable(prefix : PartialSchedule, possibilities : ArrayBuffer[Array[Int]], extended : Boolean,
      resultsCallback : (isl.UnionMap, Seq[Array[Int]], Seq[Int], Seq[Int], Boolean) => Unit) : Unit = {

    val linDep : isl.Set = prefix.computeLinDepSpace()
    if (linDep != null && prefix.domInfo.universe.subtract(linDep).isEmpty()) {
      val prefixes = createVectorizable(prefix) // result contains prefix
      for (pref <- prefixes) {
        pref.newBand()
        addConstDim(pref, true) // to finalize schedule and ensure all deps are carried
        resultsCallback(pref.getSchedule(), pref.scheduleVectors, pref.bands, pref.carriedDeps.map(_.size), pref.cstVectable)
      }
      prefix.removeLastScheduleVector()
    } else {
      var foundExt : Boolean = false
      for (point <- possibilities)
        if (linDep == null || !Util.islSetContains(linDep, point)) {
          foundExt = true
          if (prefix.addScheduleVector(point, true) != 0)
            throw new Error("this should not happen!?")
          // filter for linear memory access; therefore: inner dimension of iteration domain has linear memory access
          var good : Boolean = true
          if (filterLinMemAc) {
            val nonCst = prefix.scheduleVectors.view.filter(sVec => sVec.view.take(prefix.domInfo.nrIt).exists(_ != 0)) // filter constant dimensions (no loop in corresponding AST)
            if (nonCst.size < Knowledge.dimensionality)
              good = prefix.domInfo.stmtInfo.values.forall(sInf => nonCst.last(sInf.itStart + sInf.nrIt - 1) == 0)
          }
          if (good)
            completeScheduleGuidedTilable(prefix, possibilities, extended, resultsCallback)
          prefix.removeLastScheduleVector()
        }

      if (!foundExt)
        completeScheduleGuided(prefix, extended, resultsCallback)
    }
  }

  def addConstDim(prefix : PartialSchedule, carryAll : Boolean) : Unit = {
    val n = prefix.domInfo.nrStmts
    // heuristics: first check if (0,...,0) or (0,1,...,n-1) is sufficient before creating and testing all possibilities
    val cstCoeffsList = new Array[Int](n) #:: Array.range(0, n) #:: computeConstDimPossibilities(n, prefix.getLastStmtGrps()).toStream
    for (cstCoeffs <- cstCoeffsList) {
      val res = prefix.addCstScheduleVector(cstCoeffs)
      // check which dependences are strongly satisfied now
      if (res == 0 && (!carryAll || prefix.remainingDeps.isEmpty))
        return
      prefix.removeLastScheduleVector()
    }
    throw new Error("No legal constant dimension found?! but...?! why...?!")
  }

  private def createVectorizable(prefix : PartialSchedule) : Seq[PartialSchedule] = {

    // basic idea of this algorithm: memory access mapping is an identity (ignoring the tuple name)
    //   new access (using the new schedule) is the reversed schedule mapping
    //   innermost access dimension is a linear combination of the schedule for which the outer loop indices disappear und and the inner index has coefficient of 1
    //   i.e., given all constraints from the schedule we need to find the coefficients for which the upper hold and we use these to compute the constant part of the array access
    //   this constant part must be 0, so we can compute modifications to the schedule to ensure this

    val prefixes = new ArrayBuffer[PartialSchedule]()
    prefixes += prefix

    // create array of indices to all but the innermost loop iterators to take care about later
    // return null, if memory is not traversed linearly -> vectorization impossible anyway
    val sVecs = prefix.scheduleVectors
    val indices = new Array[Int](prefix.domInfo.nrIt - prefix.domInfo.nrStmts) // should contain indices to all except the innermost iterator coefficients
    var i : Int = 0
    val lastVec = sVecs.last
    for ((_, StmtCoeffInfo(start, nr, _, _)) <- prefix.domInfo.stmtInfo) { // for each statement
      val lDimInd = start + nr - 1
      for (j <- start until lDimInd) { // indices for all iterator coefficients except last one
        indices(i) = j // save all indices (for direct access later)
        i += 1
      }
      for (j <- 0 until sVecs.length - 1) // all except last (which is lastVec)
        if (sVecs(j)(lDimInd) != 0)
          return prefixes // no linear memory access
      if (lastVec(lDimInd) == 0)
        return prefixes // no linear memory access
    }

    val ctx = prefix.domInfo.ctx
    val nrInp = sVecs.length
    val endExcl = lastVec.length
    val cstStart = endExcl - prefix.domInfo.nrStmts

    // offsetsMap should be a mapping from the coefficients of the schedule dimensions to the constants of the innermost memory access
    //   (padded with zeros in the beginning to match the width of a schedule vector)
    // constant value in access according to not restricted coefficients
    var mAff = isl.MultiAff.zero(isl.Space.alloc(ctx, 0, nrInp, lastVec.length))
    for (pos <- cstStart until endExcl) {
      var aff = isl.Aff.zeroOnDomain(isl.LocalSpace.fromSpace(isl.Space.setAlloc(ctx, 0, nrInp)))
      for (d <- 0 until nrInp)
        aff = aff.setCoefficientSi(T_IN, d, sVecs(d)(pos))
      mAff = mAff.setAff(pos, aff)
    }
    var offsetsMap : isl.Map = isl.BasicMap.fromMultiAff(mAff)

    // restrict coefficients in a way that they form an equation like a*k = ..., where k is the innermost loop iterator (of the original domain) and the rhs does not contain any other iterator of the schedule domain
    for (iVec <- indices) { // loop over all except the innermost iterator dimensions
      var aff = isl.Aff.zeroOnDomain(isl.LocalSpace.fromSpace(isl.Space.setAlloc(ctx, 0, nrInp)))
      for (d <- 0 until nrInp)
        aff = aff.setCoefficientSi(T_IN, d, sVecs(d)(iVec))
      offsetsMap = offsetsMap.intersectDomain(aff.zeroBasicSet())
    }
    // we search for the absolutely smallest values of the domain, so they must be bounded
    var domain = offsetsMap.domain()
    val old = offsetsMap
    for (i <- 0 until nrInp) {
      // bounds checks are only reliable for the polyhedral hull of a 1D set (for {[i,i]} both methods return true)
      val d = domain.projectOut(T_SET, 0, i).projectOut(T_SET, 1, nrInp - i - 1).polyhedralHull()
      if (!d.dimHasLowerBound(T_SET, 0) && !d.dimHasUpperBound(T_SET, 0)) {
        domain = domain.lowerBoundVal(T_SET, i, isl.Val.intFromSi(ctx, 0))
        offsetsMap = offsetsMap.intersectDomain(domain)
      }
    }
    // coefficient for innermost dim must not be 0, since it is the only one that contains the innermost loop iterator
    domain = isl.Set.universe(domain.getSpace())
    domain = domain.fixVal(T_SET, nrInp - 1, isl.Val.intFromSi(ctx, 0)).complement()
    offsetsMap = Isl.simplify(offsetsMap.intersectDomain(domain))
    val coeffsSet : isl.Set = offsetsMap.domain().lexmin()

    val coeffs : isl.Point = coeffsSet.samplePoint()
    val offsetPoint : isl.Point = coeffsSet.apply(offsetsMap).samplePoint()

    val offsetArray = new Array[Int](lastVec.length)
    for (i <- 0 until offsetArray.length)
      offsetArray(i) = offsetPoint.getCoordinateVal(T_SET, i)

    // if constants are already 0 we are done and everything is fine
    if (offsetArray.forall(_ == 0)) {
      prefix.cstVectable = true
      return prefixes // adding a zero vector is boring...
    }

    // find possible dims (coeff not 0)
    val offsetCoeffsBuffer = new ArrayBuffer[(Int, Int)]()
    for (i <- 0 until nrInp) {
      val c : Int = coeffs.getCoordinateVal(T_SET, i)
      if (c != 0)
        offsetCoeffsBuffer += ((c, i))
    }

    val prefixBnds : Int =
      if (prefix.bands.last == 0) // last band may be empty (precisely: it is empty)
        prefix.bands.size - 1
      else
        prefix.bands.size

    // build prefixes for modified schedule
    for (((coeff : Int, index : Int)) <- offsetCoeffsBuffer) Breaks.breakable {
      val newPrefix = new PartialSchedule(prefix.domInfo, prefix.allDeps)
      newPrefix.allowedVectors = prefix.allowedVectors
      newPrefix.newBand()
      i = 0
      while (i < sVecs.length) {
        var schedVec = sVecs(i)
        if (i == index) {
          val actualOffset = Util.divEArrayPW(offsetArray, -coeff, null)
          if (actualOffset == null)
            Breaks.break() // continue
          schedVec = Util.addArrayPW(schedVec, actualOffset, actualOffset)
        }
        newPrefix.addScheduleVector(schedVec) match {
          case 0  => // valid schedule
            i += 1
          case 1  => // already carried deps violated, add new band then retry (i is not incremented!)
            newPrefix.removeLastScheduleVector()
            newPrefix.newBand()
          case -1 => // schedule invalid
            Breaks.break() // continue
        }
      }
      newPrefix.cstVectable = true
      if (prefixBnds >= newPrefix.bands.size) // last band of newPrefix is not empty (by construction)
        prefixes += newPrefix // do not add versions with smaller bands
    }
    return prefixes
  }
}

object PartialSchedule {

  def expandCstCoeffVector(cstCoeffs : Array[Int], domInfo : DomainCoeffInfo) : Array[Int] = {
    if (cstCoeffs.length != domInfo.nrStmts)
      throw new Error("Ups... something happend...")
    val coeffs = new Array[Int](domInfo.dim)
    System.arraycopy(cstCoeffs, 0, coeffs, coeffs.length - cstCoeffs.length, cstCoeffs.length)
    return coeffs
  }

  def createFromCoefficients(domInfo : DomainCoeffInfo, coeffs : Array[Int]*) : isl.UnionMap = {
    if (coeffs.isEmpty)
      return null

    val ctx : isl.Ctx = domInfo.ctx
    var schedule : isl.UnionMap = isl.UnionMap.empty(domInfo.scheduleParamSpace)
    implicit val int2Val = (i : Int) => isl.Val.intFromSi(ctx, i)

    for ((stmt, sInfo) <- domInfo.stmtInfo) {
      val setSpace : isl.Space = domInfo.scheduleParamSpace.addDims(T_SET, sInfo.nrIt)
      val mapSpace : isl.Space = domInfo.scheduleParamSpace.addDims(T_IN, sInfo.nrIt).addDims(T_OUT, coeffs.size)
      var mAff = isl.MultiAff.zero(mapSpace)
      val lspace = isl.LocalSpace.fromSpace(setSpace)
      for ((coeff, dim) <- coeffs.view.zipWithIndex) {
        var aff = isl.Aff.zeroOnDomain(lspace)
        for (i <- 0 until sInfo.nrIt)
          aff = aff.setCoefficientVal(isl.DimType.In, i, coeff(sInfo.itStart + i))
        for (i <- 0 until domInfo.nrParPS)
          aff = aff.setCoefficientVal(isl.DimType.Param, i, coeff(sInfo.parStart + i))
        aff = aff.setConstantVal(coeff(sInfo.cstIdx))
        mAff = mAff.setAff(dim, aff)
      }
      val stmtSchedule = isl.Map.fromMultiAff(mAff).setTupleName(isl.DimType.In, stmt)
      schedule = if (schedule == null) stmtSchedule else schedule.union(stmtSchedule)
    }

    return schedule
  }

  private def computeLinDepSpace(domInfo : DomainCoeffInfo, vectors : Iterable[Array[Int]], start : Int, nr : Int) : isl.BasicSet = {
    if (vectors.isEmpty)
      return null

    val ctx = domInfo.ctx
    val nrPoints : Int = vectors.size
    val coeffsWithIndex : Iterable[(Array[Int], Int)] = vectors.view.zipWithIndex
    implicit val int2Val = (i : Int) => isl.Val.intFromSi(ctx, i)

    var mAff = isl.MultiAff.zero(isl.Space.alloc(ctx, 0, nrPoints, nr))
    for (pos <- 0 until nr) {
      var aff = isl.Aff.zeroOnDomain(isl.LocalSpace.fromSpace(isl.Space.setAlloc(ctx, 0, nrPoints)))
      for ((cs, i) <- coeffsWithIndex)
        aff = aff.setCoefficientVal(T_IN, i, cs(start + pos))
      mAff = mAff.setAff(pos, aff)
    }
    var linDepSpace : isl.BasicSet = isl.BasicMap.fromMultiAff(mAff).range()
    // remove existentially qualified variables in linDepSace (with removeDivs)
    //   because there could be holes in the space... [1 0 | 0 2] does not span the full 2D space
    linDepSpace = linDepSpace.removeDivs()

    // expand to full dimensionality
    return linDepSpace.insertDims(T_SET, 0, start).addDims(T_SET, domInfo.dim - (start + nr))
  }

  def computeLinIndepSpace(domInfo : DomainCoeffInfo, vectors : Iterable[Array[Int]], start : Int, nr : Int) : isl.Set = {
    if (vectors.isEmpty)
      return null
    val linIndep : isl.Set = computeLinDepSpace(domInfo, vectors, start, nr).complement()
    if (linIndep.isEmpty())
      return null
    else
      return linIndep
  }

  private var pos : isl.Set = null
  private var zer : isl.Set = null
  private var neg : isl.Set = null

  def computeDirection(schedule1D : isl.UnionMap, dep : isl.BasicMap) : CmpMultiResult = {
    val newDep : isl.UnionMap = dep.applyDomain(schedule1D).applyRange(schedule1D)
    if (newDep.nMap() != 1)
      throw new Error("ERROR?!")
    var sp = schedule1D.getSpace() // param space (since lastSched is a UnionMap)
    sp = sp.addDims(T_IN, 1).addDims(T_OUT, 1)
    val delta : isl.Set = newDep.extractMap(sp).deltas()

    if (zer == null) {
      val ctx = dep.getCtx()
      val univ = isl.Set.universe(delta.getSpace())
      zer = univ.fixVal(T_SET, 0, isl.Val.zero(ctx))
      pos = univ.lowerBoundVal(T_SET, 0, isl.Val.one(ctx))
      neg = univ.upperBoundVal(T_SET, 0, isl.Val.negone(ctx))
    }

    val isNeg = !delta.intersect(neg).isEmpty()
    val isZer = !delta.intersect(zer).isEmpty()
    val isPos = !delta.intersect(pos).isEmpty()

    return CmpMultiResult(isNeg, isZer, isPos)
  }

  def computeDirection(schedule1D : Array[Int], dep : isl.BasicMap, domInfo : DomainCoeffInfo) : CmpMultiResult = {
    return computeDirection(createFromCoefficients(domInfo, schedule1D), dep)
  }
}

case class CmpMultiResult(val isNegative : Boolean, val isZero : Boolean, val isPositive : Boolean) {
  val isNegativeOnly : Boolean = isNegative && !isZero && !isPositive
  val isZeroOnly : Boolean = !isNegative && isZero && !isPositive
  val isPositiveOnly : Boolean = !isNegative && !isZero && isPositive
}

class PartialSchedule(val domInfo : DomainCoeffInfo, val allDeps : ArrayBuffer[isl.BasicMap]) {

  val scheduleVectors = new ArrayBuffer[Array[Int]]()
  var remainingDeps = ArrayBuffer(allDeps : _*)
  val carriedDeps = new ArrayBuffer[ArrayBuffer[isl.BasicMap]]()
  var allowedVectors : isl.Set = null
  var cstVectable : Boolean = false
  private var preserveCstVectable : Boolean = false
  private var lastDimSchedule : isl.UnionMap = null

  private var addedStmtGrps : Boolean = false
  private val stmtGrpsStack : ArrayStack[List[List[Int]]] = ArrayStack(List((0 until domInfo.nrStmts).toList))

  val bands = new ArrayBuffer[Int]()
  newBand()

  /**
    * @return 0: schedule is valid, -1: uncarried dependence violated, 1: carried dependence (from current band) violated
    */
  def addCstScheduleVector(cstCoeffs : Array[Int], noCheck : Boolean = false) : Int = {
    if (preserveCstVectable)
      throw new Error("two subsequent constant dimensions are not allowed at the moment!")
    preserveCstVectable = true // a const dimension does not effect vectorization
    val coeffs = PartialSchedule.expandCstCoeffVector(cstCoeffs, domInfo)
    val ok = this.addScheduleVector(coeffs, noCheck)

    //    val cstCoeffsList = cstCoeffs.view.zipWithIndex.toList
    //    val stmtGrpIds = (0 until cstCoeffs.length).view // use view to prevent construction of intermediate collections
    //    val stmtGrps = stmtGrpIds.map(i => cstCoeffsList.view.filter(_._1 == i).map(_._2).toList).filter(!_.isEmpty)
    val stmtGrps = Array.fill(cstCoeffs.length)(Nil : List[Int])
    for (i <- cstCoeffs.length - 1 to 0 by -1)
      stmtGrps(cstCoeffs(i)) ::= i
    stmtGrpsStack.push(stmtGrps.view.filter(!_.isEmpty).to)
    addedStmtGrps = true

    return ok
  }

  /**
    * @return 0: schedule is valid, -1: uncarried dependence violated, 1: carried dependence (from current band) violated
    */
  def addScheduleVector(coeffs : Array[Int], noCheck : Boolean = false) : Int = {
    bands(bands.length - 1) += 1
    scheduleVectors += coeffs
    lastDimSchedule = null
    val carried = this.filterCarriedDeps(remainingDeps)
    carriedDeps += carried
    if (carried == null)
      return -1
    remainingDeps = remainingDeps.filterNot(carried.contains)
    if (!noCheck)
      for (i <- 1 until bands.last)
        if (this.filterCarriedDeps(carriedDeps(carriedDeps.length - i - 1)) == null)
          return 1
    return 0
  }

  def newBand() : Unit = {
    if (bands.isEmpty || bands.last > 0)
      bands += 0
  }

  def removeLastScheduleVector() : Unit = {
    var bndLst = bands.length - 1
    if (bands(bndLst) == 0) {
      bands.remove(bndLst)
      bndLst -= 1
    }
    bands(bndLst) -= 1
    scheduleVectors.remove(scheduleVectors.length - 1)
    remainingDeps ++= carriedDeps.remove(carriedDeps.length - 1)
    lastDimSchedule = null
    if (addedStmtGrps) {
      stmtGrpsStack.pop()
      addedStmtGrps = false
    }
    if (preserveCstVectable)
      preserveCstVectable = false
    else
      cstVectable = false
  }

  def getSchedule() : isl.UnionMap = {
    return PartialSchedule.createFromCoefficients(domInfo, scheduleVectors : _*)
  }

  def getLastStmtGrps() : List[List[Int]] = {
    return stmtGrpsStack.top
  }

  def scheduleVectorsString() : String = {
    if (scheduleVectors.isEmpty)
      return "List()"
    val sb = new StringBuilder()
    sb ++= "List("
    for (v <- scheduleVectors)
      sb ++= java.util.Arrays.toString(v) ++= ", "
    sb.delete(sb.length - 2, sb.length)
    sb += ')'
    return sb.toString()
  }

  def lastDimDirection(dep : isl.BasicMap) : CmpMultiResult = {
    if (lastDimSchedule == null)
      lastDimSchedule = PartialSchedule.createFromCoefficients(domInfo, scheduleVectors.last)
    return PartialSchedule.computeDirection(lastDimSchedule, dep)
  }

  def filterRemainingDeps(deps : ArrayBuffer[isl.BasicMap]) : ArrayBuffer[isl.BasicMap] = {
    val remainingDeps = new ArrayBuffer[isl.BasicMap](deps.length)
    var valid : Boolean = true
    for (dep <- deps) {
      val cmpRes : CmpMultiResult = this.lastDimDirection(dep)
      if (cmpRes.isNegative)
        valid = false
      if (!cmpRes.isPositiveOnly)
        remainingDeps += dep
    }
    if (valid)
      return remainingDeps
    else
      return null
  }

  def filterCarriedDeps(deps : ArrayBuffer[isl.BasicMap]) : ArrayBuffer[isl.BasicMap] = {
    val carriedDeps = new ArrayBuffer[isl.BasicMap]()
    var valid : Boolean = true
    for (dep <- deps) {
      val cmpRes : CmpMultiResult = this.lastDimDirection(dep)
      if (cmpRes.isPositiveOnly)
        carriedDeps += dep
      else if (cmpRes.isNegative)
        valid = false
    }
    if (valid)
      return carriedDeps
    else
      return null
  }

  def computeLinIndepSpace() : isl.Set = {
    var result : isl.Set = domInfo.universe
    if (scheduleVectors.isEmpty)
      return result

    val zeroVal = isl.Val.zero(domInfo.ctx)
    val oneVal = isl.Val.one(domInfo.ctx)
    var allZero : Boolean = true

    for ((_, StmtCoeffInfo(itStart, nrIt, parStart, cstIdx)) <- domInfo.stmtInfo) {
      // linear independence for iterator coeffs only is required (params and consts are irrelevant)
      val linIndepSpace : isl.Set = PartialSchedule.computeLinIndepSpace(domInfo, scheduleVectors, itStart, nrIt)
      if (linIndepSpace == null) {
        // dimension for this statement irrelevant... no exploration required, set anything
        result = result.fixVal(T_SET, itStart, oneVal) // 0-solution may not be allowed in universe
        for (i <- itStart + 1 until itStart + nrIt)
          result = result.fixVal(T_SET, i, zeroVal)
        for (i <- parStart until parStart + domInfo.nrParPS)
          result = result.fixVal(T_SET, i, zeroVal)
        result = result.fixVal(T_SET, cstIdx, zeroVal)
      } else {
        allZero = false
        result = result.intersect(linIndepSpace)
      }
    }

    return if (allZero) null else Isl.simplify(result)
  }

  def computeLinDepSpace() : isl.Set = {
    if (scheduleVectors.isEmpty)
      return null

    var result : isl.Set = domInfo.universe
    for ((_, StmtCoeffInfo(itStart, nrIt, _, _)) <- domInfo.stmtInfo) {
      // linear dependence for iterator coeffs only is required (params and consts are irrelevant)
      val linDepSpace : isl.Set = PartialSchedule.computeLinDepSpace(domInfo, scheduleVectors, itStart, nrIt)
      result = result.intersect(linDepSpace)
    }

    return Isl.simplify(result)
  }
}

private class SchedVecWrapper(scheduleVectors : Seq[Array[Int]]) {
  val schedVects : Seq[Array[Int]] = scheduleVectors.map(_.clone())

  override def equals(that : scala.Any) : Boolean = {
    if (that == null || !that.isInstanceOf[SchedVecWrapper])
      return false
    val thatSchedVecs = that.asInstanceOf[SchedVecWrapper].schedVects
    if (this.schedVects == thatSchedVecs)
      return true
    if (this.schedVects.length != thatSchedVecs.length)
      return false
    return this.schedVects.view.zip(thatSchedVecs).forall {
      schedVecs : (Array[Int], Array[Int]) =>
        java.util.Arrays.equals(schedVecs._1, schedVecs._2)
    }
  }

  override def hashCode() : Int = {
    var hash : Int = 1
    for (vec <- schedVects)
      hash = 31 * hash + java.util.Arrays.hashCode(vec)
    return hash
  }
}
