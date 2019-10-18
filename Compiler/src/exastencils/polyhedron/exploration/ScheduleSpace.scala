//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.polyhedron.exploration

import exastencils.polyhedron.Isl.TypeAliases._

object ScheduleSpace {

  def compSchedConstrForDep(dep : isl.BasicMap, domInfo : DomainCoeffInfo, strongSatisfy : Boolean) : isl.BasicSet = {

    val ctx : isl.Ctx = dep.getCtx()
    val depSrcName = dep.getTupleName(T_IN)
    val depDstName = dep.getTupleName(T_OUT)
    val inEqOut : Boolean = depSrcName == depDstName

    val srcDim = dep.dim(T_IN)
    val dstDim = dep.dim(T_OUT)
    val parDim = dep.dim(T_PAR)
    val depPoly : isl.BasicSet = dep.moveDims(T_IN, srcDim, T_OUT, 0, dstDim).domain()
    var schedCoeffs : isl.BasicMap = depPoly.coefficients().unwrap()

    // if in and out are for the same statement:
    //   coefficients of source and dest must have the same value but a different sign
    //     "[ci0,ci1, di0,di1] -> [-ci0,-ci1] : ci0 = -di0 and ci1 = -di1"
    // else
    //   invert coefficients for source statement
    //     "[ci0,ci1, di0] -> [-ci0,-ci1, di0]"
    val njuDim : Int = if (inEqOut) srcDim else srcDim + dstDim
    var postprocIt = isl.Map.universe(isl.Space.alloc(ctx, 0, srcDim + dstDim, njuDim))
    for (i <- 0 until srcDim)
      postprocIt = postprocIt.oppose(T_IN, i, T_OUT, i)
    val njuOff : Int = if (inEqOut) 0 else srcDim
    for (i <- 0 until dstDim)
      postprocIt = postprocIt.equate(T_IN, srcDim + i, T_OUT, njuOff + i)
    schedCoeffs = schedCoeffs.applyRange(postprocIt.affineHull())

    // if in and out are for the same statement:
    //   coefficient for parameters must be 0, constant must be  if (strong) -1 else 0
    //     "[c_cst, c_p0,c_p1] -> [] : c_cst = {-1/0} and c_p0 = 0 and c_p1 = 0"
    // else
    //   split single coefficients for parameter and constants in two (one for each schedule)
    //     "[c_cst, c_p0,c_p1] -> [cs,cd, cp0s,cp1s,cp0d,cp1d] : c_cst = cd-cs{-1} and c_p0 = cp0d-cp0s and c_p1 = cp1d-cp1s"
    //       ==
    //     "[c_cst, c_p0,c_p1] -> [cs, c_cst+cs{+1}, cp0s, cp1s, c_p0+cp0s, c_p1+cp1s]"
    var postprocPar : isl.BasicMap = null
    if (inEqOut) {
      postprocPar = isl.BasicMap.universe(isl.Space.alloc(ctx, 0, parDim + 1, 0))

      val zeroVal = isl.Val.zero(ctx)
      val valC = if (strongSatisfy) isl.Val.negone(ctx) else zeroVal
      postprocPar = postprocPar.fixVal(T_IN, 0, valC)
      for (i <- 0 until parDim)
        postprocPar = postprocPar.fixVal(T_IN, i + 1, zeroVal)

    } else {
      // parameter dimensions are  [cs, cp0s,cp1s]
      var mAff = isl.MultiAff.zero(isl.Space.alloc(ctx, parDim + 1, parDim + 1, 2 * parDim + 2))
      val lSp = isl.LocalSpace.fromSpace(isl.Space.setAlloc(ctx, parDim + 1, parDim + 1))
      val oneVal = isl.Val.one(ctx)

      val affC1 = isl.Aff.varOnDomain(lSp, T_PAR, 0)
      var affC2 = isl.Aff.varOnDomain(lSp, T_SET, 0).add(affC1)
      if (strongSatisfy)
        affC2 = isl.Aff.valOnDomain(lSp, oneVal).add(affC2)

      mAff = mAff.setAff(0, affC1)
      mAff = mAff.setAff(1, affC2)
      for (i <- 0 until parDim) {
        val affP1 = isl.Aff.varOnDomain(lSp, T_PAR, i + 1)
        val affP2 = isl.Aff.varOnDomain(lSp, T_SET, i + 1).add(affP1)
        mAff = mAff.setAff(i + 2, affP1)
        mAff = mAff.setAff(i + 2 + parDim, affP2)
      }
      postprocPar = isl.BasicMap.fromMultiAff(mAff).projectOut(T_PAR, 0, parDim + 1)
    }
    schedCoeffs = schedCoeffs.applyDomain(postprocPar)

    // expand dimensionality to global schedule constraints
    val StmtCoeffInfo(srcItOff, srcNrIt, srcParOff, srcCstIdx) = domInfo.stmtInfo(depSrcName)
    val StmtCoeffInfo(dstItOff, dstNrIt, dstParOff, dstCstIdx) = domInfo.stmtInfo(depDstName)

    val nrPar : Int = domInfo.nrParPS
    var solSpMap = isl.BasicMap.fromDomainAndRange(
      schedCoeffs.reverse().wrap().flatten(),
      isl.BasicSet.universe(isl.Space.setAlloc(ctx, 0, domInfo.dim)))

    var off : Int = 0

    for (i <- 0 until srcNrIt)
      solSpMap = solSpMap.equate(T_IN, off + i, T_OUT, srcItOff + i)
    if (!inEqOut) {
      off += srcNrIt
      for (i <- 0 until dstNrIt)
        solSpMap = solSpMap.equate(T_IN, off + i, T_OUT, dstItOff + i)
      off += dstNrIt

      solSpMap = solSpMap.equate(T_IN, off, T_OUT, srcCstIdx)
      off += 1
      solSpMap = solSpMap.equate(T_IN, off, T_OUT, dstCstIdx)
      off += 1

      for (i <- 0 until nrPar)
        solSpMap = solSpMap.equate(T_IN, off + i, T_OUT, srcParOff + i)
      off += nrPar
      for (i <- 0 until nrPar)
        solSpMap = solSpMap.equate(T_IN, off + i, T_OUT, dstParOff + i)
      // off += nrPar // not needed anymore (comment is just for symmetry)
    }

    return solSpMap.range()
  }
}
