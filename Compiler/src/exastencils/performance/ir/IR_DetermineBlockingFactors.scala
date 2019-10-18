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


package exastencils.performance.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_Datatype
import exastencils.config._
import exastencils.performance.PlatformUtils

/// IR_DetermineBlockingFactors

object IR_DetermineBlockingFactors {
  // this approach can be improved in multiple ways:
  //   - try to match the number of tiles with the number of OpenMP threads
  //   - round blocking factors to multiples of the cache line size while upholding the balance between OpenMP threads
  //   - take cases yielding very small blocking factors into account - apply different blocking
  //   - block for different levels of the cache hierarchy

  def apply(fieldAccesses : HashMap[String, IR_Datatype], offsets : HashMap[String, ListBuffer[Long]], loopSize : Array[Long]) : Array[Long] = {
    val effCacheSize = Platform.hw_usableCache * PlatformUtils.cacheSizePerThread

    // determine required window size
    val reqMaxWindow = fieldAccesses.keys.map(ident => (offsets(ident).max - offsets(ident).min) * fieldAccesses(ident).typicalByteSize).max

    // determine available window size
    val numWindows = fieldAccesses.size
    val availWindow = effCacheSize / (numWindows * fieldAccesses.head._2.typicalByteSize)// assume identical data types

    // ratio determines blocking factors
    var ratio = availWindow.toDouble / reqMaxWindow.toDouble

    if (ratio >= 1.0) // all good, no need for blocking
      return loopSize.map(_ => 0L)

    var blockingFactors = loopSize.dropRight(1).map(_.toDouble) // don't block outer dimension

    val defCorrection = 2.0
    while (ratio < 1.0) {
      val largestDim = blockingFactors.zipWithIndex.maxBy(_._1)._2
      if (1.0 == blockingFactors(largestDim)) // blocking not possible
        return loopSize.map(_ => 0L)

      val correction = defCorrection // Math.min(defCorrection, 1.0 / ratio)
      blockingFactors(largestDim) = blockingFactors(largestDim) / correction
      ratio *= correction
    }

    // honor outer dimension
    blockingFactors :+= 0.0

    blockingFactors.map(_.ceil.toLong)
  }
}